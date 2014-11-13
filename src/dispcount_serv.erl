%% In charge of relaying info about the supervisor when called.
-module(dispcount_serv).
-behaviour(gen_server).
-include("state.hrl").

-export([start_link/4, wait_for_dispatch/2, get_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
-spec start_link(Parent::pid(), Name::atom(), {module(),[term()]}, [term(),...]) -> {ok, pid()}.
start_link(Parent, Name, {M,A}, Opts) ->
    gen_server:start_link(?MODULE, {Parent, Name, {M,A}, Opts}, []).

-spec wait_for_dispatch(Name::atom(), infinity | pos_integer()) -> ok.
wait_for_dispatch(Name, Timeout) ->
    gen_server:call(get_name(Name), wait_for_tables, Timeout).

-spec get_info(Name::atom()) -> {ok, #config{}}.
get_info(Name) ->
    gen_server:call(get_name(Name), get_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Parent, Name, {M,A}, Opts}) ->
    %% This one needs to go fast because we're gonna mess up the synchronous
    %% starts of servers for the sake of the pool. For this reason, we'll
    %% temporarily use this process to receive all requests and just forward
    %% them when the time has come, maybe.
    ConfTmp = init_tables(Opts),
    Conf = ConfTmp#config{dispatch_name=Name, num_watchers=proplists:get_value(resources,Opts,10)},
    SupSpec =
    {{simple_one_for_one, proplists:get_value(maxr, Opts, 1), proplists:get_value(maxt, Opts, 60)},
      [{watchers,
        {dispcount_watcher, start_link, [Conf,{M,A}]},
        proplists:get_value(restart,Opts,permanent),
        proplists:get_value(shutdown,Opts,5000),
        worker,
        [dispcount_watcher,M]}]}, % <- check to make sure this can survive relups
    ChildSpec = {watchers_sup, {watchers_sup, start_link, [SupSpec]},
                 permanent, infinity, supervisor, [watchers_sup]},
    self() ! continue_init,
    register(get_name(Name), self()),
    {ok, {Parent, ChildSpec, Conf}}.

handle_call(get_info, _From, S = #config{}) ->
    {reply, {ok, S}, S};
handle_call(wait_for_tables, _From, S = #config{num_watchers=N, dispatch_table=Tid}) ->
    %% there should be N + 1 entries in the dispatch table
    case ets:info(Tid, size) of
        X when X =:= N+1 ->
            {reply, ok, S};
        _ ->
            timer:sleep(1),
            handle_call(wait_for_tables, _From, S)
    end;
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(continue_init, {Parent, ChildSpec, Conf}) ->
    {ok, Sup} = supervisor:start_child(Parent, ChildSpec),
    ok = start_watchers(Sup, Conf),
    {noreply, Conf};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE & HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
init_tables(Opts) ->
    case proplists:get_value(watcher_type, Opts, ets) of
        ets -> %% here
            Dispatch = ets:new(dispatch_table, [set, public, {write_concurrency,true}]),
            Worker = ets:new(worker_table, [set, public, {read_concurrency,true}]),
            true = ets:insert(Dispatch, {ct,0}),
            #config{watcher_type = ets,
                    dispatch_table = Dispatch,
                    worker_table = Worker};
        named -> %% here
            Dispatch = ets:new(dispatch_table, [set, public, {write_concurrency,true}]),
            true = ets:insert(Dispatch, {ct,0}),
            #config{watcher_type = named,
                    dispatch_table = Dispatch,
                    worker_table = undefined};
        Other ->
            erlang:error({bad_option,{watcher_type,Other}})
    end.

start_watchers(Sup, #config{num_watchers=Num}) ->
    [start_watcher(Sup, Id) || Id <- lists:seq(1,Num)],
    ok.

start_watcher(Sup, Id) ->
    {ok, _} = supervisor:start_child(Sup, [Id]).

get_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_serv").
