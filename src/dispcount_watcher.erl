-module(dispcount_watcher).
-behaviour(gen_server).
-include("state.hrl").

-record(state, {callback :: module(),
                callback_state :: term(),
                config :: #config{},
                id :: pos_integer(),
                ref :: reference() | undefined}).

-export([start_link/3, checkout/1, checkin/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUBLIC INTERFACE %%%
%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(#config{}, {module(), term()}, pos_integer()) -> {ok, pid()} | {error, _} | ignore.
start_link(Conf, Callback={_,_}, Id) ->
    gen_server:start_link(?MODULE, {Id, Conf, Callback}, []).

-spec checkout(#config{}) -> {ok, Ref::term(), Resource::term()} | {error, Reason::term()}.
checkout(Conf) ->
    checkout(self(), Conf).

-spec checkout(pid(), #config{}) -> {ok, Ref::term(), Resource::term()} | {error, Reason::term()}.
checkout(ToPid,#config{num_watchers=Num, watcher_type=Type, dispatch_table=DTid, worker_table=WTid}) ->
     case {Type, is_free(DTid, Id = dispatch_id(Num))} of
        {ets, true} ->
            [{_,Pid}] = ets:lookup(WTid, Id),
            gen_server:call(Pid, {get,ToPid});
        {named, true} ->
            gen_server:call(get_name(Id), {get,ToPid});
        {_, false} ->
            {error, busy}
    end.

-spec checkin(#config{}, Ref::term(), Resource::term()) -> ok.
checkin(#config{}, {Pid,Ref}, Resource) ->
    %% we cheated, using a Pid for the CheckRef. Dirty optimisation!
    gen_server:cast(Pid, {put, Ref, Resource}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Id,C=#config{watcher_type=ets,dispatch_table=DTid,worker_table=WTid},{M,A}}) ->
    ets:insert(WTid, {Id, self()}),
    ets:insert(DTid, {Id, 0}),
    init(Id,C,M,A);
init({Id,C=#config{watcher_type=named,dispatch_table=Tid},{M,A}}) ->
    register(get_name(Id), self()),
    ets:insert(Tid, {Id, 0}),
    init(Id,C,M,A).

handle_call({get, Pid}, _From, S=#state{callback=M, callback_state=CS, ref=undefined}) ->
    try M:checkout(Pid, CS) of
        {ok, Res, NewCS} ->
            MonRef = erlang:monitor(process, Pid),
            {reply, {ok, {self(),MonRef}, Res}, S#state{callback_state=NewCS, ref=MonRef}};
        {error, Reason, NewCS} ->
            {reply, {error, Reason}, S#state{callback_state=NewCS}};
        {stop, Reason, NewCS} ->
            M:terminate(Reason, NewCS),
            {stop, Reason, S}
    catch
        Type:Reason ->
            {stop, {Type,Reason}, S}
    end;
handle_call({get, _Pid}, _From, State) -> % busy
    {reply, {error, busy}, State};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({put, Ref, Res},
            S=#state{callback=M, callback_state=CS, config=Conf, id=Id, ref=Ref}) ->
    try M:checkin(Res, CS) of
        {ok, NewCS} ->
            #config{dispatch_table=DTid} = Conf,
            erlang:demonitor(Ref, [flush]),
            set_free(DTid, Id),
            {noreply, S#state{ref=undefined,callback_state=NewCS}};
        {ignore, NewCS} ->
            {noreply, S#state{callback_state=NewCS}};
        {stop, Reason, NewCS} ->
            M:terminate(Reason, NewCS),
            {stop, Reason, S}
    catch
        Type:Reason ->
            {stop, {Type,Reason}, S}
    end;
handle_cast({put, _Ref, _Res}, State) -> % nomatch on refs
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason},
            S=#state{ref=Ref, callback=M, callback_state=CS, config=Conf, id=Id}) ->
    try M:dead(CS) of
        {ok, NewCS} ->
            #config{dispatch_table=DTid} = Conf,
            set_free(DTid, Id),
            {noreply, S#state{ref=undefined,callback_state=NewCS}};
        {stop, Reason, NewCS} ->
            M:terminate(Reason, NewCS),
            {stop, Reason, S}
    catch
        Type:Reason ->
            {stop, {Type,Reason}, S}
    end;
handle_info(Msg, S=#state{callback=M, callback_state=CS}) ->
    try M:handle_info(Msg, CS) of
        {ok, NewCS} ->
            {noreply, S#state{callback_state=NewCS}};
        {stop, Reason, NewCS} ->
            M:terminate(Reason, NewCS),
            {stop, Reason, S}
    catch
        Type:Reason ->
            {stop, {Type,Reason}, S}
    end.

%% How do we handle things for the callback module??
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPERS/PRIVATE %%%
%%%%%%%%%%%%%%%%%%%%%%%
get_name(Id) ->
    list_to_atom("#"++atom_to_list(?MODULE)++integer_to_list(Id)).

init(Id,Conf,M,A) ->
    case M:init(A) of
        {ok, S} ->
            {ok, #state{callback=M,callback_state=S,config=Conf,id=Id}};
        X -> X
    end.

dispatch_id(Num) ->
    erlang:phash2({now(),self()}, Num) + 1.

is_free(Tid, Id) ->
    %% We optionally keep a tiny message queue in there,
    %% which should cause no overhead but be fine to deal
    %% with short spikes.
    case ets:update_counter(Tid, Id, {2,1}) of
        1 -> true;
        _ -> false
    end.

set_free(Tid, Id) ->
    ets:insert(Tid, {Id,0}).
