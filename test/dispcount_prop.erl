%%% Basic sequential statem based tests for dispcount.
-module(dispcount_prop).
-include_lib("proper/include/proper.hrl").
-behaviour(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).
-export([checkin/2]).

-define(SERVER, dispcount).
-define(NAME, prop_dispatch).
-define(INFO, {call, erlang, element, [2,{call, dispcount, dispatcher_info, [?NAME]}]}).

-record(state, {resource=[]}).

initial_state() ->
    #state{}.

%% Checking in and checking out is always valid
command(#state{resource=R}) ->
    oneof([{call, ?SERVER, checkout, [?INFO]}] ++
          [{call, ?MODULE, checkin, [?INFO, hd(R)]} || R =/= []]).

next_state(S=#state{resource=L}, V, {call, _, checkout, _}) ->
    S#state{resource=[V|L]};
next_state(S=#state{resource=[_|L]}, _V, {call, _, checkin, _}) ->
    S#state{resource=L}.

%% These preconditions might need a little rework. They were added
%% when the tests only worked with one resource, but since then, the
%% rules about empty lists no longer holds with 10 resources. The
%% tests still pass, but this should be cleaned up.
precondition(#state{resource=R}, {call,_,checkin,_}) when R =/= [] -> true;
precondition(#state{resource=R}, {call,_,checkout,_}) when R =/= [] -> true;
precondition(#state{resource=[]}, {call,_,checkout,_}) -> true;
precondition(A, B) -> io:format("PRE: ~p~n",[{A,B}]),  false.

%% The postconditions are a little bit more complex.
%% The following rules are for resources that we managed to check out.
postcondition(#state{resource=[{ok,_Ref,_Res}|_]}, {call, _, checkin, _}, ok) -> true;
postcondition(#state{resource=[{ok,_Ref,_Res}|_]}, {call, _, checkout, _}, {error,busy}) -> true;
postcondition(#state{resource=L}, {call, _, checkout, _}, {ok,_Ref,Res}) ->
    %% Gotta make sure we didn't manage to checkout the same resource twice
    case lists:keyfind(Res,3,L) of
        false -> true;
        {ok,_,Res} -> false
    end;
%% These postconditions check for what happens when we were busy beforehand
%% This state pretty much allows anything to go
postcondition(#state{resource=[{error,busy}|_]}, {call, _, checkout, _}, {error,busy}) -> true;
postcondition(#state{resource=[{error,busy}|_]}, {call, _, checkout, _}, {ok,_Ref,_Res}) -> true;
postcondition(#state{resource=[{error,busy}|_]}, {call, _, checkin, _}, busy_checkin) -> true;
%% Checking out is always fine when we had no resource checked out beforehand
postcondition(#state{resource=[]}, {call, _, checkout, _}, {ok,_Ref,_Res}) -> true;
postcondition(_, _, _) -> false.

prop_nocrash() ->
    ?FORALL(Cmds, commands(?MODULE, #state{}),
        begin
            %% the ETS table works with the dispcount dispatcher
            %% in this test to assign increasing IDs to each dispatch_watcher
            %% instance.
            Tid = ets:new(ids, [public,set]),
            ets:insert(Tid, {id,0}),
            application:stop(dispcount),
            application:start(dispcount),
            ok = ?SERVER:start_dispatch(?NAME,
                                        {?NAME, [Tid]},
                                        [{restart,permanent},{shutdown,4000},
                                         {maxr,10},{maxt,60},{resources,10}]),
            {H,_S,_R} = run_commands(?MODULE, Cmds),
            ets:delete(Tid),
            ?WHENFAIL(io:format("History: ~p~n",[{Cmds,H}]),
                      true)
        end).

%% Simple wrapper to work around limitations of statem stuff.
checkin(_Info, {error,busy}) -> busy_checkin;
checkin(Info, {ok, Ref, Res}) -> ?SERVER:checkin(Info, Ref, Res).

