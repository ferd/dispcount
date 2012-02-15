-module(dispcount).
-behaviour(application).
-export([start/2,stop/1]).
-export([start_dispatch/3, stop_dispatch/1, dispatcher_info/1, checkout/1, checkin/3]).
-export([behaviour_info/1]).

%% eventually switch to -callback if it becomes backwards compatible
behaviour_info(callbacks) ->
    [{init,1},
     {checkout, 2},
     {checkin, 2},
     {handle_info,2},
     {dead,1},
     {terminate,2},
     {code_change,3}];
behaviour_info(_Other) ->
    undefined.

-spec start(normal, _) -> {ok, pid()}.
start(normal, _Args) ->
    dispcount_supersup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.

-spec stop_dispatch(Name::atom()) -> ok.
stop_dispatch(Name) ->
    dispcount_supersup:stop_dispatch(Name).

-spec start_dispatch(Name::atom(), {module(), _}, term()) -> ok | already_defined.
start_dispatch(Name, Mod={M,A}, DispatchOpts) when is_atom(M) ->
    Res = dispcount_supersup:start_dispatch(Name, Mod, DispatchOpts),
    %% wait for all tables to be there. A bit messy, but it can be done:
    dispcount_serv:wait_for_dispatch(Name, infinity),
    Res.

%% Should be called as infrequently as possible
-spec dispatcher_info(Name::atom()) -> term().
dispatcher_info(Name) ->
    dispcount_serv:get_info(Name).

-spec checkout(term()) -> {ok, term(), term()} | {error, term()}.
checkout(Info) ->
    dispcount_watcher:checkout(Info).

-spec checkin(term(), term(), term()) -> ok.
checkin(Info, CheckinRef, Resource) ->
    dispcount_watcher:checkin(Info, CheckinRef, Resource).
