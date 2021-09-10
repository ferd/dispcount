-module(dispcount).
-behaviour(application).
-export([start/2,stop/1]).
-export([start_dispatch/3, stop_dispatch/1,
         dispatcher_info/1, checkout/1, checkout/2, transaction/2, checkin/3]).

-callback init(Args::[any()]) -> {ok, CallbackState::any()}.
-callback checkout(From::pid(), CallbackState::any()) -> {ok, Resource::any(), NewCallbackState::any()} |
                                    {error, Reason::any(), NewCallbackState::any()} |
                                    {stop, Reason::any(), NewCallbackState::any()}.
-callback transaction(From::pid(), Fun::function(), CallbackState::any()) -> ok |
                                    {ok, NewCallbackState::any()} |
                                    {stop, Reason::any(), NewCallbackState::any()}.
-callback checkin(Resource::any(), CallbackState::any()) -> {ok, NewCallbackState::any()} |
                                   {ignore, CallbackState::any()} |
                                   {stop, Reason::any(), NewCallbackState::any()}.
-callback handle_info(Info::any(), CallbackState::any()) -> {ok, NewCallbackState::any()} |
                                                            {stop, Reason::any(), NewCallbackState::any()}.
-callback dead(CallbackState::any()) -> {ok, NewCallbackState::any()} |
                                        {stop, Reason::any(), NewCallbackState::any()}.
-callback terminate(Reason::any(), CallbackState::any()) -> Ignored::any().

-spec start(normal, _) -> {ok, pid()}.
start(normal, _Args) ->
    dispcount_supersup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.

-spec stop_dispatch(Name::atom()) -> ok.
stop_dispatch(Name) ->
    dispcount_supersup:stop_dispatch(Name).

-spec start_dispatch(Name::atom(), {module(), _}, term()) -> ok | already_started.
start_dispatch(Name, Mod={M,_A}, DispatchOpts) when is_atom(M) ->
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
    dispcount_watcher:checkout(Info, 5000).

-spec checkout(term(), timeout()) -> {ok, term(), term()} | {error, term()}.
checkout(Info, Timeout) ->
    dispcount_watcher:checkout(Info, Timeout).

-spec transaction(term(), function()) -> ok.
transaction(Info, Fun) ->
    dispcount_watcher:transaction(Info, Fun).

-spec checkin(term(), term(), term()) -> ok.
checkin(Info, CheckinRef, Resource) ->
    dispcount_watcher:checkin(Info, CheckinRef, Resource).
