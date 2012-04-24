-module(prop_dispatch).
-behaviour(dispcount).
-export([init/1, checkout/2, checkin/2, handle_info/2, dead/1,
         terminate/2, code_change/3]).

init([Tid]) ->
    Id = ets:update_counter(Tid, id, 1),
    {ok, Id}.

checkout(_From, Id) ->
    {ok, Id, Id}.

checkin(Id, Id) ->
    {ok, Id};
checkin(_SomeId, Id) ->
    {ignore, Id}.

dead(Id) ->
    {ok, Id}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

