-module(ref_dispatch).
-behaviour(dispcount).
-export([init/1, checkout/2, checkin/2, handle_info/2, dead/1,
         terminate/2, code_change/3]).

init([]) ->
    {ok, make_ref()}.

checkout(_From, Ref) ->
    {ok, Ref, undefined}.

checkin(Ref, undefined) ->
    {ok, Ref};
checkin(SomeRef, Ref) ->
    {ignore, Ref}.

dead(undefined) ->
    {ok, make_ref()}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
