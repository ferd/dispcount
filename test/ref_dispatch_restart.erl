-module(ref_dispatch_restart).
-behaviour(dispcount).
-export([init/1, checkout/2, checkin/2, handle_info/2, dead/1,
         terminate/2, code_change/3]).

init([Ref]) ->
    {ok, Ref}.

checkout(From, Ref) ->
    {dictionary, Dict} = erlang:process_info(From, dictionary),
    case proplists:get_value(crash, Dict) of
        true -> erlang:error(asked_for);
        false -> {ok, Ref, undefined}
    end.

checkin(Ref, undefined) ->
    {ok, Ref};
checkin(_SomeRef, Ref) ->
    {ignore, Ref}.

dead(undefined) ->
    {ok, make_ref()}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

