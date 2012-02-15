-module(http_dispatch).
-behaviour(dispcount).
-export([init/1, checkout/2, checkin/2, handle_info/2, dead/1, terminate/2, code_change/3]).

-record(state, {resource, given=false, port}).

init([{port,Num}]) ->
    {ok,Socket} = gen_tcp:connect({127,0,0,1}, Num, [binary]),
    {ok, #state{resource=Socket, port=Num}}.

checkout(_From, State = #state{given=true}) ->
    {error, busy, State};
checkout(From, State = #state{resource=Socket}) ->
    gen_tcp:controlling_process(Socket, From),
    {ok, Socket, State#state{given=true}}.

checkin(Socket, State = #state{resource=Socket, given=true}) ->
    {ok, State#state{given=false}};
checkin(_Socket, State) ->
    %% The socket doesn't match the one we had -- an error happened somewhere
    {ignore, State}.

dead(State) ->
    %% aw shoot, someone lost our resource, we gotta create a new one:
    {ok, NewSocket} = gen_tcp:connect({127,0,0,1}, State#state.port, [binary]),
    {ok, State#state{resource=NewSocket,given=false}}.
    %% alternatively:
    %% {stop, Reason, State}

handle_info(_Msg, State) ->
    %% something unexpected with the TCP connection if we set it to active,once???
    {ok, State}.

terminate(_Reason, _State) ->
    %% let the GC clean the socket.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
