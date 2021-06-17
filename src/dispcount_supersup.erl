-module(dispcount_supersup).
-behaviour(supervisor).
-export([start_dispatch/3, stop_dispatch/1, start_link/0, init/1]).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

-spec start_dispatch(Name::atom(), {module(),[term()]}, Opts::[term()]) -> ok | already_started.
start_dispatch(Name, Mod, Opts) ->
    case supervisor:start_child(?MODULE, [Name, Mod, Opts]) of
        {ok, _} -> ok;
        {error,{already_started,_}} -> already_started
    end.

-spec stop_dispatch(Name::atom()) -> ok.
stop_dispatch(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            supervisor:terminate_child(?MODULE, Pid);
        _ ->
            ok
    end.

init([]) ->
    {ok, {{simple_one_for_one, 1, 60},
          [{dispcount_sup,
            {dispcount_sup, start_link, []},
            permanent, infinity, supervisor, [dispcount_sup]}]}}.
