-module(watchers_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-spec start_link({{supervisor:strategy(), pos_integer(), pos_integer()}, [supervisor:child_spec()]}) -> {ok, pid()}.
start_link(Spec) ->
    supervisor:start_link(?MODULE, Spec).

%% the spec is coming from dispcount_serv, tunneled through
%% dispcount_sup.
init(Spec) ->
    {ok, Spec}.
