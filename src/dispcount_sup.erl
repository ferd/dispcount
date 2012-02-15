-module(dispcount_sup).
-behaviour(supervisor).
-export([start_link/3, init/1]).

-spec start_link(Name::atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Mod, InitOpts) ->
    supervisor:start_link({local, Name}, ?MODULE, {Name,Mod,InitOpts}).

init({Name,Mod,InitOpts}) ->
    %% dispcount_sup is started by dispcount_serv
    {ok, {{one_for_all, 1, 60}, % once a minute is pretty generous
          [%{watchers_sup, {dispcount_sup, start_link, [Name, Mod, InitOpts]},
           %  permanent, infinity, supervisor, [dispcount_sup]},
           {info_server,
            {dispcount_serv, start_link, [self(), Name, Mod, InitOpts]},
             permanent, infinity, worker, [dispcount_serv]}
          ]}}.
