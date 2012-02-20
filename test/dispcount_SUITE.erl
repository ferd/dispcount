-module(dispcount_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([starting/1, stopping/1, overload/1, dead/1, error/1]).

all() -> [starting, stopping, overload, dead, error].

init_per_suite(Config) ->
    application:start(dispcount),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(overload, Config) ->
    ok = dispcount:start_dispatch(
            ref_overload_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,2}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_overload_dispatcher),
    [{info, Info} | Config];
init_per_testcase(dead, Config) ->
    ok = dispcount:start_dispatch(
            ref_dead_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_dead_dispatcher),
    [{info, Info} | Config];
init_per_testcase(error, Config) ->
    ok = dispcount:start_dispatch(
            ref_error_dispatcher,
            {ref_dispatch_error, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_error_dispatcher),
    [{info, Info} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(overload, Config) ->
    dispcount:stop_dispatch(ref_overload_dispatcher);
end_per_testcase(dead, Config) ->
    dispcount:stop_dispatch(ref_dead_dispatcher);
end_per_testcase(_, Config) ->
    ok.

starting(_Config) ->
    ok = dispcount:start_dispatch(
            ref_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,10}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_dispatcher),
    case dispcount:checkout(Info) of
        {ok, CheckinReference, Resource} ->
            timer:sleep(10),
            dispcount:checkin(Info, CheckinReference, Resource);
        {error, busy} ->
            give_up
    end.

stopping(_Config) ->
    ok = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1}]
    ),
    already_started = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1}]
    ),
    dispcount:stop_dispatch(stop_dispatch),
    ok = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1}]
    ),
    dispcount:stop_dispatch(stop_dispatch).

overload(Config) ->
    %% should be two workers max. Loop until we reach overload,
    %% then a bit more to make sure nothing is available (damn hashing makes
    %% things non-deterministic), then free resources and check that we
    %% can access more.
    Info = ?config(info, Config),
    %% the list comprehension monad, hell yes! Skip all busy calls and see that
    %% only two resources are acquired
    Resources = [{Ref, Res} || _ <- lists:seq(1,20), {ok, Ref, Res} <- [dispcount:checkout(Info)]],
    2 = length(Resources),
    [] = [{Ref, Res} || _ <- lists:seq(1,100), {ok, Ref, Res} <- [dispcount:checkout(Info)]],
    %% turning ressources in
    [dispcount:checkin(Info, Ref, Res) || {Ref, Res} <- Resources],
    %% then we're able to get more in.
    timer:sleep(100),
    Resources2 = [{Ref, Res} || _ <- lists:seq(1,20), {ok, Ref, Res} <- [dispcount:checkout(Info)]],
    2 = length(Resources2).

dead(Config) ->
    %% The dispatcher with this test has 1 ressource available.
    Info = ?config(info, Config),
    %% resource owners should be monitored automatically and handled when stuff dies.
    spawn(fun() -> dispcount:checkout(Info), timer:sleep(500) end),
    timer:sleep(100),
    {error, busy} = dispcount:checkout(Info),
    timer:sleep(500),
    {ok, _Ref, _Res} = dispcount:checkout(Info).

%% an error being returned resets the counter
error(Config) ->
    %% The dispatcher has 1 resource available
    Info = ?config(info, Config),
    %% returning a custom error (as done in the dispatch callback module for
    %% this test) should reset the counter.
    {error, denied} = dispcount:checkout(Info),
    %% if we get {error, busy}, this is an error.
    {error, denied} = dispcount:checkout(Info).
