-module(dispcount_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
-export([starting/1, starting_named/1, stopping/1, overload/1, dead/1, error/1,
         restart/1, timer/1]).

suite() -> [{timetrap,{seconds,30}}].

groups() ->
    [{hash, [], [starting, starting_named, stopping, overload, dead, error,
                 restart, timer]},
     {round_robin, [], [starting, starting_named, stopping, overload, dead, error,
                        restart, timer]}].

all() -> [{group, hash}, {group, round_robin}].

init_per_suite(Config) ->
    application:start(dispcount),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(hash, Config) ->
    [{dispatch_mechanism, hash} | Config];
init_per_group(round_robin, Config) ->
    [{dispatch_mechanism, round_robin} | Config].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(overload, Config) ->
    ok = dispcount:start_dispatch(
            ref_overload_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,2},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_overload_dispatcher),
    [{info, Info} | Config];
init_per_testcase(dead, Config) ->
    ok = dispcount:start_dispatch(
            ref_dead_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_dead_dispatcher),
    [{info, Info} | Config];
init_per_testcase(error, Config) ->
    ok = dispcount:start_dispatch(
            ref_error_dispatcher,
            {ref_dispatch_error, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_error_dispatcher),
    [{info, Info} | Config];
init_per_testcase(restart, Config) ->
    Ref = make_ref(),
    ok = dispcount:start_dispatch(
            ref_restart_dispatcher,
            {ref_dispatch_restart, [Ref]},
             [{restart,permanent},{shutdown,4000},
              {maxr,100},{maxt,1},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_restart_dispatcher),
    [{info, Info},{ref,Ref} | Config];
init_per_testcase(timer, Config) ->
    ok = dispcount:start_dispatch(
            ref_timer_dispatcher,
            {ref_dispatch_noreply, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,1},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_timer_dispatcher),
    [{info, Info} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(overload, _Config) ->
    dispcount:stop_dispatch(ref_overload_dispatcher);
end_per_testcase(dead, _Config) ->
    dispcount:stop_dispatch(ref_dead_dispatcher);
end_per_testcase(error, _Config) ->
    dispcount:stop_dispatch(ref_error_dispatcher);
end_per_testcase(restart, _Config) ->
    dispcount:stop_dispatch(ref_restart_dispatcher);
end_per_testcase(timer, _Config) ->
    dispcount:stop_dispatch(ref_timer_dispatcher);
end_per_testcase(_, _Config) ->
    ok.

starting(Config) ->
    ok = dispcount:start_dispatch(
            ref_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,10},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, Info} = dispcount:dispatcher_info(ref_dispatcher),
    case dispcount:checkout(Info) of
        {ok, CheckinReference, Resource} ->
            timer:sleep(10),
            dispcount:checkin(Info, CheckinReference, Resource);
        {error, busy} ->
            give_up
    end,
    dispcount:stop_dispatch(ref_dispatcher).

starting_named(Config) ->
    ok = dispcount:start_dispatch(
            first_named_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,10},{watcher_type,named},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    ok = dispcount:start_dispatch(
            second_named_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,10},{watcher_type,named},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    {ok, FirstInfo} = dispcount:dispatcher_info(first_named_dispatcher),
    {ok, FirstRef, FirstRes} = dispcount:checkout(FirstInfo),
    dispcount:checkin(FirstInfo, FirstRef, FirstRes),

    {ok, SecondInfo} = dispcount:dispatcher_info(second_named_dispatcher),
    {ok, SecondRef, SecondRes} = dispcount:checkout(SecondInfo),
    dispcount:checkin(SecondInfo, SecondRef, SecondRes),

    dispcount:stop_dispatch(first_named_dispatcher),
    dispcount:stop_dispatch(second_named_dispatcher),
    ok.

stopping(Config) ->
    ok = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    already_started = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
    ),
    dispcount:stop_dispatch(stop_dispatch),
    ok = dispcount:start_dispatch(
            stop_dispatch,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,1},
              {dispatch_mechanism, ?config(dispatch_mechanism, Config)}]
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

restart(Config) ->
    %% One resource available.
    Info = ?config(info, Config),
    Res = ?config(ref, Config),
    put(crash, true),
    %% Crashing the handler should make it possible to restart it
    {'EXIT',_} = (catch dispcount:checkout(Info)),
    timer:sleep(500),
    put(crash, false),
    {ok, _Ref, Res} = dispcount:checkout(Info).

timer(Config) ->
    %% Never replying would mean a timeout after 5ms
    %% Allow for more strict or permissive values
    Info = ?config(info, Config),
    T1 = os:timestamp(),
    {'EXIT',{timeout, _}} = (catch dispcount:checkout(Info, 200)),
    T2 = os:timestamp(),
    true = 300000 > timer:now_diff(T2,T1).
