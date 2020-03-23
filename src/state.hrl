-record(config, {dispatch_name :: atom(),
                 num_watchers = 25 :: pos_integer(),
                 watcher_type = ets :: 'named' | 'ets' | 'atomics',
                 dispatch_table :: ets:tid() | atomics:atomics_ref() | 'undefined',
                 dispatch_mechanism = hash :: 'hash' | 'round_robin',
                 worker_table :: ets:tid() | 'undefined'}).
