-record(config, {dispatch_name :: atom(),
                 num_watchers = 25 :: pos_integer(),
                 watcher_type = ets :: 'named' | 'ets',
                 dispatch_table :: ets:tid() | 'undefined',
                 dispatch_mechanism :: 'hash' | 'round_robin',
                 worker_table :: ets:tid() | 'undefined'}).
