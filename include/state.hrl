-record(config, {dispatch_name :: atom(),
                 num_watchers = 25 :: pos_integer(),
                 watcher_type = ets :: 'named' | 'ets',
                 dispatch_table :: ets:tid() | 'undefined',
                 worker_table :: ets:tid() | 'undefined'}).
