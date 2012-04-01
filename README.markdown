# Dispcount #

Dispcount is an attempt at making more efficient resource dispatching than usual Erlang pool approaches based on a single process receiving messages from everyone and possibly getting overloaded when demand is too high, or at least seeing slower and slower response times. It is still a fairly young library and we expect it to get more stable with time.

## When should I use dispcount? ##

There have been a few characteristics assumed to be present for the design of dispcount:

- resources are limited, but the demand for them is superior to their availability.
- requests for resources are *always* incoming
- because of the previous point, it is possible and prefered to simply not queue requests for busy resources, but instantly return. Newer requests will take their spot
- low latency to know whether or not a resource is available is more important than being able to get all queries to run.

If you cannot afford to ignore a query and wish to eventually serve every one of them, dispcount might not be for you. Otherwise, you'll need to queue them yourself because all it does is grant you a resource or tell you it's busy.

## How to build ##

 `$ ./rebar compile`

## Running tests ##

Run the small Common Test suite with:

 `$ ./rebar compile && ./rebar ct`

## How to use dispcount ##

First start the application:

  `application:start(dispcount).`

When resources need to be dispatched, a dispatcher has to be started:

    ok = dispcount:start_dispatch(
            ref_dispatcher,
            {ref_dispatch, []},
             [{restart,permanent},{shutdown,4000},
              {maxr,10},{maxt,60},{resources,10}]
    )

The general form is:

    ok = dispcount:start_dispatch(
            DispatcherName,
            {CallbackMod, Arg},
             [{restart,Type},{shutdown,Timeout},
              {maxr,X},{maxt,Y},{resources,Num}]
    )

The `restart`, `shutdown`, `maxr`, and `maxt` values allow to configure the supervisor that will take care of that dispatcher. The `resources` value lets you set how many 'things' you want available. If you were handling HTTP sockets, you could use 200 connections by putting `{resources,200}`. Skip to the next section to see how to write your own dispatcher callback module.

The dispatcher is then put under the supervision structure of the dispcount application. To be able to further access resources, you need to fetch information related to the dispatcher:

    {ok, Info} = dispcount:dispatcher_info(ref_dispatcher)

This is because we want to reduce the number of calls to configuration spots and centralized points in a node. As such, you should call this function in the supervisor of whatever is going to call the dispatcher, and share the value to all children if possible. That way, a basic blob of content is going to be shared without any cost to all processes.

Using this `Info` value, calls to checkout resources can be made:

    case dispcount:checkout(Info) of
        {ok, CheckinReference, Resource} ->
            timer:sleep(10),
            dispcount:checkin(Info, CheckinReference, Resource);
        {error, busy} ->
            give_up
    end

And that's the gist of it.

## Writing a dispatcher callback module ##

Each dispatcher to allow to lend resources is written as a callback for a custom behaviour. Here's an example (tested) callback module that simply returns references:

    -module(ref_dispatch).
    -behaviour(dispcount).
    -export([init/1, checkout/2, checkin/2, handle_info/2, dead/1,
             terminate/2, code_change/3]).
    
    init([]) ->
        {ok, make_ref()}.

This one works a bit like a gen\_server. You have arguments passed and return `{ok,State}`. The state will then be carried around for the subsequent calls.

The next function is `checkout`:

    checkout(_From, Ref) ->
        {ok, Ref, undefined}.

By default, the behaviour takes care of making sure only valid requests for a checkout (resources aren't busy) are going through. The `_From` variable is the pid of the requester of a resource. This is useful if you need to change things like a socket's controlling process or a port's controller. Then, you only need to return a resource by doing `{ok, Resource, NewState}`, and the caller will see `{ok, Reference, Resource}`. The `Reference` is a token added in by dispcount and is needed to chick the resource back in. Other things to return are `{error, Reason, NewState}`, which will return `{error, Reason}` to the caller.

Finally, you can return `{stop, Reason, NewState}` to terminate the resource watcher. Note that this is risky because of how things work (see the relevant section for this later in this README).

To check resources back in, the behaviour needs to implement the following:

    checkin(Ref, undefined) ->
        {ok, Ref};
    checkin(SomeRef, Ref) ->
        {ignore, Ref}.

In this case, what happens is that we make sure that the resource that is being sent back to us is the right one. The first function clause makes sure that we only receive a reference after we've distributed one, and we then accept that one. If we receive extraneous references (maybe someone called the `checkin/3` function twice?), we ignore the result.

The second clause here is entirely optional and defensive programming. Note that checking a resource in is a synchronous operation.

The next call is the `dead/1` function:

    dead(undefined) ->
        {ok, make_ref()}.

`dead(State)` is called whenever the process that checked out a given resource has died. This is because dispcount automatically monitors them so you don't need to do it yourself. If it sees the resource owner died, it calls that function.

 This lets you create a new instance of a resource to distribute later on, if required or possible. As an example, if we were to use a permanent connection to a database as a resource, then this is where we'd set a new connection up and then keep going as if nothing went wrong.

You can also receive unexpected messages to your process, if you felt like implementing your own side-protocols or whatever:

    handle_info(_Msg, State) ->
        {ok, State}.

And finally, you benefit from a traditional OTP `terminate/2` function, and the related `code_change/3`.

    terminate(_Reason, _State) ->
        ok.
    
    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

Here's a similar callback module to handle HTTP sockets (untested):

    -module(http_dispatch).
    -behaviour(dispcount).
    -export([init/1, checkout/2, checkin/2, handle_info/2, dead/1, terminate/2, code_change/3]).
    
    -record(state, {resource, given=false, port}).
    
    init([{port,Num}]) ->
        {ok,Socket} = gen_tcp:connect({127,0,0,1}, Num, [binary]),
        {ok, #state{resource=Socket, port=Num}}.
    
    %% just in case, but that should never happen anyway :V I'm paranoid!
    checkout(_From, State = #state{given=true}) ->
        {error, busy, State};
    checkout(From, State = #state{resource=Socket}) ->
        gen_tcp:controlling_process(Socket, From),
        %% We give our own pid back so that the client can make this
        %% callback module the controlling process again before
        %% handing it back.
        {ok, {self(), Socket}, State#state{given=true}}.
    
    checkin(Socket, State = #state{resource=Socket, given=true}) ->
        %% This assumes the client made us the controlling process again.
        %% This might be done via a client lib wrapping dispcount calls of
        %% some sort.
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

## How does it work ##

What killed most of the pool and dispatch systems I used before was the amount of messaging required to make things work. When many thousands of processes require information from a central point at once, performance would quickly degrade as soon as the protocol had some messaging involved at its core.

We'd see mailbox queue build-up, busy schedulers, and booming memory. Dispcount tries to solve the problem by using the ubiquitous Erlang optimization tool: ETS tables.

The core concept of dispcount is based on two ETS tables: a dispatch table (write-only) and a worker matchup table (read-only). Two tables because what costs the most performance with ETS in terms of concurrency is switching between reading and writing.

In each of the table, `N` entries are added: one for each resource available, matching with a process that manages that resource (a *watcher*). Persistent hashing of the resources allows to dispatch queries uniformly to all of these watchers. Once you know which watcher your request is dedicated to, the dispatch table is called into action.

The dispatch table manages to allow both reads and writes while remaining write-only. The trick is to use the `ets:update_counter` functions, which atomically increment a counter and return the value, although the operation is only writing and communicating a minimum of information.

The gist of the idea is that you can only get the permission to message the watcher if you're the first one to increment the counter. Other processes that try to do it just instantly give up. This guarantees that a single caller at a time has the permission to message a given worker, a bit like a mutex, but implemented efficiently (for Erlang, that is).

Then the lookup table comes in action; because we have the permission to message a watcher, we look up its pid, and then send a message.

Whenever we check a resource back in or the process that acquired it dies, the counter is reset to 0 and a new request can come in and take its place.

Generally, this allows us to move the bottleneck of similar applications away from a single process and its mailbox, to an evenly distributed number of workers. Then the next bottleneck will be the ETS tables (both set with read and write concurrency options), which are somewhat less likely to be as much of a hot spot.

## I get crashes in R14, help me! ##

The error you see is likely `{start_spec,{invalid_shutdown,infinity}}`. This is due to Erlang/OTP releases R14 (or generally, versions prior to R15) sometimes disliking the atom `infinity` in child specifications. If you have this problem, use the branch `r14` instead of `master`. It changes the `infinity` value to `120000` (arbitrarily long value), which should hopefully make things work smoothly (it did for me).

## What's left to do? ##

- More complete testing suite.
- Adding a function call to allow the transfer of ownership from a process to another one to avoid messing with monitoring in the callback module.
- Testing to make sure the callback modules can be updated with OTP relups and appups. This is so far untested.
- Allowing dynamic resizing of pools.
