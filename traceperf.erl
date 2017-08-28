-module(trace_perf).

-compile([export_all]).

%% supported tracer types
-define(FILE_PORT, file_port).
-define(TCP_PORT, tcp_port).
-define(LOCAL_PROCESS, local_process).
-define(REMOTE_PROCESS, remote_process).

-define(TRACER_TCP_PORT, 6666).

start(Tracer_type, Calls) ->
    try
        {ok,_} = dbg:start(),
        tracer(Tracer_type, Calls),
        {ok,_} = apply_tracing(),

        Arg = binary:copy(<<"q">>, 1000),
        ok = execute_function_calls(Arg, Calls),
        ok = await_trace_received_completion(Tracer_type),
        ok = dbg:stop()
    catch
        C:R ->
            io:format("EXCEPTION THROWN ~p ~p", [C, R])
    end,
    erlang:halt(0).

%% the function to be called by tracing
traced_function(Arg) ->
    {ok,Arg}.

call_traced_function(_, 0) ->
    ok;
call_traced_function(Arg, Calls) ->
    traced_function(Arg),
    call_traced_function(Arg, Calls-1).

execute_function_calls(Arg, Calls_total) ->
    Num_schedulers = erlang:system_info(schedulers),
    Process_calls = Calls_total div Num_schedulers,
    Self = self(),
    Fn =
        fun() ->
            call_traced_function(Arg, Process_calls),
            Self ! {self(), completed}
        end,
    Pids = [spawn_link(Fn) || _ <- lists:seq(1,Num_schedulers)],
    await_call_completion(Pids).

await_call_completion([]) ->
    ok;
await_call_completion(Pids1) ->
    receive
        {Pid, completed} ->
            Pids2 = lists:delete(Pid, Pids1),
            await_call_completion(Pids2)
    end.

%% start the tracer, the method of handling traces
%% for the given tracing type
tracer(?LOCAL_PROCESS, Calls) when is_integer(Calls) ->
    Self = self(),
    Tracer_pid = spawn_link(
        fun() ->
            tracer_process_loop(Calls),
            Self ! trace_perf_complete

        end),
    Acc = acc,
    {ok,_} = dbg:tracer(process,
        {fun(Trace,_) ->
                    Tracer_pid ! Trace, Acc
         end, Acc});
tracer(?FILE_PORT, _) ->
    {ok,_} = dbg:tracer(port,dbg:trace_port(file,"/tmp/trace-perf-log"));
tracer(?REMOTE_PROCESS, _) ->
    pong = net_adm:ping('remote_node@127.0.0.1'),
    Acc = acc,
    {ok,_} = dbg:tracer(process,
        {fun(Trace,_) ->
                {'remote_node@127.0.0.1', receiver_proc} ! Trace,
                Acc
         end, Acc});
tracer(?TCP_PORT, _) ->
    Max_queue = 1000,
    {ok,_} = dbg:tracer(port,
        dbg:trace_port(ip,{?TRACER_TCP_PORT, Max_queue})),
    _Client_pid = spawn_monitor(fun tracer_tcp_client_proc/0).

%%
tracer_process_loop(0) ->
    ok;
tracer_process_loop(Calls) ->
    %% just receive a message and throw it away, we're
    %% only measuring *sending* the message locally
    receive
        _ -> ok
    end,
    tracer_process_loop(Calls-1).

await_trace_received_completion(?FILE_PORT) ->
    ok = dbg:flush_trace_port();
await_trace_received_completion(?LOCAL_PROCESS) ->
    receive
        trace_perf_complete -> ok
    end;
await_trace_received_completion(?REMOTE_PROCESS) ->
    ok;
await_trace_received_completion(?TCP_PORT) ->
    receive
        Msg when element(1,Msg) == 'DOWN' ->
            io:format("FUCK ~p", [Msg])
    after 0 ->
        ok
    end,
    ok.

apply_tracing() ->
    %% trace calls from all processes
    {ok,_} = dbg:p(all, c),
    {ok,_} = dbg:tpl(?MODULE, traced_function, 1, []).

be_remote_node() ->
    register(receiver_proc, self()),
    be_remote_node2().

be_remote_node2() ->
    receive
        _ -> be_remote_node()
    end.

tracer_tcp_client_proc() ->
    {ok, Socket} = gen_tcp:connect(
        "localhost", ?TRACER_TCP_PORT, [{active, false}]),
    tracer_tcp_client_proc_recv(Socket).

tracer_tcp_client_proc_recv(Socket) ->
    case gen_tcp:recv(Socket, 10240) of
        {ok,_} ->
            tracer_tcp_client_proc_recv(Socket);
        {error,_} = Error ->
            exit(Error)
    end.