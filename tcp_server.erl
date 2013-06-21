-module(tcp_server).
-compile(export_all).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    erlang:spawn(fun() -> start(55555) end).


%% ===================================================================
%% Local Functions
%% ===================================================================

start(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    loop(LSocket, Socket).


loop(LSocket, Socket) ->
    receive
         {tcp_closed, _} ->
            io:format("received : ~p~n", [tcp_closed]),
            {ok, Socket2} = gen_tcp:accept(LSocket),
            loop(LSocket, Socket2);
        AnyMsg ->
            io:format("received : ~p~n", [AnyMsg]),
            loop(LSocket, Socket)
    end.
