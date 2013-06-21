-module(tcp_client).
-compile(export_all).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
   start("219.137.72.159", 55556). %"219.137.72.159"

start(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, true}]) of
        {ok, Socket} ->
            io:format("==========================> ~p~n", ["connected"]),
            Data = get_status_data(), 
            io:format("sending data: ~p~n", [Data]), 
            gen_tcp:send(Socket, Data),
            %loop(Socket);
            gen_tcp:close(Socket),
            io:format("==========================> ~p~n", ["disconnected"]),
            ok;
        _ ->
            Reason = "connection error",
            io:format("==========================> ~p~n", [Reason])
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

loop(Socket) ->
    receive
        {tcp_closed, _Socket} ->
            error_logger:info_msg("tcp_closed ~p~n", [erlang:self()]);
        AnyMsg ->
            error_logger:info_msg("received any data ~p: ~p~n", [erlang:self(), AnyMsg]),
            loop(Socket)
    end.


get_status_data() ->
    Data = [16#AA,0,0,0,0,0,0,16#55],

    erlang:list_to_binary(Data).