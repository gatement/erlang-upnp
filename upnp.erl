-module(upnp).
-compile(export_all).

%%===================== client ============================
client() ->
    client(<<"M-SEARCH * HTTP/1.1\r\nHost:239.255.255.250:1900\r\nMan:\"ssdp:discover\"\r\nMx:5\r\nST:ssdp:rootdevice\r\n\r\n">>).

client(Request) ->
    {ok, Socket} = gen_udp:open(0, [binary, {broadcast, true}]),
    ok = gen_udp:send(Socket, "239.255.255.250" , 1900, Request),
    receive Any ->
                io:format("received:~p~n", [Any])
    after 10000 ->
        error
    end,

    gen_udp:close(Socket).


%%===================== server ============================

server() ->
    spawn(fun() -> server(4000) end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    loop(Socket).


loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} ->
        io:format("received:~p~n", [Bin]),
        BinReply = <<"received!">>,
        gen_udp:send(Socket, Host, Port, BinReply),
        loop(Socket)
    end.
