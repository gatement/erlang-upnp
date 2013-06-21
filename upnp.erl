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


post() ->
    ServerHost = "192.168.1.7",
    ServerPort = 1900,
    case gen_tcp:connect(ServerHost, ServerPort, [binary, {active, true}]) of
        {ok, Socket} ->
            error_logger:info_msg("client===============================> ~p~n", ["connected"]),    
            do_post(Socket),
            receive Any ->
                        io:format("received:~p~n", [Any])
            after 10000 ->
                error
            end,
            gen_tcp:close(Socket),
            error_logger:info_msg("client===============================> ~p~n", ["disconnected"]), 
            ok;
        _ ->
            Reason = "connection error",
            io:format("~p", [Reason])
    end.


do_post(Socket) ->
    DeviceType = "WANIPConnection:1",
    Action = "GetStatusInfo",
    ControlUrl = "/ipc",
    Host = "192.168.1.7",
    Port = 1900,

    Body = io_lib:format("<?xml version=\"1.0\"?><s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:~s xmlns:u=\"urn:schemas-upnp-org:service:~s\"></u:~s></s:Body></s:Envelope>", 
        [Action, DeviceType, Action]),

    Length = erlang:length(Body),

    Msg = lists:flatten(io_lib:format("POST ~s HTTP/1.1\r\nHOST: ~s:~p\r\nCONTENT-TYPE: text/xml; charset=\"utf-8\"\r\nCONTENT-LENGTH: ~p\r\nUSER-AGENT: OS/version UPnP/1.1 product/version\r\nSOAPACTION: \"urn:schemas-upnp-org:service:~s#~s\"\r\n\r\n~s", 
        [ControlUrl, Host, Port, Length, DeviceType, Action, Body])),

    io:format("~n~p~n~n", [Msg]),

    gen_tcp:send(Socket, erlang:list_to_binary(Msg)).


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
