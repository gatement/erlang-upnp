-module(upnp).
-compile(export_all).


%% ===================================================================
%% API functions
%% ===================================================================

get_external_ip() ->
    {GatewayIp, GatewayPort} = get_gateway_ip_port(),

    case gen_tcp:connect(GatewayIp, GatewayPort, [list, {active, true}]) of
        {ok, Socket} ->
            %io:format("==========================> ~p~n", ["connected"]),    
            RequestData = get_GetExternalIPAddress_request_data(GatewayIp, GatewayPort),
            gen_tcp:send(Socket, RequestData),
            ExternalIp = receive {tcp, _, Msg} ->
                        %io:format("received:~p~n", [Msg]),
                        StartIndex = string:str(Msg, "<NewExternalIPAddress>") + erlang:length("<NewExternalIPAddress>"),
                        Length = string:str(Msg,"</NewExternalIPAddress>") - StartIndex,
                        string:substr(Msg, StartIndex, Length)
            after 10000 ->
                error
            end,
            gen_tcp:close(Socket),
            %io:format("==========================> ~p~n", ["disconnected"]), 
            ExternalIp;
        _ ->
            Reason = "connection error",
            io:format("~p", [Reason]),
            error
    end.


add_port_mapping() ->
    add_port_mapping(55556, 55555, 200).


add_port_mapping(ExternalPort, InternalPort, LeaseDuration) ->
    {GatewayIp, GatewayPort} = get_gateway_ip_port(),

    case gen_tcp:connect(GatewayIp, GatewayPort, [list, {active, true}]) of
        {ok, Socket} ->
            io:format("==========================> ~p~n", ["connected"]),    
            RequestData = get_AddPortMapping_request_data(GatewayIp, GatewayPort, ExternalPort, InternalPort, LeaseDuration),
            gen_tcp:send(Socket, RequestData),
            receive {tcp, _, Msg} ->
                        io:format("received:~p~n", [Msg])
            after 10000 ->
                error
            end,
            gen_tcp:close(Socket),
            io:format("==========================> ~p~n", ["disconnected"]);
        _ ->
            Reason = "connection error",
            io:format("~p", [Reason]),
            error
    end.

%% ===================================================================
%% Local Functions
%% ===================================================================

get_gateway_ip_port() ->
    Msg = msearch(),
    StartIndex = string:str(Msg, "http://") + erlang:length("http://"),
    Msg2 = string:substr(Msg, StartIndex),

    StartIndex2 = string:str(Msg2, ":"),
    StartIndex3 = string:str(Msg2, "/"),
    GatewayIp = string:substr(Msg2, 1, StartIndex2 - 1),
    GatewayPort = erlang:list_to_integer(string:substr(Msg2, StartIndex2 + 1, StartIndex3 - StartIndex2 -1)),

    {GatewayIp, GatewayPort}.


msearch() ->
    Request = <<"M-SEARCH * HTTP/1.1\r\nHost:239.255.255.250:1900\r\nMan:\"ssdp:discover\"\r\nMx:5\r\nST:ssdp:rootdevice\r\n\r\n">>,

    {ok, Socket} = gen_udp:open(0, [list, {broadcast, true}]),
    ok = gen_udp:send(Socket, "239.255.255.250", 1900, Request),
    Result = receive 
        {udp, _Socket, _Ip, _Port, Msg} ->
            %io:format("M-SEARCH result: ~p~n", [Msg]), 
            Msg
    after 10000 ->
        error
    end,

    gen_udp:close(Socket), 

    Result.


get_GetExternalIPAddress_request_data(GatewayIp, GatewayPort) ->
    Body = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><SOAP-ENV:Body><m:GetExternalIPAddress xmlns:m=\"urn:schemas-upnp-org:service:WANIPConnection:1\"/></SOAP-ENV:Body></SOAP-ENV:Envelope>",
    BodyLength = erlang:length(Body),
    Header = io_lib:format("POST /ipc HTTP/1.1\r\nCache-Control: no-cache\r\nConnection: Close\r\nPragma: no-cache\r\nContent-Type: text/xml; charset=\"utf-8\"\r\nUser-Agent: Microsoft-Windows/6.1 UPnP/1.0\r\nSOAPAction: \"urn:schemas-upnp-org:service:WANIPConnection:1#GetExternalIPAddress\"\r\nContent-Length: ~p\r\nHost: ~s:~p\r\n\r\n", [BodyLength, GatewayIp, GatewayPort]),
    
    erlang:list_to_binary(lists:flatten(Header ++ Body)).


get_AddPortMapping_request_data(GatewayIp, GatewayPort, ExternalPort, InternalPort, LeaseDuration) ->
    LocalIp = get_local_ip(),

    Body = io_lib:format("<?xml version=\"1.0\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><SOAP-ENV:Body><m:AddPortMapping xmlns:m=\"urn:schemas-upnp-org:service:WANIPConnection:1\"><NewRemoteHost xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"string\"></NewRemoteHost><NewExternalPort xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"ui2\">~p</NewExternalPort><NewProtocol xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"string\">TCP</NewProtocol><NewInternalPort xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"ui2\">~p</NewInternalPort><NewInternalClient xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"string\">~s</NewInternalClient><NewEnabled xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"boolean\">1</NewEnabled><NewPortMappingDescription xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"string\">JohnsonTest</NewPortMappingDescription><NewLeaseDuration xmlns:dt=\"urn:schemas-microsoft-com:datatypes\" dt:dt=\"ui4\">~p</NewLeaseDuration></m:AddPortMapping></SOAP-ENV:Body></SOAP-ENV:Envelope>", [ExternalPort, InternalPort, LocalIp, LeaseDuration]),
    BodyLength = erlang:length(Body),
    Header = io_lib:format("POST /ipc HTTP/1.1\r\nCache-Control: no-cache\r\nConnection: Close\r\nPragma: no-cache\r\nContent-Type: text/xml; charset=\"utf-8\"\r\nUser-Agent: Microsoft-Windows/6.1 UPnP/1.0\r\nSOAPAction: \"urn:schemas-upnp-org:service:WANIPConnection:1#AddPortMapping\"\r\nContent-Length: ~p\r\nHost: ~s:~p\r\n\r\n", [BodyLength, GatewayIp, GatewayPort]),
    
    erlang:list_to_binary(lists:flatten(Header ++ Body)).


get_local_ip() ->
   {ok, HostName} = inet:gethostname(),
   {ok, {A, B, C, D}} = inet:getaddr(HostName, inet),
   lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).