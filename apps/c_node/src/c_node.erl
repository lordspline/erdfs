-module(c_node).
-export([init/1, ip_provider/1, get_file_list/0, get_file/1, upload_file/1, delete_file/1]).

% spec : 
% 
% -------------------------------------------------------------------------------------------------
% 
% from m_node : 
%   - response to c_get_file_list - list of files
%   - response to c_get_file - s_nodes that have the file
%   - resopnse to c_upload_file - s_nodes that can store the file
% 
% from s_node : 
%   - response to c_get_file - accept file
%
% -------------------------------------------------------------------------------------------------
% 
% connect to master node using provided nodename
% get master node ip address using rpc (call getif)
% spawn process that provides the received ip address (for further messaging, so we dont need to save any state)
% 
% -------------------------------------------------------------------------------------------------

%init node with provided m_nodename
-spec init(M_Nodename :: string()) -> no_return().
init(M_Nodename) ->
    {ok, _} = net_kernel:start([c_node, shortnames]),
    true = erlang:set_cookie(erdfs),
    true = net_kernel:connect_node(list_to_atom(M_Nodename)),
    {ok, [{Ip, _, _}, _]} = rpc:call(list_to_atom(M_Nodename), inet, getif, []),
    register(ip_provider, spawn(?MODULE, ip_provider, [Ip])).

%ip provider process, provides ip address of the m_node to other processes
-spec ip_provider(Ip :: inet:ip4_address()) -> no_return().
ip_provider(Ip) ->
    receive
        {From, get_ip_address} ->
            From ! {ip_address, Ip};
        _ -> 
            io:format("ip provider received invalid message")
    end,
    ip_provider(Ip).

%get ip address of m_node from ip_provider
-spec get_ip_address(RequestingNode :: any()) -> inet:ip4_address().
get_ip_address(RequestingNode) ->
    ip_provider ! {RequestingNode, get_ip_address},
    receive
        {ip_address, Ip} ->
            Ip
    end.

%make get_file_list request to master node
-spec get_file_list() -> no_return().
get_file_list() ->
    Ip = get_ip_address(self()),
    {c_get_file_list_response, ListOfFiles} = binary_to_term(send_and_recv(Ip, term_to_binary({c_get_file_list, {}}))),
    ListOfFiles.

%make get_file request to master node
-spec get_file(Filename :: string()) -> no_return().
get_file(Filename) ->
    Ip = get_ip_address(self()),
    {c_get_file_response, ListOfNodesWithFile} = binary_to_term(send_and_recv(Ip, term_to_binary({c_get_file, {Filename}}))),
    case attempt_get_file(Filename, ListOfNodesWithFile, 0) of 
        error ->
            io:format("Could not obtain file.");
        ok ->
            io:format("File obtained.")
    end.

%make upload_file request to master node
-spec upload_file(Filename :: string()) -> no_return().
upload_file(Filename) ->
    Ip = get_ip_address(self()),
    {c_upload_file_response, ListOfNodes} = binary_to_term(send_and_recv(Ip, term_to_binary({c_upload_file, {}}))),
    NodesToSendTo = lists:split(max(2, length(ListOfNodes)), ListOfNodes),
    lists:foreach(
        fun(NodeIp) ->
            send_file(NodeIp, Filename)
        end
    , NodesToSendTo).

%make delete_file request to master node
-spec delete_file(Filename :: string()) -> no_return().
delete_file(Filename) ->
    Ip = get_ip_address(self()),
    {c_delete_file_response} = binary_to_term(send_and_recv(Ip, term_to_binary({c_delete_file, {Filename}}))).

%---------------------------------------------------------------------------------------------------- 
%---------------------------------------------------------------------------------------------------- 

%for general use
% send Data to Ip, then receive some RecvData from Ip
-spec send_and_recv(Ip :: inet:ip4_address(), Data :: binary()) -> binary().
send_and_recv(Ip, Data) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{
        family => inet,
        addr => Ip,
        port => 27000
    }),
    ok = socket:send(Socket, Data),
    ok = socket:shutdown(Socket, write),
    {ok, RecvData} = socket:recv(Socket),
    ok = socket:close(Socket),
    RecvData.

%for use by get_file
% try to get file from each node one by one
-spec attempt_get_file(Filename :: string(), ListOfNodesWithFile :: list(), Idx :: integer()) -> no_return().
attempt_get_file(Filename, ListOfNodesWithFile, Idx) ->
    if 
        Idx == length(ListOfNodesWithFile) + 1 ->
            error;
        true ->
            case request_and_accept_file(lists:nth(Idx, ListOfNodesWithFile), Filename) of
                error ->
                    attempt_get_file(Filename, ListOfNodesWithFile, Idx + 1);
                ok ->
                    ok
            end
    end.

% request file from Ip node, return ok if file obtained, error otherwise
-spec request_and_accept_file(Ip :: inet:ip4_address(), Filename :: string()) -> error | ok.
request_and_accept_file(Ip, Filename) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{
        family => inet,
        addr => Ip,
        port => 27000
    }),
    ok = socket:send(Socket, term_to_binary({c_get_file, {Filename}})),
    ok = socket:shutdown(Socket, write),
    {ok, Fd} = file:open("~/.erdfs/storage/" ++ Filename, [raw, write, binary]),
    accept_file(Socket, Fd),
    ok = socket:close(Socket),
    %check if transfer was successful and file has been created
    case file:open("~/.erdfs/storage/" ++ Filename, [read]) of
        {error, _} ->
            error;
        {ok, Fd} ->
            file:close(Fd),
            ok
    end.

%for use by request_and_accept_file
% accept data from Socket as long as it keeps coming, write it to the opened file
-spec accept_file(Socket :: socket:socket(), Fd :: file:fd()) -> no_return().
accept_file(Socket, Fd) ->
    case socket:recv(Socket) of
        {error, _} ->
            ok = file:close(Fd);
        {ok, Data} ->
            ok = file:write(Fd, Data),
            accept_file(Socket, Fd)
    end.

%for use by upload_file
% send signal to Ip, followed by the specified file
-spec send_file(Ip :: inet:ip4_address(), Filename :: string()) -> no_return().
send_file(Ip, Filename) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{
        family => inet,
        addr => Ip,
        port => 27000
    }),
    ok = socket:send(Socket, term_to_binary({c_upload_file, {Filename}})),
    {ok, Fd} = file:open("~/.erdfs/storage/" ++ Filename, [raw, read, binary]),
    _ = socket:sendfile(Socket, Fd),
    ok = file:close(Fd),
    ok = socket:close(Socket).