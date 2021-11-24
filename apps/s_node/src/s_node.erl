-module(s_node).
-export([init/1, tcp_server/2]).

% spec : 
% 
% -------------------------------------------------------------------------------------------------
% 
% from m_node : 
%   - m_delete_file (filename) (delete file, then send back confirmation)
% 
% from c_node : 
%   - c_get_file (filename) (send file to c_node)
%   - c_upload_file (filename) (accept file from c_node)
%
% -------------------------------------------------------------------------------------------------
% 
% register with master node (connect using provided nodename)
% spawn tcp listener (receives messages, files from m and c nodes, sends files)
% 
% -------------------------------------------------------------------------------------------------

%init node
-spec init(M_Nodename :: string()) -> no_return().
init(M_Nodename) ->
    ok = net_kernel:start([s_node, shortnames]),
    true = erlang:set_cookie(erdfs),
    true = net_kernel:connect_node(list_to_atom(M_Nodename)),
    spawn_tcp_server(list_to_atom(M_Nodename)),
    ok.

%spawn tcp server
-spec spawn_tcp_server(M_Nodename :: atom()) -> no_return().
spawn_tcp_server(M_Nodename) ->
    %get ip addr of m_node
    {ok, [{M_Ip, _, _}, _]} = rpc:call(list_to_atom(M_Nodename), inet, getif, []),
    %create listener
    {ok, Listener} = socket:open(inet, stream, tcp),
    ok = socket:bind(Listener, #{
        family => inet,
        port => 27000,
        addr => any
    }),
    ok = socket:listen(Listener),
    %spawn
    spawn(?MODULE, tcp_server, [Listener, M_Ip]).

%connection accepting loop for tcp server
-spec tcp_server(Listener :: socket:socket(), M_Ip :: inet:ip4_address()) -> no_return().
tcp_server(Listener, M_Ip) ->
    {ok, Socket} = socket:accept(Listener),
    {ok, Data} = socket:recv(Socket),
    case binary_to_term(Data) of 
        {m_delete_file, {Filename}} ->
            delete_file(M_Ip, Filename);
        {c_get_file, {Filename}} ->
            send_file(Socket, Filename);
        {c_upload_file, {Filename}} ->
            receive_file(Socket, Filename)
    end,
    ok = socket:close(Socket),
    tcp_server(Listener, M_Ip).

%delete file
-spec delete_file(M_Ip :: inet:ip4_address(), Filename :: string()) -> no_return().
delete_file(M_Ip, Filename) ->
    %store file info to get file size
    {ok, FileInfo} = file:read_file_info("~/.erdfs/storage/" ++ Filename),
    ok = file:delete("~/.erdfs/storage/" ++ Filename),
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{
        family => inet,
        addr => M_Ip,
        port => 27000
    }),
    {ok, [{SelfIp, _, _}, _]} = inet:getif(),
    %fileinfo element 2 is file size
    ok = socket:send(Socket, term_to_binary({s_deleted_file, {SelfIp, Filename, element(2, FileInfo)}})),
    ok = socket:close(Socket).

%send file to socket
-spec send_file(Socket :: socket:socket(), Filename :: string())-> no_return().
send_file(Socket, Filename) ->
    {ok, Fd} = file:open("~/.erdfs/storage/" ++ Filename, [raw, read, binary]),
    _ = socket:sendfile(Socket, Fd),
    ok = file:close(Fd),
    ok = socket:close(Fd).

%accept file from socket (write mode, overwrite if exists)
-spec receive_file(Socket :: socket:socket(), Filename :: string())-> no_return().
receive_file(Socket, Filename) ->
    {ok, Fd} = file:open("~/.erdfs/storage/" ++ Filename, [raw, write, binary]),
    accept_file(Socket, Fd).

%for use by receive_file
-spec accept_file(Socket :: socket:socket(), Fd :: file:fd()) -> no_return().
accept_file(Socket, Fd) ->
    case socket:recv(Socket) of 
        {error, _} ->
            ok = file:close(Fd);
        {ok, Data} ->
            ok = file:write(Fd, Data),
            accept_file(Socket, Fd)
    end.