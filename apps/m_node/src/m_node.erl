-module(m_node).
-export([init/0, tcp_server/1]).

% spec : 
% 
% -------------------------------------------------------------------------------------------------
% 
% ets tables - 
%   1 - filename to s_nodes(at least 1) (used while retreiving or deleting files)
%       {filename, [{ip, s_node_name} x n]}
%   2 - s_node name to ip + space_used (used while uploading files)
%       {s_node_name, {ip, space_used}}
% 
% -------------------------------------------------------------------------------------------------
% 
% from s_node :
%   - s_register_node (node name + ip address + [files]) (s_node tells m_node to register it, along with all the files already present)
%   - s_received_file (node name + file name + size) (s_node tells m_node to update both tables as needed (add to 1, deduct space from 2))
%   - s_deleted_file (node name + file name + size) (s_node tells m_node to update both tables as needed (add to 1, deduct space from 2))
% 
% from c_node : 
%   - c_get_file_list (socket) (c_node requests list of all files)
%   - c_get_file (socket, file name) (send c_node ip addrs of all s_nodes that have the file)
%   - c_upload_file (socket, file_name, file_size) (send c_node ip addrs of all s_nodes that can store the file)
%   - c_delete_file (socket, file_name) (delete file from all s_nodes that have it)
% 
% -------------------------------------------------------------------------------------------------
% 
% spawn N tcp servers on port 27000
% pass both table names to each server
% 
% -------------------------------------------------------------------------------------------------

%init node
-spec init() -> any().
init() ->
    {ok, _} = net_kernel:start(m_node, [shortnames]),
    ok = io:format("M Node successfully started with name : ~p~n", [node()]),
    ok = create_tables(),
    ok = spawn_tcp_servers(10).

%create both tables, return their tids
-spec create_tables() -> ok | no_return().
create_tables() ->
    _ = ets:new(table1, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    _ = ets:new(table2, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    ok.

-spec create_tcp_listener() -> no_return() | socket:socket().
create_tcp_listener() ->
    {ok, Listener} = socket:open(inet, stream, tcp),
    {ok, _} = socket:bind(Listener, #{
        family => inet,
        port => 27000,
        addr => any
    }),
    ok = socket:listen(Listener),
    Listener.

%spawn N tcp servers, pass table names to each
-spec spawn_tcp_servers(N :: integer()) -> any().
-spec spawn_tcp_servers(N :: integer(), Listener :: any()) -> any().
spawn_tcp_servers(N) ->
    Listener = create_tcp_listener(),
    spawn_tcp_servers(N, Listener).

spawn_tcp_servers(0, _) ->
    ok;
spawn_tcp_servers(N, Listener) ->
    _ = spawn(?MODULE, tcp_server, [Listener]),
    spawn_tcp_servers(N - 1, Listener).

%connection accepting loop for each tcp server
-spec tcp_server(Listener :: any()) -> any().
tcp_server(Listener) ->
    {ok, Socket} = socket:accept(Listener),
    {ok, Data} = socket:recv(Socket),
    case binary_to_term(Data) of 
        {s_register_node, {FromName, FromIp, Files, SpaceUsed}} ->
            s_register_node(FromName, FromIp, Files, SpaceUsed);
        {s_received_file, {FromName, FileName, FileSize}} ->
            s_received_file(FromName, FileName, FileSize);
        {s_deleted_file, {FromName, FileName, FileSize}} ->
            s_deleted_file(FromName, FileName, FileSize);
        {c_get_file_list, {}} ->
            c_get_file_list(Socket);
        {c_get_file, {FileName}} ->
            c_get_file(Socket, FileName);
        {c_upload_file, {}} ->
            c_upload_file(Socket);
        {c_delete_file, {FileName}} ->
            c_delete_file(FileName);
        _ ->
            ok = io:format("Received invalid Data~n")
    end,
    ok = socket:close(Socket),
    tcp_server(Listener).

%register new s_node, update both tables 
% node may already contain some files, so table1 has to be checked first
% if node contains a file already present in table1, it has to be added to list of nodes for that file
% if node contains a file not present in table1, create new entry for that file
-spec s_register_node(FromName :: atom(), FromIp :: string(), Files :: list(string()), SpaceUsed :: integer()) -> no_return().
s_register_node(FromName, FromIp, Files, SpaceUsed) ->
    %updating table1
    lists:foreach(
        fun(FileName) ->
            case ets:member(table1, FileName) of
                true ->
                    {_, NodesAlreadyHoldingFile}= lists:nth(1, ets:lookup(table1, FileName)),
                    true = ets:insert(table1, {FileName, [{FromIp, FromName} | NodesAlreadyHoldingFile]});
                false ->
                    true = ets:insert(table1, {FileName, [{FromIp, FromName}]})
            end
        end
    , Files),
    %updating table2
    true = ets:insert(table2, {FromName, {FromIp, SpaceUsed}}).

%s_node has received a new file, update both tables
-spec s_received_file(FromName :: atom(), FileName :: string(), FileSize :: integer()) -> no_return().
s_received_file(FromName, FileName, FileSize) ->
    %get ip of s_node from table2
    {_, {FromIp, SpaceUsed}} = lists:nth(1, ets:lookup(table2, FromName)),
    %update table1
    case ets:member(table1, FileName) of
        true ->
            {_, NodesAlreadyHoldingFile}= lists:nth(1, ets:lookup(table1, FileName)),
            true = ets:insert(table1, {FileName, [{FromIp, FromName} | NodesAlreadyHoldingFile]});
        false ->
            true = ets:insert(table1, {FileName, [{FromIp, FromName}]})
    end,
    %update table2
    true = ets:insert(table2, {FromName, {FromIp, SpaceUsed + FileSize}}).


%s_node has deleted a file, update both tables
% remove s_node from this file's entry in table1
% update s_node's SpaceUsed in table2
-spec s_deleted_file(FromName :: atom(), FileName :: string(), FileSize :: integer()) -> no_return().
s_deleted_file(FromName, FileName, FileSize) ->
    %get ip of s_node from table2
    {_, {FromIp, SpaceUsed}} = lists:nth(1, ets:lookup(table2, FromName)),
    %update table1
    {_, NodesHoldingFile}= lists:nth(1, ets:lookup(table1, FileName)),
    NodesHoldingFileUpdated = lists:delete({FromIp, FromName}, NodesHoldingFile),
    true = ets:insert(table1, {FileName, NodesHoldingFileUpdated}),
    %update table2
    true = ets:insert(table2, {FromName, {FromIp, SpaceUsed - FileSize}}).

%get list of files from table1 and send it to the c_node
-spec c_get_file_list(Socket :: socket:socket()) -> no_return().
c_get_file_list(Socket) ->
    ListOfFiles = [L || {L, _} <- ets:tab2list(table1)],
    ok = socket:send(Socket, term_to_binary({c_get_file_list_response, ListOfFiles})).

%get list of all nodes holding the file from table1 and send it to the c_node
-spec c_get_file(Socket :: socket:socket(), FileName :: string()) -> no_return().
c_get_file(Socket, FileName) ->
    {_, NodesHoldingFile} = lists:nth(1, ets:lookup(table1, FileName)),
    ok = socket:send(Socket, term_to_binary({c_get_file_response, NodesHoldingFile})).

%todo
-spec c_upload_file(Socket :: socket:socket()) -> no_return().
c_upload_file(Socket) ->
    ListOfNodes = [L || {_, L} <- ets:tab2list(table2)],
    ListOfNodesSorted = qsort_nodes(ListOfNodes),
    {ListOfNodesToSend, _} = lists:split(max(2, length(ListOfNodesSorted)), ListOfNodesSorted),
    ok = socket:send(Socket, term_to_binary(ListOfNodesToSend)).

%for use by c_upload_file
% quick sort list of pairs {ip, space_used} by space_used
-spec qsort_nodes(L :: list()) -> list().
qsort_nodes([]) ->
    [];
qsort_nodes([Pivot | Tail]) ->
    qsort_nodes([X || X <- Tail, element(2, X) < element(2, Pivot)]) ++ [Pivot] ++ qsort_nodes([X || X <- Tail, element(2, X) >= element(2, Pivot)]).

%todo
-spec c_delete_file(FileName :: string()) -> no_return().
c_delete_file(FileName) ->
    {_, NodesHoldingFile} = lists:nth(1, ets:lookup(table1, FileName)),
    lists:foreach(
        fun({Ip, _}) ->
            {ok, Socket} = socket:open(inet, stream, tcp),
            {ok, IpAddr} = inet:parse_address(Ip),
            ok = socket:connect(Socket, #{
                family => inet,
                addr => IpAddr,
                port => 27000
            }),
            ok = socket:send(Socket, term_to_binary({m_delete_file, {FileName}})),
            ok = socket:shutdown(Socket, read_write),
            ok = socket:close(Socket)
        end
    , NodesHoldingFile).