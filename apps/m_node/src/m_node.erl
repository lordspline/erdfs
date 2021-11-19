-module(m_node).
-export([]).

% spec : 
% 
% -------------------------------------------------------------------------------------------------
% 
% ets tables - 
%   1 - filename to s_nodes(at most 2, at least 1) (used while retreiving or deleting files)
%       {filename, {{ip, s_node_name} x 2}}
%   2 - s_node name to ip + space_left (used while uploading files)
%       {s_node_name, {ip, space_left}}
% 
% -------------------------------------------------------------------------------------------------
% 
% from s_node :
%   - s_register_node (node name + ip address + [files]) (s_node tells m_node to register it, along with all the files already present)
%   - s_received_file (node name + file name + size) (s_node tells m_node to update both tables as needed (add to 1, deduct space from 2))
%   - s_deleted_file (node name + file name + size) (s_node tells m_node to update both tables as needed (add to 1, deduct space from 2))
% 
% from c_node : 
%   - c_get_file_list (ip) (c_node requests list of all files)
%   - c_get_file (ip, file name) (send c_node ip addrs of all s_nodes that have the file)
%   - c_upload_file (ip, file_name, file_size) (send c_node ip addrs of all s_nodes that can store the file)
%   - c_delete_file (ip, file_name) (delete file from all s_nodes that have it)
% 
% -------------------------------------------------------------------------------------------------
% 
% spawn N tcp servers on port 27000
% pass both table names to each server
% 
% -------------------------------------------------------------------------------------------------

%init node
-spec init().

%create both tables, return their names
-spec create_tables().

%spawn N tcp servers, pass table names to each
-spec spawn_tcp_servers(N :: integer(), Table1 :: atom(), Table2 :: atom()).

%connection accepting loop for each tcp server
-spec tcp_server(Listener :: any(), Table1 :: atom(), Table2 :: atom()).

%update table
%this will have multiple forms, one for each possible table update event - 
%  1 - s_register_node  
%  2 - s_received_file 
%  3 - s_deleted_file 
-spec table_update(Reason :: atom(), Table1 :: atom(), Table2 :: atom(), InfoToUpdate :: any()). %this one will have multiple forms, one for each possible table update event

%read from table
%this will have multiple forms, one for each possible table read event - 
%  1 - c_get_file_list
%  2 - c_get_file
%  3 - c_upload_file
-spec table_read(Reason :: atom(), Table1 :: atom(), Table2 :: atom(), InfoToRead :: any()). %this one will have multiple forms, one for each possible table read event