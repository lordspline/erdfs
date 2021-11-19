-module(c_node).
-export([]).

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
-spec init(M_Nodename :: string()).

%spawn ip provider process with obtained ip address
-spec spawn_ip_provider(Ip :: string()).

%ip provider process, provides ip address to other processes
-spec ip_provider().

%make get_file_list request to master node
-spec get_file_list().

%make get_file request to master node
-spec get_file(Filename :: string()).

%make upload_file request to master node
-spec upload_file(Filename :: string()).