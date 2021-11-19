-module(s_node).
-export([]).

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
-spec init(M_Nodename :: string()).

%spawn tcp server
-spec spawn_tcp_server().

%connection accepting loop for tcp server
-spec tcp_server().

%delete file
-spec delete_file(Filename :: string()).

%send file to socket
-spec send_file(Socket :: any(), Filename :: string()).

%accept file from socket (write mode, overwrite if exists)
-spec receive_file(Socket :: any(), Filename :: string()).