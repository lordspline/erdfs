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
%   - c_get_file (filename, ip) (send file to c_node)
%   - c_upload_file (filename) (accept file from c_node)
%
% -------------------------------------------------------------------------------------------------
% 
% register with master node (connect using provided nodename)
% spawn tcp listener (receives messages, files from m and c nodes, sends files)
% 
% -------------------------------------------------------------------------------------------------