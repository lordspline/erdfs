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