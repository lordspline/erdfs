%%%-------------------------------------------------------------------
%% @doc s_node public API
%% @end
%%%-------------------------------------------------------------------

-module(s_node_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    s_node_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
