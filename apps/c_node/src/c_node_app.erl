%%%-------------------------------------------------------------------
%% @doc c_node public API
%% @end
%%%-------------------------------------------------------------------

-module(c_node_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    c_node_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
