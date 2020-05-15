%%%-------------------------------------------------------------------
%% @doc ddate_iface_http public API
%% @end
%%%-------------------------------------------------------------------

-module(ddate_iface_http_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ddate_iface_http_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
