%%%-------------------------------------------------------------------
%% @doc ddate public API
%% @end
%%%-------------------------------------------------------------------

-module(ddate_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ddate_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
