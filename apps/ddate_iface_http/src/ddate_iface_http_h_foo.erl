-module(ddate_iface_http_h_foo).

-behaviour(ddate_iface_http_g_resource).

-export([start_link/0]).
-export([handle_request/3, handle_method/3]).

-define(SERVER, ?MODULE).


start_link() ->
    ddate_iface_http_g_resource:start_link({local, ?SERVER}, ?MODULE).


%%---------------------------------------------------------
%% Foo View
%%---------------------------------------------------------
handle_request(Req, Params, Opts) ->
    % Dispatch
    ddate_iface_http_g_resource:handle_request(?SERVER, Req, Params, Opts).


handle_method('GET', Req, Params) ->
    {ok, {html, "<b>ohai!</b> nice to see you!</b>"}};


handle_method('POST', Req, Params) ->
    {ok, {html, "Yaaay!"}}.
