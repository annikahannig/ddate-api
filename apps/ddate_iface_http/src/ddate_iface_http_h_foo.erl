-module(ddate_iface_http_h_foo).

-behaviour(ddate_iface_http_g_resource).

-export([start_link/0]).
-export([handle_request/3, handle_method/3]).

-define(SERVER, ?MODULE).


start_link() ->
    % Start server
    ddate_iface_http_g_resource:start_link({local, ?SERVER}, ?MODULE).


%%---------------------------------------------------------
%% Foo View
%%---------------------------------------------------------
handle_request(Req, Params, Opts) ->
    % Dispatch
    ddate_iface_http_g_resource:handle_request(?SERVER, Req, Params, Opts).


handle_method('GET', Req, Params) ->
    Today = ddate_cal:today(),
    {ok, Body} = ddate_tmpl_index:render([
        {today, maps:to_list(Today)}
    ]),
    {ok, {html, Body}};


handle_method('POST', Req, Params) ->
    {ok, {html, "Yaaay!"}}.
