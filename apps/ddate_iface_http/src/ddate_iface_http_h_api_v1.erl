
-module(ddate_iface_http_h_api_v1).

-behaviour(ddate_iface_http_g_resource).

-export([start_link/0]).
-export([handle_request/3, handle_method/3]).

-define(SERVER, ?MODULE).

start_link() ->
    % Compile templates
    {ok, _} = erlydtl:compile_file(
        code:priv_dir(ddate_iface_http) ++
        "/templates/index.html", ddate_tmpl_index),

    ddate_iface_http_g_resource:start_link({local, ?SERVER}, ?MODULE).

handle_request(Req, Params, Opts) ->
    % Dispatch
    ddate_iface_http_g_resource:handle_request(?SERVER, Req, Params, Opts).


%%---------------------------------------------------------
% DDate View 
%%---------------------------------------------------------


handle_method('GET', Req, _Params) ->
    % Options
    Options = mochiweb_request:parse_qs(Req),

    {Today, _} = erlang:localtime(),
    DDate = ddate_cal:date_to_ddate(Today),

    {ok, {json, DDate}}.
