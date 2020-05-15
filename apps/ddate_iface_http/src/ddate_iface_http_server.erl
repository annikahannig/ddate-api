

%%---------------------------------------------------------
%% @doc HTTP Server using mochiweb
%% @end
%%---------------------------------------------------------

-module(ddate_iface_http_server).


-export([start_link/0]).
-export([handle_request/1]).


-define(SERVER, ?MODULE).


start_link() ->
    HttpOpts = [
        {port, 8123},
        {loop, {?MODULE, handle_request}}
    ],
    lager:info("Starting HTTP service @ :8123"),
    mochiweb_http:start_link(HttpOpts).



routes() -> [
    {"/",
     ddate_iface_http_h_root, []},
    {"/api/v1/ddate",
     ddate_iface_http_h_api_v1_ddate, []},
    {"/hello/:username",
     ddate_iface_http_h_static, "about.html"}
].


encode_response({ok, {plain, Text}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/plain"}],
    mochiweb_request:respond({200, DefaultHeaders ++ Headers, Text}, Req);

encode_response({ok, {html, Body}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/html"}],
    mochiweb_request:respond({200, DefaultHeaders ++ Headers, Body}, Req);

encode_response({ok, Body, Headers}, Req) ->
    encode_response({ok, {html, Body}, Headers}, Req);

encode_response({ok, Body}, Req) ->
    encode_response({ok, Body, []}, Req).


handle_request(Req) ->
    Method = mochiweb_request:get(method, Req),
    Path = mochiweb_request:get(path, Req),
    Query = mochiweb_request:parse_qs(Req),
    Peer = mochiweb_request:get(peer, Req),

    % Log Request
    lager:info("~s ~s ~p from ~s",[Method, Path, Query, Peer]),

    % Route and dispatch request
    {Handler, Params, Opts} = ddate_iface_http_router:route_path(
        routes(), Path),
    Response = Handler:handle_request(Req, Params, Opts),

    % Encode response: Set content type, header, status code,
    % and so on dependent on the result.
    encode_response(Response, Req).


%%---------------------------------------------------------
%% Tests
%%---------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
