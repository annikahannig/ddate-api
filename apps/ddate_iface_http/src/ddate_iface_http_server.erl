

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
    {"/api/v1/ddate",
     ddate_iface_http_h_api_v1_ddate, []},
    {"/hello/:username",
     ddate_iface_http_h_static, "about.html"},
    {"/",
     ddate_iface_http_h_root, []}
].


encode_response({Status, {text, Text}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/plain"}],
    mochiweb_request:respond({Status, DefaultHeaders ++ Headers, Text}, Req);

encode_response({Status, {html, Body}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/html"}],
    mochiweb_request:respond({Status, DefaultHeaders ++ Headers, Body}, Req);

encode_response({Status, Body, Headers}, Req) ->
    encode_response({Status, {html, Body}, Headers}, Req);

encode_response({ok, Body}, Req) ->
    encode_response({200, Body, []}, Req);

encode_response({not_found, Body}, Req) ->
    encode_response({404, Body, []}, Req);

encode_response({error, Body}, Req) ->
    encode_response({500, Body, []}, Req).


%
% Handle Errors / Create error responses
%

handle_error({not_found, Path}) ->
    {not_found, {text, "The resource `" ++ Path ++ "` does not exist."}};

handle_error(Reason) ->
    lager:error("Something bad happened."),
    lager:error("Reason: ~p", [Reason]),
    {error, {text, "something went wrong..."}}.
    

handle_request(Req) ->
    Method = mochiweb_request:get(method, Req),
    Path = mochiweb_request:get(path, Req),
    Query = mochiweb_request:parse_qs(Req),
    Peer = mochiweb_request:get(peer, Req),

    % Log Request
    lager:info("~s ~s ~p from ~s",[Method, Path, Query, Peer]),

    Response = case ddate_iface_http_router:route_path(routes(), Path) of
        % Route found: 
        {Handler, Params, Opts} ->
            try Handler:handle_request(Req, Params, Opts)
            catch
                _:Reason ->
                    % Something went wrong wile handling this request.
                    % We pass this to our error handler and generate
                    % an error response
                    handle_error(Reason)
            end;
        % No route found
        _ ->
            handle_error({not_found, Path})
    end,

    % Encode response: Set content type, header, status code,
    % and so on dependent on the result.
    encode_response(Response, Req).


%%---------------------------------------------------------
%% Tests
%%---------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
