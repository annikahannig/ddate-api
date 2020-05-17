

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
    {"/bar", ddate_iface_http_h_foo, []},

    {"/api/v1/ddate",  ddate_iface_http_h_api_v1, []},

    {"/:year/:month/:day", ddate_iface_http_h_ddate, []},

    {"/", ddate_iface_http_h_ddate, []}
].



%%---------------------------------------------------------
%% Response Encoding
%%---------------------------------------------------------

encode_response({Status, {text, Text}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/plain"}],
    mochiweb_request:respond({Status, DefaultHeaders ++ Headers, Text}, Req);

encode_response({Status, {html, Body}, Headers}, Req) ->
    DefaultHeaders = [{"Content-type", "text/html"}],
    mochiweb_request:respond({Status, DefaultHeaders ++ Headers, Body}, Req);

encode_response({Status, {template, Tmpl, Args}}, Req) ->
    {ok, Body} = Tmpl:render(Args),
    encode_response({Status, {html, Body}}, Req);

encode_response({Status, Body, Headers}, Req) ->
    encode_response({Status, {html, Body}, Headers}, Req);

encode_response({ok, Body}, Req) ->
    encode_response({200, Body, []}, Req);

encode_response({not_found, Body}, Req) ->
    encode_response({404, Body, []}, Req);

encode_response({method_not_allowed, Method}, Req) ->
    Body = "The method '" ++ erlang:atom_to_list(Method) ++
        "' is not allowed.",
    encode_response({405, Body, []}, Req);

encode_response({error, Body}, Req) ->
    encode_response({500, Body, []}, Req).


%
% Handle Errors / Create error responses
%

handle_error({not_found, Path}, _Stacktrace) ->
    {not_found, {text, "The resource `" ++ Path ++ "` does not exist."}};

handle_error(Reason, Stacktrace) ->
    {error, {text, "something went wrong..."}}.
    

handle_request(Req) ->
    Method = mochiweb_request:get(method, Req),
    Path = mochiweb_request:get(path, Req),
    Query = mochiweb_request:parse_qs(Req),
    Peer = mochiweb_request:get(peer, Req),

    % Log Request
    lager:info("~s ~s ~p from ~s",[Method, Path, Query, Peer]),
    
    % Route request
    Response = try ddate_iface_http_router:route_path(routes(), Path) of
        {Handler, Params, Opts} ->
            % Pass request to handler, encode the response
            try Handler:handle_request(Req, Params, Opts)
            % Something happened while processing the response
            catch Class:Reason:Stacktrace -> 
                % Log stacktrace
                lager:error(
                    "~nStacktrace:~s",
                    [lager:pr_stacktrace(Stacktrace, {Class, Reason})]),
                % Generate response
                handle_error(Reason, Stacktrace)
            end
    % Routing the request failed.
    catch error:_ -> handle_error({not_found, Path}, [])
    end,

    % Respond to the request 
    encode_response(Response, Req).

            

%%---------------------------------------------------------
%% Tests
%%---------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
