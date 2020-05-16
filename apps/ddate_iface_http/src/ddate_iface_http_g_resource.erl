-module(ddate_iface_http_g_resource).

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2, handle_call/3]).
-export([start_link/2]).
-export([handle_request/4]).

-callback handle_method(atom(), term(), term()) -> term().


start_link(Server, Handler) ->
    gen_server:start_link(Server, ?MODULE, [Handler], []).


init([Handler]) -> {ok, [Handler]}.

%%---------------------------------------------------------
%% Gen Server request handler
%%---------------------------------------------------------
handle_call({request, Req, Params, _Opts}, _From, [Handler]) ->
    % Dispatch callback on handler
    Method = mochiweb_request:get(method, Req),
    Response = try Handler:handle_method(Method, Req, Params)
    catch error:function_clause -> {method_not_allowed, Method}
    end,
    % Respond to call
    {reply, Response, [Handler]}.


handle_cast(_, State) -> {noreply, State}.

%%---------------------------------------------------------
%% Request Handler interface function
%%---------------------------------------------------------
handle_request(Server, Req, Params, Opts) ->
    gen_server:call(Server, {request, Req, Params, Opts}).

