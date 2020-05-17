
-module(ddate_iface_http_router).

-export([
    match_route/2,
    match_path/2,
    parse_params/2,
    route_path/2]).


%%---------------------------------------------------------
%% Helper: is_variable
%%---------------------------------------------------------

is_variable(S) ->
    case S of
        ":" ++ _ -> true;
        _ -> false
    end.


variable(V) ->
    ":" ++ Vname = V,
    erlang:list_to_atom(Vname).


% Empty or root
compare_path_parts([], []) -> true;

compare_path_parts([], _) -> false; % No partial matches

compare_path_parts(_, []) -> false; % Can't match

compare_path_parts([X1|L1], [X2|L2]) ->
    case (X1 == X2) or is_variable(X1) of
        true -> compare_path_parts(L1, L2);
        _ -> false
    end.


match_path(Tmpl, Path) ->
    TmplParts = string:tokens(Tmpl, "/"),
    PathParts = string:tokens(Path, "/"),
    compare_path_parts(TmplParts, PathParts).


match_route(Route, Path) ->
    {Tmpl, _, _} = Route,
    match_path(Tmpl, Path).


%%---------------------------------------------------------
%% Parse Parameters
%%---------------------------------------------------------
parse_params_([], _) ->
    [nil];

parse_params_(_, []) ->
    [nil];

parse_params_([T|Tmpl], [P|Path]) ->
    case is_variable(T) of
        true -> [{variable(T), P}];
        _ -> [nil]
    end
      ++
    parse_params_(Tmpl, Path).


parse_params(Tmpl, Path) ->
    TmplParts = string:tokens(Tmpl, "/"),
    PathParts = string:tokens(Path, "/"),

    % Build map of parameters
    maps:from_list([
        P || P <- parse_params_(TmplParts, PathParts),
             P =/= nil
    ]).



%%---------------------------------------------------------
%% Select request handler
%%---------------------------------------------------------
route_path(Routing, Path) ->
    % Load route that matches. Should ideally be one,
    % however, could be multiple matches - first one wins.
    [{Tmpl, Handler, Opts}|_] = [
        Route || {Tmpl, _ , _} = Route <- Routing,
                 match_path(Tmpl, Path)],

    % Decode params from path
    Params = ddate_iface_http_router:parse_params(Tmpl, Path),
    {Handler, Params, Opts}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


match_path_test_() ->
    % Test path matching
    Expect = [
        {"/", "/", true},
        {"/", "/bar", false},
        {"/foo", "/bar", false},
        {"/foo/bar", "/foo", false},
        {"/foo/bar", "/foo/bar", true},
        {"/foo/:param", "/foo/42", true},
        {"/foo/:param", "/foo", false}
    ],

    [{"match " ++ Tmpl ++ " and " ++ Path,
     ?_assertEqual(Expected, match_path(Tmpl, Path))} ||
     {Tmpl, Path, Expected} <- Expect].


parse_params_test() ->
    Params = parse_params("/x/:a/:foo", "/x/str val/42"),
    ?assertEqual(#{a => "str val", foo => "42"}, Params).

-endif.


