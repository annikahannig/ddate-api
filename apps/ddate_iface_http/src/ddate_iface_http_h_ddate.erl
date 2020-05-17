
-module(ddate_iface_http_h_ddate).

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

handle_method('GET', _Req, #{year := Year, month := Month, day := Day}) ->
    % Convert params
    {Y, _} = string:to_integer(Year),
    {M, _} = string:to_integer(Month),
    {D, _} = string:to_integer(Day),
    Date = {Y, M, D},

    case calendar:valid_date(Date) of
        true ->
            DDate = ddate_cal:date_to_ddate(Date),
            success(Date, DDate);
        false ->
            {not_found, {text, "404 - date not found"}}
    end;


handle_method('GET', _Req, _Params) ->
    {Today, _} = erlang:localtime(),
    DDate = ddate_cal:date_to_ddate(Today),
    success(today, DDate).


success(Date, DDate) ->
    DateRepr = case Date of
        today -> today;
        {Y,M,D} ->
            lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D]))
    end,

    {ok, {template, ddate_tmpl_index, [
        {ddate, DDate},
        {date, DateRepr}
    ]}}.
