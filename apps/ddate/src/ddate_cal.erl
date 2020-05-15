

%%---------------------------------------------------------
%% @doc ddate calendar service
%% @end
%%---------------------------------------------------------

-module(ddate_cal).

-behaviour(gen_server).

-export([start_link/0]).
-export([today/0, today/1, date_to_ddate/1, date_to_ddate/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-include_lib("ddate_cal.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init(_Args) ->
    {ok, []}.


%% Interface API
date_to_ddate(Date) ->
    date_to_ddate(Date, full).

date_to_ddate(Date, TextFormat) ->
    gen_server:call(?SERVER, {date_to_ddate, Date, TextFormat}).

today() ->
    today(full).

today(TextFormat) ->
    {Date, _} = erlang:localtime(),
    gen_server:call(?SERVER, {date_to_ddate, Date, TextFormat}).


%%---------------------------------------------------------
%% API Implementation
%%---------------------------------------------------------

%% DateToDdate 
handle_call({date_to_ddate, Date, TextFormat}, _From, State) ->
    Ddate = ddatee:date_to_ddate(Date),
    {Yold, _, Day} = Ddate,

    % Create ddate response 
    Response = #ddate{
        yold = Yold,
        season = ddatee:format(":season:", Ddate),
        day = Day,
        day_suffix = ddatee:format(":day_suffix:", Ddate),
        weekday = ddatee:format(":weekday:", Ddate),
        celebration = case ddatee:format(":celebration:", Ddate) of
            [] -> nil;
            Val -> Val
        end,
        text = ddatee:format(TextFormat, Ddate)},

    {reply, Response, State}.


handle_cast(_, State) -> {noreply, State}.

