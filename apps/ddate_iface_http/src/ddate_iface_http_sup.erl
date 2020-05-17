%%%-------------------------------------------------------------------
%% @doc ddate_iface_http top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ddate_iface_http_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 1},
    ChildSpecs = [
        #{id => ddate_iface_http_server,
          start => {ddate_iface_http_server, start_link, []},
          type => worker,
          modules => [ddate_iface_http_server]},
        %
        % Handlers
        %
        #{id => ddate_iface_http_h_ddate,
          start => {ddate_iface_http_h_ddate, start_link, []},
          type => worker,
          modules => [ddate_iface_http_h_ddate]},

        #{id => ddate_iface_http_h_api_v1,
          start => {ddate_iface_http_h_api_v1, start_link, []},
          type => worker,
          modules => [ddate_iface_http_h_api_v1]}

    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
