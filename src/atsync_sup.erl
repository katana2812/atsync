%%% This is the root supervisor, from which other servers in fermium are
%%% started.
-module(atsync_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Initialization of the server, start other process.
init([]) ->
    %% params
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 120,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    % Create specifications for child process
    Child = {atsync, {atsync_event,  start, []}, permanent, brutal_kill, worker,  [atsync_event]},

    ChildSpecs = [Child],
    {ok, {SupFlags, ChildSpecs}}.
