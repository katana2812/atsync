%% Copyright (c) 2011 Rusty Klophaus
%% Released under the MIT License.

-module (atsync).
-behaviour(application).

%% API.
-export ([
    start/0,
    go/0,
    stop/0
]).

%% Application Callbacks.
-export([start/2, stop/1]).

-include_lib("kernel/include/file.hrl").
-define(SERVER, ?MODULE).

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

start() ->
    application:start(atsync).

go() ->
    application:start(atsync).

stop() ->
    application:stop(atsync).

%% ----------------------------------------------------------------------
%% Application Callbacks
%% ----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    io:format("Starting Sync (Automatic Code Compiler / Reloader)~n"),
    atsync_sup:start_link().

stop(_State) ->
    ok.
