%%
%%
-module(atsync_event).

-behaviour(gen_event).

%% API
-export([start/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @doc .
%% @spec start() -> {ok, Pid} | {error, Reason}
start() ->
    Dirs = atsync_config:get_content(dirs),
    SrcDirs = lists:map(fun(Dir) -> Dir ++ "/src" end, Dirs),
    Res = inotifywait:start_event_manager({local, ?SERVER}, SrcDirs, [
        recursive, {events, [modify, moved_to]}, {exclude, "'goutputstream|lock'"}]),
    gen_event:add_handler(?SERVER, ?MODULE, []),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%------------------------------------------------------------------------------
init([]) ->
    {ok, undefined}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%------------------------------------------------------------------------------
handle_event({inotify,_Event,false,FileName,Dir}, State) ->
    compile(FileName,Dir),
    {ok, State};

handle_event(_Data, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%%------------------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%------------------------------------------------------------------------------
%% Function:
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_app_dir(Dir) ->
    SrcIndex = string:rstr(Dir, "src"),
    string:sub_string(Dir, 1, SrcIndex-1).

compile(FileName,Dir) ->
    Module = lists:nth(1, string:tokens(FileName, ".")),

    AppDir = find_app_dir(Dir),

    IncludeDir = AppDir ++ "/include/",

    case c:c(io_lib:format("~s/~s", [Dir, Module]),
        [debug_info, verbose,report_errors,report_warnings, {i, IncludeDir}]) of
        error ->
            io:format("Error: ~s/~s~n", [Dir, Module]),
            error;

        _Other ->
            io:format("Recompile: ~s/~s~n", [Dir, Module]),

            BeamFile = Module ++ ".beam",

            SrcFile = io_lib:format("./~s", [BeamFile]),
            DstFile = io_lib:format("~sebin/~s", [AppDir, BeamFile]),
            file:copy(SrcFile, DstFile),
            file:delete(SrcFile),
            {ok, Module}
    end.
