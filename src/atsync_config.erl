%%% @doc
%%% This is an utility module that helps to access configuration files
%%% (under 'config/' directory in user's application) easily.
%%%
-module(atsync_config).

-compile(export_all).

-define(DEFAULT_CONFIG_BASE_NAME, "atsync").
-define(DEFAULT_CONFIG_DIRECTORY, "config").
-define(DEFAULT_CONFIG_EXT, ".config").

%% @doc
%% Return content of a configuration file with a specific name (ConfigName).
%%
%% @spec get(ConfigName::atom() | string()) ->
%%     ConfigContent::list()
get_all_contents() ->
    get_all_contents(?DEFAULT_CONFIG_DIRECTORY).

get_all_contents(Directory) ->
    FileName = filename:join([Directory, ?DEFAULT_CONFIG_BASE_NAME ++ ?DEFAULT_CONFIG_EXT]),
    case file:consult(FileName) of
        {ok, [Content]} -> Content;
        _               -> []
    end.

%% APIs for 'erlydb' configuration.

%% @doc
%% Return content of 'erlydb' configuration file.
%%
%% @spec get_erlydb() ->
%%     ConfigContent::list()
get_content(Key) -> get_content(Key, ?DEFAULT_CONFIG_DIRECTORY).

get_content(Key, Directory) ->
    proplists:get_value(wf:to_atom(Key), get_all_contents(Directory)).
