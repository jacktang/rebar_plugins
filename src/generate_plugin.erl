%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% rebar.config
%%%   {generate, [{skeletons, "deps/rebar_plugins/templates"},
%%%               {metadata, [ {authors, ["Jack Tang <jack@taodinet.com>"]}
%%%                            {copyright, "" },
%%%                            {license, ""}
%%%                          ]
%%%               }]
%%%    }
%%%
%%% ./rebar generate skeleton=otp.gen_server module=hello to=/tmp/
%%% ./rebar generate skeleton=otp.gen_fsm
%%% ./rebar generate skeleton=otp.start-dev to=./ebin/
%%% ./rebar generate skeleton=lib_module module=datetime_utils
%%% ./rebar generate skeleton=espec module=foo
%%% ./rebar generate skeleton=rebar.plugin module=test
%%% 
%%% @end
%%% Created :  5 Jan 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(generate_plugin).

%% API
-export([pre_generate/2, generate/2, do_generate/2]).

-define(DEFAULT_SKELETONS, "deps/rebar_plugins/generator/templates/").

%%%===================================================================
%%% API
%%%===================================================================
pre_generate(Config, _) ->
    Pa = rebar_config:get_global(Config, pa, []),
    Ebin = rebar_utils:ebin_dir(),
    NPa = [Ebin | Pa ],
    NConfig = rebar_config:set_global(Config, pa, NPa),
    {ok, NConfig}.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generate(Config, _Appfile) ->
    case processing_base_dir(Config) of % run rebar in cwd
        false -> ok;
        true ->
            case rebar_config:get_global(Config, skeleton, undefined) of
                undefined ->
                    show_help();
                Skeleton  ->
                    do_generate(Skeleton, Config)
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
do_generate(Skeleton, Config) ->
    GenerateConf = rebar_config:get(Config, generate, []),
    SkeletonsDir = proplists:get_value(skeletons, GenerateConf, ?DEFAULT_SKELETONS),
    
    TemplateFile = template_file(SkeletonsDir, Skeleton),
    OptionsFile  = options_file(SkeletonsDir, Skeleton),
    
    TemplateBin = load_file({template, TemplateFile}),
    Options = load_file({options, OptionsFile}),
    Metadata = proplists:get_value(metadata, GenerateConf, []),
    MMetadata = merge_meta(Metadata, Options),

    Context = build_context(Skeleton, Config, MMetadata),
    
    ReOpts = [global, {return, list}],
    TemplateStr = binary_to_list(TemplateBin),
    Str0 = re:replace(TemplateStr, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    Merged = mustache:render(Str1, Context),

    OutputFile = dest_file(Skeleton, Config, Options),
    case write_file(OutputFile, Merged, "1") of
        ok ->
            case proplists:get_value('$chmod', Options, undefined) of
                Mod when is_integer(Mod) ->
                    ok = file:change_mode(OutputFile, Mod);
                E ->
                    E % io:format("Mod is not integer: ~p~n", [E])
            end;
        Error ->
            Error
    end,
    post_generate(Options, Config).

build_context("otp.start-dev", Config, Metadata) ->
    Paths = rebar_config:get_global(Config, pa, []),
    MnesiaOpt = mnesia_opt(Metadata),
    EnvOpt    = env_opt(Metadata),
    PreLoadOpt = pre_load_opt(Metadata),
    Metadata2 = orddict:erase(env, orddict:erase(mnesia, Metadata)),
    
    Context = [  {pa_opt, pa_opt(Paths)},
               {mnesia_opt, MnesiaOpt},
               {pre_load_opt, PreLoadOpt},
               {env_opt, EnvOpt} 
               | orddict:to_list(Metadata2)],
    dict:from_list(Context);

build_context("lib", _Config, Metadata) ->
    {{Year, _M, _D} = Date, Time} = erlang:localtime(),
    
    Context = [{year, Year},
               {date, datetime_utils:datetime_as_string({Date, Time})}
               | orddict:to_list(Metadata) ],
    dict:from_list(Context);

build_context("espec", Config, Metadata) ->
    C = orddict:to_list(Metadata),
    Context = case rebar_config:get_global(Config, module, undefined) of
                  undefined -> C;
                  Module    -> [{module, Module ++ "_spec"} | C ]
               end,   
    dict:from_list(Context);

build_context(_Skeleton, Config, Metadata) ->
    C = orddict:to_list(Metadata),
    Context = case rebar_config:get_global(Config, module, undefined) of
                  undefined -> C;
                  Module    -> [{module, Module} | C ]
               end,
    dict:from_list(Context).

post_generate(Options, Config) ->
    case proplists:get_value('$post_generate', Options) of
        undefined -> ok;
        PActions ->
            lists:foreach(fun({F, A}) ->
                                  apply(?MODULE, F, [A, Config])
                          end, PActions)
    end.

pa_opt(Paths) ->
    string:join(["-pa " ++ Path || Path <- Paths], " ").
mnesia_opt(Metadata) ->
    case orddict:find(mnesia, Metadata) of
        {ok, MnesiaOpt} ->
            string:join(["-mnesia " ++ to_list(K) ++ " " ++ to_list(V) ||{K, V} <- MnesiaOpt], " ");
        error ->
            ""
    end.
env_opt(Metadata) ->
    case orddict:find(env, Metadata) of
        {ok, EnvOpt} ->
            string:join(["-env " ++ to_list(K) ++ " " ++ to_list(V) || {K, V} <- EnvOpt], " ");
        error ->
            ""
    end.
pre_load_opt(Metadata) ->
    case orddict:find(pre_load, Metadata) of
        {ok, PreLoads} ->
            lists:foldl(
              fun(PreLoad, Acc0) ->
                      case PreLoad of
                          {M, F} ->
                              Acc0 ++ " -s " ++ to_list(M) ++ " " ++ to_list(F);
                          M ->
                              Acc0 ++ " -s " ++ to_list(M)
                      end
              end, "", PreLoads);
        error ->
            ""
    end.

merge_meta(Metadata, Options) ->
    Meta2 = proplists:get_value(metadata, Options, []),
    M = orddict:from_list(Meta2),
    lists:foldl(
      fun({K, V}, Acc0) ->
              orddict:store(K, V, Acc0)
      end, M, Metadata).

template_file(SkeletonsDir, GenSkeleton) ->
    File = re:replace(GenSkeleton,"(\\.)+","\\/",[global, {return,list}]) ++ ".template",
    filename:join(SkeletonsDir, File).
            

options_file(SkeletonsDir, GenSkeleton) ->
    File = re:replace(GenSkeleton,"(\\.)+","\\/",[global, {return,list}]) ++ ".opt",
    filename:join(SkeletonsDir, File).

load_file({options, Path}) ->
    case filelib:is_regular(Path) of
        true ->
            {ok, Options} = file:consult(Path),
            Options;
        false ->
            []
    end;
load_file({template, Path}) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

dest_file("espec", Config, Options) ->
    DefaultDir = case proplists:get_value(dest_dir, Options, undefined) of
                     undefined ->
                         proplists:get_value(dir, rebar_config:get(Config, espec, []), "spec");
                     V ->
                         V
                 end,
    FileName = rebar_config:get_global(Config, module, "example") ++ "_spec",
    dest_file(DefaultDir, FileName, Config, Options);
dest_file("otp.start-dev", Config, Options) ->
    DefaultDir = case proplists:get_value(dest_dir, Options, undefined) of
                     undefined -> rebar_utils:get_cwd();
                     V -> V
                 end,
    dest_file(DefaultDir, "start-dev", Config, Options);

dest_file(_Skeleton, Config, Options) ->
    DefaultDir = case proplists:get_value(dest_dir, Options, undefined) of
                     undefined -> rebar_utils:get_cwd();
                     V -> V
                 end,
    FileName = rebar_config:get_global(Config, module, "example"),
    dest_file(DefaultDir, FileName, Config, Options).

dest_file(DefaultDir, FileName, Config, Options) ->
    DestDir = rebar_config:get_global(Config, to, DefaultDir),
    ok = filelib:ensure_dir(DestDir),
    case proplists:get_value('$suffix', Options, none) of
        none ->
            filename:join(DestDir, FileName);
        Suffix ->
            filename:join(DestDir, FileName ++ Suffix)
    end.

write_file(Output, Data, Force) ->
    % determine if the target file already exists
    FileExists = filelib:is_regular(Output),

    % perform the function if we're allowed,
    % otherwise just process the next template
    case Force =:= "1" orelse FileExists =:= false of
        true ->
            ok = filelib:ensure_dir(Output),
            case {Force, FileExists} of
                {"1", true} ->
                    io:format("Writing ~s (forcibly overwriting)~n",
                              [Output]);
                _ ->
                    io:format("Writing ~s~n", [Output])
            end,
            case file:write_file(Output, Data) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("Failed to write output file ~p: ~p\n",
                              [Output, Reason])
            end;
        false ->
            {error, exists}
    end.

show_help() ->
    io:format("Usage: ~n", []),
    ok.

to_list(Any) when is_list(Any) ->
    Any;
to_list(Any) when is_integer(Any)->
    integer_to_list(Any);
to_list(Any) when is_atom(Any) ->
    atom_to_list(Any);
to_list(Any) when is_binary(Any) ->
    binary_to_list(Any).
    
processing_base_dir(Config) ->
    rebar_utils:processing_base_dir(Config, filename:absname(rebar_utils:get_cwd())).
