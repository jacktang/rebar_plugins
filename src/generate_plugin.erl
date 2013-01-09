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
%%% ./rebar generate skeleton=lib module=datetime_utils
%%% ./rebar generate skeleton=espec module=foo
%%% ./rebar generate skeleton=scripts.start-dev to=./ebin/
%%% ./rebar generate skeleton=rebar.plugin module=test
%%% 
%%% @end
%%% Created :  5 Jan 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(generate_plugin).

%% API
-export([pre_generate/2, generate/2]).

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
    case rebar_utils:processing_base_dir(Config) of % run rebar in cwd
        false -> ok;
        true ->
            GenerateConf = rebar_config:get(Config, generate, []),
            SkeletonsDir = proplists:get_value(skeletons, GenerateConf, ?DEFAULT_SKELETONS),
            
            case rebar_config:get_global(Config, skeleton, undefined) of
                undefined ->
                    show_help();
                Skeleton  ->
                    Paths = rebar_config:get_global(Config, pa, []),
                    
                    TemplateFile = template_file(SkeletonsDir, Skeleton),
                    OptionsFile  = options_file(SkeletonsDir, Skeleton),
                   
                    TemplateBin = load_file({template, TemplateFile}),
                    Options = load_file({options, OptionsFile}),
                    Metadata  = proplists:get_value(metadata, GenerateConf, []),
                    Metadata2 = merge_meta(Metadata, Options),

                    MnesiaOpt = mnesia_opt(Metadata2),
                    EnvOpt    = env_opt(Metadata2),
                    PreLoadOpt = pre_load_opt(Metadata2),
                    
                    Metadata3 = orddict:erase(env, orddict:erase(mnesia, Metadata2)),

                    Context = [{date, "Jan 7 2013"},
                               {pa_opt, pa_opt(Paths)},
                               {mnesia_opt, MnesiaOpt},
                               {pre_load_opt, PreLoadOpt},
                               {env_opt, EnvOpt} 
                               | orddict:to_list(Metadata3)],

                    Context2 = case rebar_config:get_global(Config, module, undefined) of
                                   undefined -> Context;
                                   Module ->
                                       M = case Skeleton of
                                                 "espec" -> Module ++ "_spec";
                                                 _ -> Module
                                             end,
                                       [{module, M} | Context ]
                               end,
                    
                    ReOpts = [global, {return, list}],
                    TemplateStr = binary_to_list(TemplateBin),
                    Str0 = re:replace(TemplateStr, "\\\\", "\\\\\\", ReOpts),
                    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
                    Merged = mustache:render(Str1, dict:from_list(Context2)),

                    OutputFile = dest_file(TemplateFile, Config, Options),
                    case write_file(OutputFile, Merged, "1") of
                        ok ->
                            case proplists:get_value('$chmod', Options, undefined) of
                                Mod when is_integer(Mod) ->
                                    ok = file:change_mode(OutputFile, Mod);
                                E ->
                                    io:format("Mod is not integer: ~p~n", [E])
                            end;
                        Error ->
                            Error
                    end
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_lib() ->
    ok.

generate_espec() ->
    ok.

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

dest_file(TmplFilePath, Config, Options) ->
    Skeleton = rebar_config:get_global(Config, skeleton, undefined),
    DefaultDir = case Skeleton of
                     "espec" ->
                         proplists:get_value(dir, rebar_config:get(Config, espec, []), "spec");
                     _ ->
                         rebar_utils:get_cwd()
                 end,
    DestDir = rebar_config:get_global(Config, to, DefaultDir),
    ok = filelib:ensure_dir(DestDir),
    
    FileName = case rebar_config:get_global(Config, module, undefined) of
                   undefined ->
                       filename:rootname(filename:basename(TmplFilePath));
                   Module ->
                       Module
               end,
    FileName2 = case Skeleton of
                    "espec" -> FileName ++ "_spec";
                    _ -> FileName
                end,
    
    case proplists:get_value('$suffix', Options, none) of
        none ->
            filename:join(DestDir, FileName2);
        Suffix ->
            filename:join(DestDir, FileName2 ++ Suffix)
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
    
