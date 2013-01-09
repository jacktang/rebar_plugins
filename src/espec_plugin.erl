%%%-------------------------------------------------------------------
%%% @author Slepher Chen <slepher.chen@taodinet.com>
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 31 Dec 2012 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(espec_plugin).

%% API
-export([espec/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% usage : rebar espec : test all specs in spec dir
%%         rebar espec spec=test test test_spec.erl in spec dir
%% 
%%%===================================================================
%%% API
%%%===================================================================
espec(Config, Appfile) ->
    rebar_deps:'check-deps'(Config, Appfile),
    ESpecConf = rebar_config:get(Config, espec, []),
    SpecDir = proplists:get_value(dir, ESpecConf, "spec"),
    EbinPath = rebar_utils:ebin_dir(),
    true = code:add_patha(EbinPath),

    case rebar_utils:processing_base_dir(Config) of
        false -> ok;
        true ->
            case rebar_config:get_global(Config, spec, undefined) of
                undefined -> %% test all specs under SpecDir
                    case filelib:is_dir(SpecDir) of
                        true ->
                            espec_bin:run_spec_files_from_args([SpecDir]),
                            ok;
                        false ->
                            ok
                    end;
                Spec -> % run specified spec
                    SpecFile = filename:join(SpecDir, Spec),
                    espec_bin:run_spec_files_from_args([SpecFile]),
                    ok
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
