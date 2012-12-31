%%%-------------------------------------------------------------------
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

%% usage : rebar spec : test specs in spec dir
%%         rebar spec spec=test test test_spec.erl in spec dir
%%         rebar spec all=true test all spec include deps
%%%===================================================================
%%% API
%%%===================================================================
espec(Config, Appfile) ->
    rebar_deps:'check-deps'(Config, Appfile),
    SpecDir = rebar_config:get(Config, spec_dir, "spec"),
    case rebar_config:get_global(Config, all, undefined) of
        "true" ->
            case filelib:is_dir(SpecDir) of
                true ->
                    Args = [SpecDir],
                    espec_bin:run_spec_files_from_args(Args),
                    ok;
                false ->
                    ok
            end;
        _ ->
            case rebar_utils:processing_base_dir(Config) of
                true ->
                    case rebar_config:get_global(Config, spec, undefined) of
                        undefined ->
                            Args = [SpecDir],
                            case filelib:is_dir(SpecDir) of
                                true ->
                                    espec_bin:run_spec_files_from_args(Args);
                                false ->
                                    ok
                            end;
                        Spec ->
                            Args = [SpecDir ++ "/" ++ Spec ++ "_spec"],
                            espec_bin:run_spec_files_from_args(Args)
                    end;
                false ->
                    ok
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
