%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @author Slepher Chen <slepher.chen@taodinet.com>
%%% 
%%% @copyright (C) 2012, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 31 Dec 2012 by Jack Tang <jack@taodi.local>
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
%%
%% check espec in the dependencies
%%
%% rebar.config
%%    {espec_dir, "spec"}
%%
%% usage : rebar spec : test specs in spec dir
%%         rebar spec spec=test test test_spec.erl in spec dir
%%         rebar spec all=true test all spec include deps
%% 
%% @end
%%--------------------------------------------------------------------
pre_espec(Config, Appfile) ->
    % check espec in deps
    ok.

espec(Config, Appfile) ->
    rebar_deps:'check-deps'(Config, Appfile),
    case rebar_config:get_global(Config, all, undefined) of
        "true" ->
            case filelib:is_dir("spec") of
                true ->
                    Args = ["spec"],
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
                            Args = ["spec"],
                            case filelib:is_dir("spec") of
                                true ->
                                    espec_bin:run_spec_files_from_args(Args);
                                false ->
                                    ok
                            end;
                        Spec ->
                            Args = ["spec/" ++ Spec ++ "_spec"],
                            espec_bin:run_spec_files_from_args(Args)
                    end;
                false ->
                    ok
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
