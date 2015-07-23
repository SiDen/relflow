-module('relflow').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'relflow').
-define(DEPS, [app_discovery]).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).
-define(DEFAULT_INITIAL_VERSION, "0.1.0").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 relflow"}, % How to use the plugin
            {opts, opts()},                   % list of options understood by the plugin
            {short_desc, "release workflow util"},
            {desc, desc()}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

desc() -> "
Relflow
=======

Examples:
    rebar3 relflow -u v1.2.3        # upgrade from last release, at git tag v1.2.3
    rebar3 relflow init-versions    # reset all vsns using relflow format
    rebar3 relflow --version        # print relflow version

".

opts() ->
    [
     {upfrom, $u, "upfrom", string,
      "Git revision/tag to upgrade from (for appup generation)"},
     {nextver, $x, "nextversion", {string, "auto"},
      "The version string to use for the next release"},
     {include_untracked, $i, "include-untracked", {boolean, false},
      "Include untracked files"},
     {autogit, $g, "autogit", {boolean, true},
      "Automatically add and commit relflow changes to git"},
     {force, $f, "force", {boolean, false},
      "Force relflow to run even with uncommitted local changes"},
     {check, $c, "check", {boolean, false},
      "Check only"},
     {version, $v, "version", undefined,
      "Print relflow version and exit"}
    ].

relflow_version() ->
    {ok, V} = application:get_key(relflow, vsn),
    V.

do(RebarState) ->
    State = relflow_state:new(RebarState),

    case relflow_state:version(State) of
        undefined ->
            do_0(State);
        _ ->
            io:format("~s\n",[relflow_version()]),
            {ok, RebarState}
    end.

do_0(State) ->
    case assemble_state(State) of
        {error, Error} -> Error;
        NewState ->
            case relflow_state:task(NewState) of
                undefined ->
                    do_1(NewState);
                "init-versions" ->
                    do_init_versions(NewState)
            end
    end.

do_1(State) ->
    case relflow_state:upfrom(State) of
        undefined ->
            ?PRV_ERROR(no_upfrom);
        _ ->
            do_2(State)
    end.

do_2(State) ->
    rebar_api:debug("relflow upgrading from release ~s to ~s",[
                        relflow_state:oldrelver(State),
                        relflow_state:nextrelver(State)]),
    ChangesSinceRev = relflow_git:since(relflow_state:upfrom(State),
                                        relflow_state:include_untracked(State)),
    ChangeMap       = relflow_appup:generate_appups(ChangesSinceRev, State),
    exec(ChangeMap, State, fun(A,B) -> exec_1(A,B,fun exec_2/2) end).

assemble_state(State) ->
    lists:foldl(fun
        (_, {error, _} = Error) -> Error;
        (F, Acc) ->
            F(Acc)
        end, State, [
            fun(S) ->
                case relflow_state:upfrom(S) of
                    undefined -> S;
                    Rev ->
                        OldRelVer = relflow_git:relver_at(Rev),
                        relflow_state:oldrelver(OldRelVer, S)
                end
            end,
            fun(S) ->
                case relflow_state:oldrelver(S) of
                    undefined -> S;
                    OldRelVer ->
                        case relflow_vsn:check(OldRelVer) of
                            ok -> S;
                            error ->
                                {error, ?PRV_ERROR({relvsn_parse, OldRelVer})}
                        end
                end
            end,
            fun(S) ->
                CurVer = case relflow_state:include_untracked(S) of
                    true -> relflow_git:relver_from_disk();
                    false -> relflow_git:relver_at("HEAD")
                end,
                relflow_state:currelver(CurVer, S)
            end,
            fun(S) ->
                NextVer = case relflow_state:nextver(S) of
                    "auto" ->
                        OldRelVer = relflow_state:oldrelver(S),
                        CurRelVer = relflow_state:currelver(S),
                        case {relflow_vsn:parse(OldRelVer),
                                relflow_vsn:parse(CurRelVer)} of
                            {error, error} -> ?DEFAULT_INITIAL_VERSION;
                            {error, _} -> CurRelVer;
                            {{ok, OV}, error} ->
                                relflow_vsn:serialize(relflow_vsn:inc(OV));
                            {{ok, OV}, {ok, CV}} ->
                                case relflow_vsn:gt(CV, OV) of
                                true -> CurRelVer;
                                false ->
                                    relflow_vsn:serialize(relflow_vsn:inc(OV))
                                end
                        end;
                    NV ->
                        case relflow_vsn:check(NV) of
                            ok -> NV;
                            error -> {error, ?PRV_ERROR({relvsn_parse, NV})}
                        end
                end,
                case NextVer of
                    {error, _} = Error -> Error;
                    _ -> relflow_state:nextrelver(NextVer, S)
                end
            end
        ]).

do_init_versions(State0) ->
    NextVer = relflow_state:nextrelver(State0),
    rebar_api:info("New release vsn: ~s", [NextVer]),
    BumperFun = fun(_Map, State) ->
        Apps = [ {rebar_app_info:name(A), rebar_app_info:app_file_src(A)}
             || A <- rebar_state:project_apps(relflow_state:rebar_state(State0))
        ],
        lists:foreach(fun({_AppName, AppFile}) ->
            ok = relflow_rewriter:set_appfile_version(AppFile, NextVer)
        end, Apps),
        rebar_api:info("Setting release vsn in rebar.config to ~s", [NextVer]),
        case relflow_rewriter:set_rebar_relx_version("rebar.config", NextVer) of
            {error, ErrReason} ->
                ?PRV_ERROR(ErrReason);
            _ ->
                rebar_api:info("You are advised to commit and tag this as '~s'",[NextVer]),
                {ok, relflow_state:rebar_state(State)}
        end
    end,
    Changes = #{},
    exec(Changes, State0, fun(A,B) -> exec_1(A,B,BumperFun) end).

format_error(parse_rebar_config) ->
    "Can not parse rebar.config";
format_error(unclean_git) ->
    "Relflow modifies files in-place. Will not run with uncommitted changes.";
format_error(no_upfrom) ->
    "Missing git revision to upgrade from, eg: rebar3 relflow -u abc123\n     " ++
    "(or try: rebar3 help relflow)";
format_error({relvsn_ordering, Old, New}) ->
    io_lib:format("New release vsn is less than old! (new:~s < old:~s)", [New, Old]);
format_error({relvsn_parse, Vsn}) ->
    io_lib:format("Can not parse version: ~p", [Vsn]);
format_error(check_failed) ->
    "Check FAILED";
format_error(Reason) ->
    io_lib:format("Unhandled relflow error: ~p", [Reason]).


exec(Map, State, Next) ->
    case relflow_state:force(State)
            orelse relflow_git:is_clean()
            orelse relflow_state:include_untracked(State) of
        true -> Next(Map, State);
        false -> ?PRV_ERROR(unclean_git)
    end.

exec_1(Map, State, Next) ->
    OldRelVsn = relflow_state:oldrelver(State),
    NewRelVsn = relflow_state:nextrelver(State),
    Continue = case {relflow_vsn:parse(NewRelVsn),
            relflow_vsn:parse(OldRelVsn)} of
        {error, _} -> false;
        {_, error} -> true;
        {{ok, V1}, {ok, V2}} -> relflow_vsn:gt(V1, V2)
    end,
    case Continue of
        false ->
            ?PRV_ERROR({relvsn_ordering, OldRelVsn, NewRelVsn});
        true ->
            Next(Map, State)
    end.

exec_2(Map, State) ->
    NewMap = rewrite_appups(Map, State),
    ChangedApps = maps:fold(fun(AppName, M, Acc) ->
            Op = case maps:find(op, M) of
                {ok, V} -> V;
                error -> added
            end,
            [{AppName, Op} | Acc]
        end, [], NewMap),
    RebarConfigChanged = rewrite_rebar_config(ChangedApps, State),
    %% print summary
    UncommitedModules = fun(M) ->
        case maps:find(changes, M) of
                error -> [];
                {ok, Changes} ->
                    maps:fold(fun
                            (_, #{path := Path, in_git := false}, Acc) ->
                                [Path | Acc];
                            (_, _, Acc) -> Acc
                        end, [], Changes)
        end
    end,
    Files = case RebarConfigChanged of
        true -> ["rebar.config"];
        false -> []
    end,
    FilesTouched = maps:fold(fun
        (K, #{op := Op, in_git := InGit} = M, Acc) ->
            case {Op, InGit} of
                {added, false} -> ["apps/" ++ atom_to_list(K) | Acc];
                {added, true} ->
                    UncommitedModules(M) ++ Acc;
                _ -> Acc
            end;
        (_, #{appup_path := F1, appsrc_path := F2} = M, Acc) ->
            case maps:get(app_changed, M, false) of
                true -> [F2];
                false -> []
            end ++
            case maps:get(appup_changed, M, false) of
                true -> [F1];
                false -> []
            end ++
            UncommitedModules(M) ++ Acc
    end, Files, NewMap),

    case relflow_state:check(State) of
        true ->
            NeededChanges = maps:fold(fun
                (_, _, true) -> true;
                (_, #{op := _}, Acc) -> Acc;
                (_, M, _) ->
                    maps:get(app_changed, M, false)
                    orelse
                    maps:get(appup_changed, M, false)
            end, RebarConfigChanged, NewMap),
            case NeededChanges of
                true ->
                    ?PRV_ERROR(check_failed);
                false ->
                    rebar_api:info("Check OK", []),
                    {ok, relflow_state:rebar_state(State)}
            end;
        false ->
            do_git(FilesTouched, State),
            {ok, relflow_state:rebar_state(State)}
    end.

do_git(FilesTouched, State) ->
    %% git things
    NewRelVsn = relflow_state:nextrelver(State),
    GitAddCmds = [ fmt("git add ~s", [AddFile]) || AddFile <- FilesTouched ],
    GitCmds = GitAddCmds ++ [
        fmt("git commit -m\"relflow ~s --> ~s\"", [relflow_state:oldrelver(State), NewRelVsn]),
        fmt("git tag v~s", [NewRelVsn])
    ],
    case {relflow_state:autogit(State), relflow_state:force(State)} of
        {true, false}  -> exec_git(GitCmds);
        {false, true}  -> exec_git(GitCmds);
        {false, false} -> print_git(GitCmds);
        {true, true}   ->
            rebar_api:warn("Not running git commands, because you --forced",[]),
            print_git(GitCmds)
    end.

rewrite_appups(Map, State) ->
    DoIfNotCheckOnly = fun(F) ->
        case relflow_state:check(State) of
            true -> ok;
            false -> F()
        end
    end,
    maps:map(fun
        (_AppName, #{op := Op} = V) when Op == added orelse Op == deleted -> V;
        (_AppName, #{appup_path := Path,
                                   appup_instructions := AppupIntrstructions,
                                   appsrc_path := AppSrc,
                                   cur_vsn := CurVsn,
                                   vsn := Vsn} = M) ->
            NewVsn = case {relflow_vsn:parse(CurVsn), relflow_vsn:parse(Vsn)} of
                {{ok, PCurVsn}, {ok, PVsn}} ->
                    case relflow_vsn:gt(PCurVsn, PVsn) of
                        true -> CurVsn;
                        false -> relflow_vsn:serialize(relflow_vsn:inc(PVsn))
                    end;
                {{ok, _}, _} -> CurVsn;
                _ -> ?DEFAULT_INITIAL_VERSION
            end,
            NewM = case NewVsn == CurVsn of
                true -> M;
                false ->
                    DoIfNotCheckOnly(fun() ->
                        ok = relflow_rewriter:set_appfile_version(AppSrc,NewVsn)
                    end),
                    maps:put(app_changed, true, M)
            end,
            OldAppupInstructions = relflow_appup:extract_instructions(Path,
                                                                Vsn, NewVsn),
            DiffAppupInstructions = relflow_appup:diff_instructions(
                                    OldAppupInstructions, AppupIntrstructions),
            case DiffAppupInstructions of
                [] -> NewM;
                _ ->
                    DoIfNotCheckOnly(fun() ->
                        relflow_rewriter:add_appup_instructions(Path,
                                            Vsn, NewVsn, DiffAppupInstructions)
                    end),
                    maps:put(appup_changed, true, NewM)
            end
    end, Map).

rewrite_rebar_config(ChangedApps, State) ->
    NewRelVsn = relflow_state:nextrelver(State),
    DoIfNotCheckOnly = fun(F) ->
        case relflow_state:check(State) of
            true -> ok;
            false -> F()
        end
    end,
    case ChangedApps of
    [] ->
        case relflow_state:nextver(State) of
            "auto" -> false;
            _ ->
                rebar_api:info("Rewriting release vsn in rebar.config: ~s",
                                                                [NewRelVsn]),
                DoIfNotCheckOnly(fun() ->
                    relflow_rewriter:set_rebar_relx_version("rebar.config",
                                                                    NewRelVsn)
                end),
                true
        end;
    _ ->
        CurRelVsn = relflow_state:currelver(State),
        VsnChanged = case NewRelVsn == CurRelVsn of
            true -> false;
            false ->
                rebar_api:info("Rewriting release vsn in rebar.config: ~s",
                                                                [NewRelVsn]),
                DoIfNotCheckOnly(fun() ->
                    relflow_rewriter:set_rebar_relx_version("rebar.config",
                                                                    NewRelVsn)
                end),
                true
        end,
        OldApps = case relflow_state:include_untracked(State) of
            true -> relflow_git:relapps_from_disk();
            false -> relflow_git:relapps_at("HEAD")
        end,
        NewApps = lists:foldl(fun({AppName, Op}, Apps) ->
            Exists = lists:member(AppName, Apps),
            case Op of
                added when Exists -> Apps;
                added ->
                    rebar_api:info("Adding application to rebar.config: ~s",
                                                                    [AppName]),
                    Apps ++ [AppName];
                deleted when not Exists -> Apps;
                delete ->
                    rebar_api:info("Removing application from rebar.config: ~s",
                                                                    [AppName]),
                    Apps -- [AppName]
            end
        end, OldApps, ChangedApps),
        case NewApps == OldApps of
            true -> VsnChanged;
            false ->
                DoIfNotCheckOnly(fun() ->
                    relflow_rewriter:set_rebar_relx_apps("rebar.config",
                                                                        NewApps)
                end),
                true
        end
    end.

exec_git(Cmds) ->
    lists:foreach(fun(Cmd) ->
        case os:cmd(Cmd) of
            ""  -> rebar_api:info("$ ~s", [Cmd]);
            Res -> rebar_api:info("$ ~s\n~s", [Cmd, string:strip(Res, right)])
        end
    end, Cmds).

print_git(Cmds) ->
    S = iolist_to_binary([ [C, "\n"] || C <- Cmds ]),
    rebar_api:info("Recommended git commands:\n~s", [S]).

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).
