%% does git things using os:cmd("git...") and parses the output.
%%
%% This has some hardcoded paths like src and apps
%% ideally it would ask rebar about paths so it works with non-standard setups
%%
%% for now, single-app repos (src/) and apps/<app..> repos will work
-module(relflow_git).
-export([
    is_clean/0,
    relapps_at/1,
    relapps_from_disk/0,
    relver_at/1,
    relver_from_disk/0,
    since/1,
    since/2
]).

update() ->
    _ = os:cmd("git update-index --really-refresh").

since(Rev) -> since(Rev, false).

since(Rev, IncludeUntracked) when is_list(Rev) ->
    update(),
    R1 = since_revision(Rev, IncludeUntracked),
    R2 = add_app_paths(R1),
    R3 = detect_application_changes(R2, Rev, IncludeUntracked),
    appvers_at_revision(R3, Rev, IncludeUntracked).

is_clean() ->
    Cmd = "git diff-index --quiet HEAD -- && echo clean || echo dirty",
    case os:cmd(Cmd) of
        "clean" ++ _ -> true;
        "dirty" ++ _ -> false
    end.

relver_at(Rev) ->
    with_rel_at(Rev, fun(Content) ->
        with_release(Content, fun({release, {_AppName, Vsn}, _Apps}) ->
            Vsn
        end)
    end).

relver_from_disk() ->
    with_rel_from_disk(fun(Content) ->
        with_release(Content, fun({release, {_AppName, Vsn}, _Apps}) ->
            Vsn
        end)
    end).

relapps_at(Rev) ->
    with_rel_at(Rev, fun(Content) ->
        with_release(Content, fun rel_apps/1)
    end).

relapps_from_disk() ->
    with_rel_from_disk(fun(Content) ->
        with_release(Content, fun rel_apps/1)
    end).

with_rel_from_disk(F) ->
    {ok, Content} = file:consult("rebar.config"),
    F(Content).

rel_mainapp_at(Rev) ->
    with_rel_at(Rev, fun rel_app/1).

rel_apps({release, {_AppName, _Vsn}, Apps}) -> Apps.

rel_app(Content) ->
    with_release(Content, fun({release, {AppName, _Vsn}, _Apps}) ->
        AppName
    end).

with_release(Content, F) ->
    case proplists:get_value(relx, Content) of
        undefined -> undefined;
        L ->
            case lists:keyfind(release, 1, L) of
                {release, {_AppName, _Vsn}, _Apps} = R -> F(R);
                _ -> undefined
            end
    end.

with_rel_at(Rev, F) ->
    File = "./rebar.config",
    Cmd = fmt("git show ~s:~s", [Rev, File]),
    Content = os:cmd(Cmd),
    {ok, Scanned, _} = erl_scan:string(Content),
    {_, Parsed} = lists:foldl(fun
            ({dot, X}, {A, P}) ->
                {ok, NP} = erl_parse:parse_term(lists:reverse([{dot, X} | A])),
                {[], [NP | P]};
            (V, {A, P}) -> {[V | A], P}
        end, {[], []}, Scanned),
    Rel = lists:reverse(Parsed),
    F(Rel).
%%

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).

add_app_paths(Changes) ->
    maps:map(
      fun(AppName, AppMap) ->
              case maps:get(single_src_app, AppMap) of
                  true ->
                      AppSrc = fmt("src/~s.app.src", [AppName]),
                      Appup  = fmt("src/~s.appup.src",  [AppName]),
                      M1 = maps:put(appup_path,  Appup,  AppMap),
                      maps:put(appsrc_path, AppSrc, M1);
                  false ->
                      AppSrc = fmt("apps/~s/src/~s.app.src", [AppName, AppName]),
                      Appup  = fmt("apps/~s/ebin/~s.appup",  [AppName, AppName]),
                      M1 = maps:put(appup_path,  Appup,  AppMap),
                      maps:put(appsrc_path, AppSrc, M1)
              end
      end,
      Changes
     ).

since_revision(Rev, IncludeUntracked) when is_list(Rev) ->
    gather_changed_modules(changed_modules_since(Rev, IncludeUntracked)).


appver_at_revision(Rev, Name, DirType) ->
    Cmd = case DirType of
        apps_dir ->
            fmt("git show ~s:apps/~s/src/~s.app.src", [Rev, Name, Name]);
        single_src_dir ->
            fmt("git show ~s:src/~s.app.src", [Rev, Name]);
        X ->
                  throw({unhandled_dirtype, X})
    end,
    appver_of_content(Name, os:cmd(Cmd)).

appver_from_disk(Name, DirType) ->
    Cmd = case DirType of
        apps_dir ->
            fmt("cat apps/~s/src/~s.app.src", [Name, Name]);
        single_src_dir ->
            fmt("cat src/~s.app.src", [Name]);
        X ->
                  throw({unhandled_dirtype, X})
    end,
    appver_of_content(Name, os:cmd(Cmd)).

appver_of_content(Name, Content) ->
    {application, Name, AppOpts} = eval(Content),
    proplists:get_value(vsn, AppOpts).

appvers_at_revision(Changes, Rev, IncludeUntracked) when is_list(Rev) ->
    maps:map(
        fun(AppName, AppMap) ->
            case maps:find(op, AppMap) of
                {ok, Op} when Op == added orelse Op == deleted -> AppMap;
                error ->
                    SS = case maps:get(single_src_app, AppMap) of
                        true  -> single_src_dir;
                        false -> apps_dir
                    end,
                    Vsn = appver_at_revision(Rev, AppName, SS),
                    NewAppMap = maps:put(vsn, Vsn, AppMap),
                    CurVsn = case IncludeUntracked of
                        true -> appver_from_disk(AppName, SS);
                        false -> appver_at_revision("HEAD", AppName, SS)
                    end,
                    maps:put(cur_vsn, CurVsn, NewAppMap)
            end
        end, Changes).

eval(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    Env = [],
    {value, Term, Env} = erl_eval:exprs(Parsed,Env),
    Term.

git_diff_names(Rev) when is_list(Rev) ->
    Cmd = "git diff --name-status "++Rev++" | grep -E '\.erl$' | grep -E \"\t(apps|src)\"",
    [{git, L} || L <- string:tokens(os:cmd(Cmd), "\n")].

git_untracked() ->
    Cmd = "git ls-files --others --exclude-standard . | grep -E '\.erl$' | grep -E \"^(apps|src)\"",
    [{disk, L} || L <- string:tokens(os:cmd(Cmd), "\n")].

git_status_to_atom("D") -> deleted;
git_status_to_atom("A") -> added;
git_status_to_atom(_)   -> modified.

changed_modules_since(Rev, IncludeUntracked) when is_list(Rev) ->
    Files = git_diff_names(Rev) ++
        case IncludeUntracked of
            true -> git_untracked();
            false -> []
        end,
    lists:sort(lists:foldl(fun({Type, Line}, Acc) ->
        [Status, Path] = case Type of
            git ->
                case string:tokens(Line, "\t") of
                    [_Status, _Path] = Res -> Res;
                    _Else ->
                        io:format("git error: ~s\n",[Line]),
                        throw(git_error)
                end;
            disk ->
                ["A", Line]
        end,
        StatusAtom = git_status_to_atom(Status),
        Module = list_to_atom(filename:basename(Path, ".erl")),
        ModInfo = #{status => StatusAtom, path => Path, in_git => (Type==git)},
        %% This depends on unchanged rebar3 defaults for source locations,
        %% but we only support apps/ and src/
        %%
        %% directories where OTP applications for the project can be located
        %% {project_app_dirs, ["apps", "lib", "."]}.
        case filename:split(Path) of
            ["apps", _, "test" | _] -> [];
            ["test" | _] -> [];
            ["apps", AppName, "src" | _] ->
                [{AppName, Module, ModInfo}];
            ["src" | _] ->
                %% we lookup the app name at the end, during gathering:
                AppName = "$$single_app",
                [{AppName, Module, ModInfo}]
        end ++ Acc
    end, [], Files)).

gather_changed_modules(List) ->
    lists:foldl(fun({AppStr, Changes}, Acc) ->
           {Name, IsSrcApp} = case AppStr of
                "$$single_app" ->
                    AppSrc = hd(filelib:wildcard("src/*.app.src")),
                    {list_to_atom(filename:basename(AppSrc, ".app.src")), true};
                _ ->
                    {list_to_atom(AppStr), false}
            end,
            maps:put(Name, #{changes => maps:from_list(Changes),
                             single_src_app =>  IsSrcApp}, Acc)
        end,
        #{},
        gather_changed_modules(List, [])
    ).

gather_changed_modules([{AppName, Filename, ModInfo} | Rest], [{AppName,Changes} | AccRest]) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, ModInfo}|Changes]}|AccRest]);
gather_changed_modules([{AppName, Filename, ModInfo} | Rest], Acc) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, ModInfo}]}|Acc]);
gather_changed_modules([], Acc) ->
    Acc.

detect_application_changes(Changes, Rev, IncludeUntracked) ->
    ListFromBranch = fun(Br) ->
        Cmd = "git ls-tree --name-only "++Br++" "++"apps/",
        lists:foldl(fun(Path, Acc) ->
            SplittedPath = string:tokens(Path, "/"),
            Last = lists:last(SplittedPath),
            case "apps/" ++ Last == Path of
                true -> [list_to_atom(Last) | Acc];
                false ->
                    rebar_api:warn("Ignore application ~p", [Path]),
                    Acc
            end
        end, [rel_mainapp_at(Br)], string:tokens(os:cmd(Cmd), [10, 13]))
    end,
    OldApps = ListFromBranch(Rev),
    NewApps = case IncludeUntracked of
        true ->
            File = "./rebar.config",
            {ok, Content} = file:consult(File),
            {ok, L} = file:list_dir("apps"),
            [rel_app(Content) | [list_to_atom(N) || N <- L]];
        false -> ListFromBranch("HEAD")
    end,
    Old = sets:from_list(OldApps),
    New = sets:from_list(NewApps),
    Deleted = sets:to_list(sets:subtract(Old, New)),
    Added = sets:to_list(sets:subtract(New, Old)),
    Apps = [{Name, deleted} || Name <- Deleted] ++
            [{Name, added} || Name <- Added],
    InGit = case IncludeUntracked of
        false -> fun(_Name) -> true end;
        true ->
            AppsInGit = ListFromBranch("HEAD"),
            fun(Name) -> lists:member(Name, AppsInGit) end
    end,
    lists:foldl(fun({Name, Op}, Acc) ->
            NewApp = case maps:find(Name, Changes) of
                {ok, App} ->
                    maps:put(op, Op, App);
                error ->
                    #{op => Op}
            end,
            maps:put(Name, maps:put(in_git, InGit(Name), NewApp), Acc)
        end, Changes, Apps).
