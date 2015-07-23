%% rewrite app and app.src files to update vsn field
-module(relflow_rewriter).

-export([
    add_appup_instructions/4,
    set_appfile_version/2,
    set_rebar_relx_apps/2,
    set_rebar_relx_version/2
]).

-define(AppHeader, "%% Vsn auto-managed by relflow utility.\n%% DO NOT CHANGE VSN FIELD MANUALLY!").

add_appup_instructions(FileName, OldVsn, NewVsn, Instructions) ->
    ok = filelib:ensure_dir(FileName),
    OldContent = case file:consult(FileName) of
        {ok, [C]} -> C;
        _ -> undefined
    end,
    NewContent = relflow_appup:add_instructions(OldContent, OldVsn, NewVsn,
                                                                Instructions),
    ok = file:write_file(FileName, io_lib:format("~p.", [NewContent])).

set_appfile_version(Filepath, NewVsn) when is_list(Filepath) ->
    {ok, [{application, AppName, Sections}]} = file:consult(Filepath),
    Vsn = proplists:get_value(vsn, Sections),  %% don't need to know previous vsn
    NewSections = [{vsn, NewVsn} | proplists:delete(vsn, Sections)],
    Contents = io_lib:format("~s\n~p.~n",[?AppHeader, {application, AppName, NewSections}]),
    ok = file:write_file(Filepath, Contents),
    rebar_api:debug("Vsn changed from ~s --> ~s in: ~s",[Vsn, NewVsn, Filepath]),
    ok.

set_rebar_relx_version(Filepath, NewVsn) ->
    set_rebar_relx_release(Filepath, fun({release, {AppName, _Vsn}, Apps}) ->
        {release, {AppName, NewVsn}, Apps}
    end).

set_rebar_relx_apps(Filepath, NewApps) ->
    set_rebar_relx_release(Filepath, fun({release, {AppName, Vsn}, _Apps}) ->
        {release, {AppName, Vsn}, NewApps}
    end).

set_rebar_relx_release(Filepath, F) ->
    case file:consult(Filepath) of
        {ok, Reltool} ->
            [H | T] = lists:map(fun
                ({relx, L}) ->
                    NL = lists:map(fun
                            ({release, {_AppName, _Vsn}, _Apps} = Release) ->
                                F(Release);
                        (E) -> E
                    end, L),
                    {relx, NL};
                (V) -> V
            end, Reltool),
            ok = file:write_file(Filepath, io_lib:format("~p.~n", [H])),
            lists:foreach(fun(P) ->
                ok = file:write_file(Filepath,
                                        io_lib:format("~p.~n", [P]), [append])
            end, T);
        {error, _} -> {error, parse_rebar_config}
    end.
