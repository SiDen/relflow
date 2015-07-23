-module(relflow_appup).
-export([
    add_instructions/4,
    diff_instructions/2,
    extract_instructions/3,
    generate_appups/2
]).

generate_appups(Map, State) when is_map(Map) ->
    maps:map(fun(K, V) ->
        case maps:find(op, V) of
            {ok, added} -> V;
            {ok, deleted} -> V;
            error -> generate_appup_ops(K, V, State)
        end
    end, Map).

%%

generate_appup_ops(AppName, AppMap = #{changes := Changes}, State) ->
    Ctx = #{appname => AppName, profile => relflow_state:profile(State)},
    Instructions = maps:fold(
        fun(Mod, ModMap, InstrAcc) ->
            [module_instructions(Mod, ModMap, Ctx) | InstrAcc]
        end,
        [],
        Changes
    ),
    maps:put(appup_instructions, Instructions, AppMap).


module_instructions(Mod, #{status := added}, _Ctx) ->
    {add_module, Mod};

module_instructions(Mod, #{status := deleted}, _Ctx) ->
    {delete_module, Mod};

module_instructions(Mod, #{status := modified}, #{appname := AppName, profile := Profile}) ->
    BeamInfo = beam_info(AppName, Mod, Profile),
    WithUpgradeHook = has_upgrade_hook(BeamInfo),
    case is_supervisor(BeamInfo) of
        true ->
            {
                update_supervisor,
                Mod,
                has_sup_upgrade_notify(BeamInfo),
                WithUpgradeHook
            };
        false ->
            {
                update_module,
                Mod,
                has_code_change(BeamInfo),
                WithUpgradeHook
            }
    end.

is_supervisor(#{behaviours := Behavs}) ->
    lists:member(supervisor, Behavs).

has_sup_upgrade_notify(#{exports := Exports}) ->
    lists:member({sup_upgrade_notify, 2}, Exports).

has_code_change(#{exports := Exports}) ->
    lists:member({code_change, 3}, Exports) orelse
    lists:member({code_change, 4}, Exports).

has_upgrade_hook(#{exports := Exports}) ->
    lists:member({appup_upgrade_hook, 2}, Exports).


beam_info(AppName, Mod, Profile) ->
    BeamInfo = read_beam(beam_path(AppName, Mod, Profile)),
    BeamInfo#{name => Mod, appname => AppName}.

beam_path(AppName, Mod, RebarProfile) when is_atom(Mod) ->
    lists:flatten(
      io_lib:format(
        "_build/~s/lib/~s/ebin/~s.beam",
        [RebarProfile, AppName, Mod])).


read_beam(Path) ->
    case file:read_file(Path) of
        {ok, Beam} ->
            extract_module_info(Beam);
        _ ->
            io:format("Could not read ~s\n", [Path]),
            throw(fail_beam_read)
    end.

extract_module_info(Beam) when is_binary(Beam) ->
    Chunker = fun(K) ->
        case beam_lib:chunks(Beam, [K]) of
            {ok, {_, [{K, Result}]}} -> Result;
            _ -> []
        end
    end,
    Exports = Chunker(exports),
    %% Tidy up the americanised spelling of behaviour
    Behaviours = lists:usort(
                    lists:flatten(
                        proplists:get_value(behaviour, Chunker(attributes), []) ++
                        proplists:get_value(behavior,  Chunker(attributes), []))),
    #{
        behaviours => Behaviours,
        exports => Exports
    }.

add_instructions(OldContent, OldVsn, NewVsn, Instrs) ->
    {Upgrades, Downgrades} = generate_instructions(OldVsn, NewVsn, Instrs),
    case OldContent of
        {NewVsn, OldUpgrades, OldDowngrades} ->
            AddInstrs = fun(L, A) ->
                case lists:keytake(OldVsn, 1, L) of
                    {value, {_, O}, R} ->
                        [{OldVsn, O ++ A} | R];
                    false -> [{OldVsn, A} | L]
                end
            end,
            {NewVsn,
                AddInstrs(OldUpgrades, Upgrades),
                AddInstrs(OldDowngrades, Downgrades)
            };
        _ -> {NewVsn,
                [{OldVsn, Upgrades}],
                [{OldVsn, Downgrades}]
            }
    end.

diff_instructions({OldUpgrInstrs, _OldDwngrdInstrs}, NewInstrs) ->
    OldInstrs = lists:foldl(fun
        ({add_module, Name}, Acc) -> [{add_module, Name} | Acc];
        ({delete_module, Name}, Acc) -> [{delete_module, Name} | Acc];
        (I, Acc) when is_tuple(I) ->
            case element(1, I) of
                load_module -> [{update_module, element(2, I)} | Acc];
                update -> [{update_module, element(2, I)} | Acc];
                _ -> Acc
            end;
        (_, Acc) -> Acc
    end, [], OldUpgrInstrs),
    lists:filter(fun
        ({add_module, _Name} = I) -> not lists:member(I, OldInstrs);
        ({delete_module, _Name} = I) -> not lists:member(I, OldInstrs);
        ({update_supervisor, Name, _, _}) ->
            not lists:member({update_module, Name}, OldInstrs);
        ({update_module, Name, _, _}) ->
            not lists:member({update_module, Name}, OldInstrs)
    end, NewInstrs).

extract_instructions(FileName, OldVsn, NewVsn) ->
    case file:consult(FileName) of
        {ok, [{NewVsn, Upgrades, Downgrades}]} ->
            {
                proplists:get_value(OldVsn, Upgrades, []),
                proplists:get_value(OldVsn, Downgrades, [])
            };
        _ -> {[], []}
    end.

generate_instructions(OldVsn, NewVsn, Instrs) ->
    {Upgrd, Dwngrd} = lists:foldl(fun
       ({add_module, Name}, {U, D}) ->
            {
                [{add_module, Name} | U],
                [{delete_module, Name} | D]
            };
        ({delete_module, Name}, {U, D}) ->
            {
                [{delete_module, Name} | U],
                [{add_module, Name} | D]
            };
        ({update_supervisor, Name, UpgradeNotify, UpgradeHook}, {U, D}) ->
            {UHook, DHook} = case UpgradeHook of
                true ->
                    {
                        [{apply, {Name, appup_upgrade_hook, [OldVsn, NewVsn]}}],
                        [{apply, {Name, appup_upgrade_hook, [NewVsn, OldVsn]}}]
                    };
                false -> {[], []}
            end,
            {UNotify, DNotify} = case UpgradeNotify of
                true ->
                    {
                        [{apply, {Name, sup_upgrade_notify, [OldVsn, NewVsn]}}],
                        [{apply, {Name, sup_upgrade_notify, [NewVsn, OldVsn]}}]
                    };
                false -> {[], []}
            end,
            {
                UHook ++ UNotify ++ [{update, Name, supervisor} | U],
                DHook ++ DNotify ++ [{update, Name, supervisor} | D]
            };
        ({update_module, Name, CodeChange, UpgradeHook}, {U, D}) ->
            {UHook, DHook} = case UpgradeHook of
                true ->
                    {
                        [{apply, {Name, appup_upgrade_hook, [OldVsn, NewVsn]}}],
                        [{apply, {Name, appup_upgrade_hook, [NewVsn, OldVsn]}}]
                    };
                false -> {[], []}
            end,
            {UI, DI} = case CodeChange of
                true ->
                    {
                        {update, Name, {advanced, {OldVsn, NewVsn, []}}},
                        {update, Name, {advanced, {NewVsn, OldVsn, []}}}
                    };
                false ->
                    {
                        {load_module, Name},
                        {load_module, Name}
                    }
            end,
            {
                UHook ++ [UI | U],
                DHook ++ [DI | D]
            }
    end, {[], []}, Instrs),
    {sort_instructions(Upgrd), sort_instructions(Dwngrd)}.

sort_instructions(L) ->
    lists:sort(fun sort_instr_cmp/2, L).

%% we always want to load/add/update a module before calling apply MFA on it:
sort_instr_cmp({load_module, M},   {apply, {M, _F, _A}}) ->
    true;
sort_instr_cmp({add_module, M},    {apply, {M, _F, _A}}) ->
    true;
sort_instr_cmp({update, M, _},     {apply, {M, _F, _A}}) ->
    true;
%% and we always do the apply MFA before we delete the module
sort_instr_cmp({delete_module, M}, {apply, {M, _F, _A}}) ->
    false;
%%
%% sorting precedence for various types of appup instruction
sort_instr_cmp({add_module,_}=A,{add_module,_}=B) ->
    compare(A,B);
%% add_module to the top
sort_instr_cmp({add_module,_},_) ->
    true;
sort_instr_cmp({delete_module,_}=A,{delete_module,_}=B) ->
    compare(A,B);
%% delete_module to the bottom
sort_instr_cmp({delete_module,_},_) ->
    false;
sort_instr_cmp({update,_,supervisor}=A,{update,_,supervisor}=B) ->
    compare(A,B);
%% supervisor updates before other module types
sort_instr_cmp({update,_,supervisor},_) ->
    true;
%% otherwise compare module name using cmp_name/2
sort_instr_cmp(A,B) ->
    compare(A,B).

compare(A,B) ->
    cmp(unpack(A),unpack(B)).

%% modules are ordered alphabetically, except _sup modules sort first
cmp(A,B) when is_atom(A), is_atom(B) ->
    cmp(atom_to_list(A), atom_to_list(B));
cmp(A,B) when is_list(A), is_list(B) ->
    ASup = lists:suffix("_sup", A),
    BSup = lists:suffix("_sup", B),
    case {ASup, BSup} of
        {true, false} -> true;
        {false, true} -> false;
        _             -> A =< B
    end;
cmp(A,B) ->
    A =< B.

unpack({add_module,M}) -> M;
unpack({delete_module,M}) -> M;
unpack({load,M}) -> M;
unpack({update,M,_}) -> M;
unpack({apply,{M,_,_}}) -> M;
unpack(M) -> M.
