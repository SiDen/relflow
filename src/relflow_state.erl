%% small wrapper around rebar_state to get our conf values,
%% plus a couple of extra config values we store ourselves to be passed
%% to our various helper modules
-module(relflow_state).

-record(relflow_st, {
    rebar_state,
    oldrelver,
    currelver,
    nextrelver
}).

-export([
    autogit/1,
    build_dir/1,
    check/1,
    currelver/1,
    currelver/2,
    force/1,
    include_untracked/1,
    new/1,
    nextrelver/1,
    nextrelver/2,
    nextver/1,
    oldrelver/1,
    oldrelver/2,
    profile/1,
    rebar_state/1,
    task/1,
    upfrom/1,
    version/1
]).

autogit(State) -> parg(autogit, State) == true.

build_dir(#relflow_st{rebar_state = State}) -> rebar_dir:base_dir(State).

check(State) -> parg(check, State) == true.

currelver(#relflow_st{currelver=V}) -> V.
currelver(NV, State = #relflow_st{}) -> State#relflow_st{currelver=NV}.

force(State) -> parg(force, State) == true.

include_untracked(State) -> parg(include_untracked, State) == true.

new(RebarSt) -> #relflow_st{rebar_state = RebarSt}.

nextrelver(#relflow_st{nextrelver=V}) -> V.
nextrelver(NV, State = #relflow_st{}) -> State#relflow_st{nextrelver=NV}.

nextver(State) -> parg(nextver, State).

oldrelver(#relflow_st{oldrelver=V}) -> V.
oldrelver(NV, State = #relflow_st{}) -> State#relflow_st{oldrelver=NV}.

profile(State) -> lists:last(filename:split(build_dir(State))).

rebar_state(#relflow_st{rebar_state = R}) -> R.

task(State) -> parg(task, State).

upfrom(State) -> parg(upfrom, State).

version(State) -> parg(version, State).

%%

parg(K, #relflow_st{rebar_state = State}) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(K, Opts).
