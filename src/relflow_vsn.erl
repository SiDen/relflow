-module(relflow_vsn).

-export([
    check/1,
    compare/2,
    eq/2,
    gt/2,
    inc/1,
    lt/2,
    parse/1,
    serialize/1
]).

check(Vsn) ->
    case parse(Vsn) of
        {ok, _Parsed} -> ok;
        error -> error
    end.

compare({D1, _}, {D2, _}) ->
    compare(lists:reverse(D1), lists:reverse(D2));
compare([], []) -> equal;
compare([], _) -> less;
compare(_, []) -> greater;
compare([V | T1], [V | T2]) -> compare(T1, T2);
compare([V1 | _], [V2 | _]) when V1 < V2 -> less;
compare([V1 | _], [V2 | _]) when V1 > V2 -> greater.

eq(Vsn1, Vsn2) ->
    compare(Vsn1, Vsn2) == equal.

gt(Vsn1, Vsn2) ->
    compare(Vsn1, Vsn2) == greater.

inc({[H | T], Rest}) ->
    {[H + 1 | T], Rest}.

lt(Vsn1, Vsn2) ->
    compare(Vsn1, Vsn2) == less.

parse(Vsn) ->
    try
        {H, T} = lists:splitwith(fun(E) ->
            (E >= $0 andalso E =< $9) orelse E == $.
        end, Vsn),
        case string:tokens(H, ".") of
        [] -> error;
        L ->
            NL = lists:foldl(fun(E, Acc) ->
                    case string:to_integer(E) of
                        {error, _} -> Acc;
                        {V, []} -> [V | Acc];
                        _ -> Acc
                    end
                end, [], L),
            case serialize({NL, T}) /= Vsn of
                true -> error;
                false -> {ok, {NL, T}}
            end
        end
    catch _:_ -> error
    end.

serialize({Digits, Rest}) ->
    string:join([integer_to_list(D) || D <- lists:reverse(Digits)],".") ++ Rest.
