-module(s2_tests).

-include_lib("eunit/include/eunit.hrl").

is_proper_subset_test() ->
    Set1 = s2:from_list([1, 2, 3]),
    Set2 = s2:from_list([1, 2]),
    ?assert(s2:is_proper_subset(Set2, Set1)).

is_not_proper_subset_test() ->
    Set1 = s2:from_list([1, 2, 3]),
    ?assertNot(s2:is_proper_subset(Set1, Set1)).

partition_test() ->
    Set = s2:from_list([1, 2, 3]),
    {Keep, Discard} = s2:partition(fun(X) -> X >= 2 end, Set),
    ?assertEqual(s2:from_list([1]), Discard),
    ?assertEqual(s2:from_list([2, 3]), Keep).

insert_test() ->
    Input = s2:singleton(0),
    Output = s2:from_list([0, 1]),
    ?assertEqual(s2:insert(1, Input), Output).

cartesian_product_test() ->
    Set1 = s2:from_list([1, 2]),
    Set2 = s2:from_list([3, 4]),
    Result = s2:from_list([{1, 3}, {1, 4}, {2, 3}, {2, 4}]),
    ?assertEqual(s2:cartesian_product(Set1, Set2), Result).
