-module(s2_tests).

-include_lib("eunit/include/eunit.hrl").

is_proper_subset_test() ->
    Set1 = s2:from_list([1, 2, 3]),
    Set2 = s2:from_list([1, 2]),
    ?assert(s2:is_proper_subset(Set2, Set1)).

is_not_proper_subset_test() ->
    Set1 = s2:from_list([1, 2, 3]),
    ?assertNot(s2:is_proper_subset(Set1, Set1)).
