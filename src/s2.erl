-module(s2).
-moduledoc """
Here lies s2, a wrapper around the Erlang's [`sets`](https://www.erlang.org/doc/apps/stdlib/sets.html)
library which gives you `{version, 2}` without having to worry about it. The API surface is
more similar to Haskell's [`Data.Set`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html)
module because of my own personal preferences. 

If you have any suggestions or find an issue please submit an issue on [github](https://github.com/chiroptical/s2)
""".

-export([
    insert/2,
    delete/2,
    filter/2,
    fold/3,
    from_list/1,
    singleton/1,
    intersections/1,
    intersection/2,
    is_empty/1,
    is_disjoint/2,
    is_element/2,
    is_subset/2,
    is_proper_subset/2,
    to_list/1,
    map/2,
    empty/0,
    size/1,
    difference/2,
    unions/1,
    union/2,
    partition/2,
    cartesian_product/2
]).

-export_type([set/1]).

% The memory documentation https://www.erlang.org/doc/system/memory.html
% suggests this is 1 word of storage for a zero element list.
-define(VALUE, []).

-doc "As returned by `empty/0`.".
-opaque set(Element) :: #{Element => ?VALUE}.

-doc """
Return the empty set.

```erlang
1> s2:empty().
#{}
```
""".
-spec empty() -> set(_).
empty() ->
    sets:new([{version, 2}]).

-doc "Construct a `Set` with a single element.".
-spec singleton(Elem) -> Set when Elem :: Element, Set :: set(Element).
singleton(X) ->
    s2:from_list([X]).

-doc """
Construct a `Set` from a list of elements.
""".
-spec from_list(List) -> Set when List :: [Element], Set :: set(Element).
from_list(List) ->
    sets:from_list(List, [{version, 2}]).

-doc """
Add an element to a set.

```erlang
1> s2:insert(1, s2:singleton(2)).
#{1 => [],2 => []}
```
""".
-spec insert(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
insert(Elem, Set1) ->
    sets:add_element(Elem, Set1).

-doc """
Remove an element from the set. It won't fail if the element doesn't exist.

```erlang
1> Set = s2:from_list([1, 2, 3]).
#{1 => [],2 => [],3 => []}
2> s2:delete(1, Set).
#{2 => [],3 => []}
3> s2:delete(4, Set).
#{1 => [],2 => [],3 => []}
```
""".
-spec delete(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
delete(Elem, Set) ->
    sets:del_element(Elem, Set).

-doc """
Keep elements in the set for which the predicate returns `true`.

```erlang
1> S = s2:from_list([1, 2, 3, 4]).
#{1 => [],2 => [],3 => [],4 => []}
2> s2:filter(fun (X) -> X >= 2 end, S).
#{2 => [], 3 => [], 4 => []}  
```
""".
-spec filter(Pred, Set1) -> Set2 when
    Pred :: fun((Element) -> boolean()), Set1 :: set(Element), Set2 :: set(Element).
filter(Pred, Set1) ->
    sets:filter(Pred, Set1).

-doc """
Given a binary operation on elements of the set and an accumulator,
an initial accumulator, and a set collect the result of applying
the binary operation to each element of the set starting from
the initial accumulator.

```erlang
1> S = s2:from_list([1, 2, 3, 4]).
#{1 => [],2 => [],3 => [],4 => []}
2> s2:fold(fun (Elem, Acc) -> Elem + Acc end, 0, S).
10
```
""".
-spec fold(Function, Acc0, Set) -> Acc1 when
    Function :: fun((Element, AccIn) -> AccOut),
    Set :: set(Element),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc.
fold(Function, Acc0, Set) ->
    sets:fold(Function, Acc0, Set).

-spec intersections(SetList) -> Set when SetList :: [set(Element), ...], Set :: set(Element).
intersections(SetList) ->
    sets:intersection(SetList).

-spec intersection(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
intersection(Set1, Set2) ->
    sets:intersection(Set1, Set2).

-spec is_disjoint(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_disjoint(Set1, Set2) ->
    sets:is_disjoint(Set1, Set2).

-spec is_element(Element, Set) -> boolean() when Set :: set(Element).
is_element(Element, Set) ->
    sets:is_element(Element, Set).

-spec is_empty(Set) -> boolean() when Set :: set(_).
is_empty(Set) ->
    sets:is_empty(Set).

-spec is_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_subset(Set1, Set2) ->
    sets:is_subset(Set1, Set2).

-spec is_proper_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_proper_subset(Set1, Set2) ->
    sets:is_subset(Set1, Set2) and (sets:size(Set1) =/= sets:size(Set2)).

-spec map(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> Element2), Set1 :: set(Element1), Set2 :: set(Element2).
map(Fun, Set1) ->
    sets:map(Fun, Set1).

-spec size(Set) -> non_neg_integer() when Set :: set(_).
size(Set) ->
    sets:size(Set).

-spec difference(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
difference(Set1, Set2) ->
    sets:subtract(Set1, Set2).

-spec unions(SetList) -> Set when SetList :: [set(Element)], Set :: set(Element).
unions(SetList) ->
    sets:union(SetList).

-spec union(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
union(Set1, Set2) ->
    sets:union(Set1, Set2).

-spec to_list(Set) -> List when Set :: set(Element), List :: [Element].
to_list(Set) ->
    sets:to_list(Set).

-doc """
partition(Pred, Set) -> {Keep, Discard}

Similar to `filter/2` but it returns a `Tuple` of two elements. The first
contains elements where the predicate evaluates to `true`. The second contains
elements where the predicate evaluates to `false`.
""".
-spec partition(Pred, Set1) -> {Set2, Set2} when
    Pred :: fun((Element) -> boolean()), Set1 :: set(Element), Set2 :: set(Element).
partition(Pred, Set1) ->
    sets:fold(
        fun(Elem, {Keep, Discard}) ->
            case Pred(Elem) of
                true -> {s2:insert(Elem, Keep), Discard};
                false -> {Keep, s2:insert(Elem, Discard)}
            end
        end,
        {s2:empty(), s2:empty()},
        Set1
    ).

-doc """
Given two sets, pair every element in the first set with every element in the second.

```erlang
1> Product = s2:cartesian_product(s2:from_list([1, 2]), s2:from_list([3, 4])).
#{{1,3} => [],{1,4} => [],{2,3} => [],{2,4} => []}
2> Product =:= s2:from_list([{1, 3}, {1, 4}, {2, 3}, {2, 4}]).
true
```
""".
-spec cartesian_product(Set1, Set2) -> Set3 when
    Set1 :: set(Element1), Set2 :: set(Element2), Set3 :: set({Element1, Element2}).
cartesian_product(Set1, Set2) ->
    s2:fold(
        fun(X, Outer) ->
            s2:fold(
                fun(Y, Inner) ->
                    s2:insert({X, Y}, Inner)
                end,
                Outer,
                Set2
            )
        end,
        s2:empty(),
        Set1
    ).
