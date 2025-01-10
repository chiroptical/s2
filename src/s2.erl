-module(s2).
-moduledoc """
Here lies s2, a wrapper around the Erlang's [`sets`](https://www.erlang.org/doc/apps/stdlib/sets.html)
module which gives you the `{version, 2}` representation without having to worry about it. This library
exists because I was using [elp](https://github.com/WhatsApp/erlang-language-platform) during
Advent of Code and it kept complaining about using `{version, 2}` for `sets:new/1` and `sets:from_list/1`.
The documentation says,

> This new representation will become the default in future Erlang/OTP versions. 

but it hasn't happened yet so either this library will be useful or it will push Erlang/OTP
to make it the default. Either way, he API surface is more similar to Haskell's 
[`Data.Set`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html) module
because of my own personal preferences. For reference, the `{version, 2}` representation is just
a map where all the values are `[]`.

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

-doc """
Construct a set with a single element.

```erlang
1> s2:singleton(1).
#{1 => []}
```
""".
-spec singleton(Elem) -> Set when Elem :: Element, Set :: set(Element).
singleton(X) ->
    s2:from_list([X]).

-doc """
Construct a set from a list of elements.

```erlang
1> s2:from_list([1, 2, 3]).
#{1 => [], 2 => [], 3 => []}
```
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

-doc """
Given a list of sets, take the set intersection of all sets.

```erlang
1> s2:intersections([s2:singleton(1), s2:singleton(1)]).
#{1 => []}
2> s2:intersections([s2:singleton(1), s2:singleton(2)]).
#{}
```
""".
-spec intersections(SetList) -> Set when SetList :: [set(Element), ...], Set :: set(Element).
intersections(SetList) ->
    sets:intersection(SetList).

-doc """
Given two sets, take the set intersection of them.

```erlang
1> s2:intersection(s2:singleton(1), s2:singleton(1)).
#{1 => []}
2> s2:intersection(s2:singleton(1), s2:singleton(2)).
#{}
```
""".
-spec intersection(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
intersection(Set1, Set2) ->
    sets:intersection(Set1, Set2).

-doc """
Given two sets, determine if their intersection is the empty set.
Returns `true` if the intersection is the empty set and `false` otherwise.

```erlang
1> s2:is_disjoint(s2:singleton(1), s2:singleton(2)).
true
2> s2:is_disjoint(s2:from_list([1, 2]), s2:singleton(2)).
false
```
""".
-spec is_disjoint(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_disjoint(Set1, Set2) ->
    sets:is_disjoint(Set1, Set2).

-doc """
Given an element and a set, if the element is contained in the set return
`true` otherwise return `false`.

```erlang
1> s2:is_element(1, s2:singleton(1)).
true
2> s2:is_element(1, s2:singleton(2)).
false
```
""".
-spec is_element(Element, Set) -> boolean() when Set :: set(Element).
is_element(Element, Set) ->
    sets:is_element(Element, Set).

-doc """
Given a set, if it is the empty set return `true` else return `false`.

```erlang
1> s2:is_empty(s2:empty()).
true
2> s2:is_empty(s2:singleton(1)).
false
```
""".
-spec is_empty(Set) -> boolean() when Set :: set(_).
is_empty(Set) ->
    sets:is_empty(Set).

-doc """
Given two sets, if every element of the first is contained in the second
return `true` otherwise return `false`.

```erlang
1> s2:is_subset(s2:singleton(1), s2:singleton(1)).
true
2> s2:is_subset(s2:from_list([1, 2]), s2:from_list([1, 2])).
true
3> s2:is_subset(s2:from_list([1, 2]), s2:from_list([1, 2, 3])).
true
```
""".
-spec is_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_subset(Set1, Set2) ->
    sets:is_subset(Set1, Set2).

-doc """
Given two sets, if every element of the first is contained in the second
**and** the sets are different return `true` otherwise return `false`.

```erlang
1> s2:is_proper_subset(s2:singleton(1), s2:singleton(1)).
true
2> s2:is_proper_subset(s2:from_list([1, 2]), s2:from_list([1, 2])).
false
3> s2:is_proper_subset(s2:from_list([1, 2]), s2:from_list([1, 2, 3])).
true
```
""".
-spec is_proper_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
is_proper_subset(Set1, Set2) ->
    sets:is_subset(Set1, Set2) and (sets:size(Set1) =/= sets:size(Set2)).

-doc """
Given a unary function and a set, apply the unary function to each element
of the set to form a new set. The input and output set may not be the same
size.

```erlang
1> s2:map(fun (X) -> X + 1 end, s2:from_list([1, 2, 3])).
#{2 => [],3 => [],4 => []}
2> s2:map(fun (_X) -> 1 end, s2:from_list([1, 2, 3])).
#{1 => []}
```
""".
-spec map(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> Element2), Set1 :: set(Element1), Set2 :: set(Element2).
map(Fun, Set1) ->
    sets:map(Fun, Set1).

-doc """
Given a set, return the number of elements in the set.

```erlang
1> s2:size(s2:empty()).
0
2> s2:size(s2:from_list([1, 2])).
2
```
""".
-spec size(Set) -> non_neg_integer() when Set :: set(_).
size(Set) ->
    sets:size(Set).

-doc """
Given two sets, return a new set with elements of the second
removed from the first.

```erlang
1> s2:difference(s2:from_list([1, 2, 3]), s2:empty()).
#{1 => [],2 => [],3 => []}
2> s2:difference(s2:from_list([1, 2, 3]), s2:from_list([1])).
#{2 => [],3 => []}
```
""".
-spec difference(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
difference(Set1, Set2) ->
    sets:subtract(Set1, Set2).

-doc """
Given a list of sets, return a new set with every element from
every set.

```erlang
1> s2:unions([s2:singleton(1), s2:singleton(2), s2:singleton(3)]).
#{1 => [],2 => [],3 => []}
```
""".
-spec unions(SetList) -> Set when SetList :: [set(Element)], Set :: set(Element).
unions(SetList) ->
    sets:union(SetList).

-doc """
Given two sets, return a new set with every element from both sets.

```erlang
1> s2:union(s2:singleton(1), s2:singleton(2)).
#{1 => [],2 => []}
```
""".
-spec union(Set1, Set2) -> Set3 when
    Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
union(Set1, Set2) ->
    sets:union(Set1, Set2).

-doc """
Given a set, return a list with all elements from the set.

```erlang
1> s2:to_list(s2:empty()).
[]
2> s2:to_list(s2:singleton(1)).
[1]
3> s2:to_list(s2:from_list([1, 2, 3])).
[1,2,3]
```
""".
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
