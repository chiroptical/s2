s2
=====

s2 is very basic wrapper around Erlang's `sets` module. The API surface is very
similar to Haskell's `Data.Set` library because that appeals to me. What does s2
code look like?

```erlang
1> X = s2:from_list([1, 2, 3]).
#{1 => [], 2 => [], 3 => []}
2> Y = s2:empty().
#{}
3> s2:union(X, Y).
#{1 => [], 2 => [], 3 => []}
```

The reason I decided to make this library was because the Erlang Language
Platform LSP kept bothering me about using `{version, 2}` when constructing
sets in my Advent of Code solutions. I am lazy and I don't want to worry about
remembering this. Additionally, day-to-day I write Haskell so the API naming of
`Data.Set` is very easy for me to remember. If Erlang's maintainers switched to
using the `{version, 2}` in the next release of Erlang this library would still
be useful for me.

Please feel free to file an issue if you think other functions would be useful
for Advent of Code problems.

Contributing
=====

If you are adding any new functionality please add a unit test, ensure `rebar3
eunit` passes and please run `rebar3 fmt -w` before submitting your pull
request. In theory, you should be able to use any of Erlang 25, 26, or 27 to
develop this library. I personally use `nix` to manage all of my development
environments, including this repository.
