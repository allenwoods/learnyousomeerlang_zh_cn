# Postscript: Maps

## [About This Chapter]

Oh hi. It's been a while. This is an add-on chapter, one that isn't yet part of the printed version of Learn You Some Erlang for great good! Why does this exist? Was the printed book a huge steaming pile of bad information? I hope not (though you can consult [the errata](http://www.nostarch.com/erlang#updates) to see how wrong it is).

The reason for this chapter is that the Erlang zoo of data types has grown just a bit larger with the R17 release, and reworking the entire thing to incorporate the new type would be a lot of trouble. The R17 release had a lot of changes baked in, and is one of the biggest one in a few years. In fact, the R17 release should be called the 17.0 release, as Erlang/OTP then started changing their `R<Major>B<Minor>` versioning scheme to a [different versioning scheme](http://www.erlang.org/doc/system_principles/versions.html).

In any case, as with most updates to the language that happened before, I've added notes and a few bits of content here and there through the website so that everything stays up to date. The new data type added, however, deserves an entire chapter on its own.

![A weird road-runner representing EEP-43](../img/eep-eep.png)

## [EEP, EEP!]

The road that led to maps being added to the language has been long and tortuous. For years, people have been complaining about records and key/value data structures on multiple fronts in the Erlang community:

-   People don't want to put the name of the record every time when accessing it and want dynamic access
-   There is no good canonical arbitrary key/value store to target and use in pattern matches
-   Something faster than `dict`s and `gb_trees` would be nice
-   Converting to and from key/value stores is difficult to do well, and native data types would help with this
-   Pattern matching is a big one, again!
-   Records are a preprocessor hack and problematic at run time (they are tuples)
-   Upgrades with records are painful
-   Records are bound to a single module
-   And so much more!

There ended up being two competing proposals. The first is [Richard O'Keefe's frames](http://www.cs.otago.ac.nz/staffpriv/ok/frames.pdf), a really deeply thought out proposal on how to replace records, and Joe Armstrong's Structs (O'Keefe's proposal describes them briefly and compares them to frames).

Later on, the OTP team came up with [Erlang Enhancement Proposal 43 (EEP-43)](http://www.erlang.org/eeps/eep-0043.html)).

Maps were the one picked to be implemented by the OTP team, and frames are still hanging with their own proposal, somewhere.

The proposal for maps is comprehensive, covers many cases, and highlights many of the design issues in trying to have them fit the language, and for these reasons, their implementation will be gradual. The specification is described in [What Maps Shall Be](maps.html#what-maps-shall-be).

## [What Maps Shall Be]

### The Module

Maps are a data type similar to the `dict` data structure in intent, and has been given a module with a similar interface and semantics. The following operations are supported:

-   `maps:new()`: returns a new, empty map.
-   `maps:put(Key, Val, Map)`: adds an entry to a map.
-   `maps:update(Key, Val, Map)`: returns a map with `Key`'s entry modified or raises the error `badarg` if it's not there.
-   `maps:get(Key, Map)` and `maps:find(Key, Map)`: find items in a map. The former returns the value if present or raises the `badkey` error otherwise, whereas the latter returns `` or `error`. Another version of the function is `maps:get(Key, Map, Default)`, which allows to specify the value to return if the key is missing.
-   `maps:remove(Key, Map)`: returns a map with the given element deleted. Similarly, `maps:without([Keys], Map)` will delete multiple elements from the returned map. If a key to be deleted isn't present, the functions behave as if they had deleted it --- they return a map without it. The opposite of `maps:without/2` is `maps:with/2`.

It also contains the good old functional standards such as `fold/3` and `map/2`, which work similarly to the functions in the `lists` module. Then there is the set of utility functions such as `size/1`, `is_map/1` (also a guard!), `from_list/1` and `to_list/1`, `keys/1` and `values/1`, and set functions like `is_key/2` and `merge/2` to test for membership and fuse maps together.

That's not all that exciting, and users want more!

### The Syntax

Despite the promised gains in speed, the most anticipated aspect of maps is the native syntax they have. Here are the different operations compared to their equivalent module call:

  Maps Module       Maps Syntax
  ----------------- -----------------------
  `maps:new/1`      `#`
  `maps:put/3`      `Map#`
  `maps:update/3`   `Map#`
  `maps:get/2`      `Map#`
  `maps:find/2`     `# = Map`

Bear in mind that [not all of this syntax has been implemented yet](maps.html#stubby-legs). Fortunately for map users, the pattern matching options go wider than this. It is possible to match more than one item out of a map at once:

```eshell
1> Pets = #.
#
2> # = Pets.
#
```

Here it's possible to grab the contents of any number of items at a time, regardless of order of keys. You'll note that elements are set with `=>` and matched with `:=`. The `:=` operator can also be used to update an *existing* key in a map:

```eshell
3> Pets#.
#
4> Pets#.
** exception error: bad argument
     in function  maps:update/3
        called as maps:update(dog,"chester",#)
     in call from erl_eval:'-expr/5-fun-0-'/2 (erl_eval.erl, line 257)
     in call from lists:foldl/3 (lists.erl, line 1248)
```

There's more matching in the specification, although it's not available in 17.0 yet:

```eshell
5> #.
#
6> Name.
"winston"
```

Within the same pattern, a known key's value can be used to define a variable (`Animal` = Map`) because there could be multiple keys with the same value.

::: note
**Note:** The syntax for accessing a single value (`Map#`) is documented as such in the EEP, but is subject to change in the future once implemented, and might be dropped entirely in favor of different solutions.
:::

There's something interesting but unrelated that was added with maps. If you remember in [Starting Out For Real](starting-out-for-real.html#list-comprehensions), we introduced list comprehensions:

```eshell
7> Weather = [,   
7>            ].
[,
 ,
 ,
 ,
 ,
 ]
8> FoggyPlaces = [X ||  <- Weather].
[london,boston]
```

The same kind of stuff can be done with map comprehensions, once they're made available:

```eshell
9> Weather = #{toronto => rain, montreal => storms, london => fog,
9>             paris => sun, boston => fog, vancouver => snow}.
#{boston => fog,
  london => fog,
  montreal => storms,
  paris => sun,
  toronto => rain,
  vancouver => snow}
10> FoggyPlaces = [X || X := fog <- Weather].
[london,boston]
```

Here, `X := fog <- Weather` represents a map generator, of the form `Key := Val <- Map`. This can be composed and substituted the same way list generators and binary generators can. Map comprehensions can also generate new maps:

```eshell
11> #.
#
```

Or to implement the map operation from the `maps` module itself:

```erl
map(F, Map) ->
    #.
```

And that's most of it! It's extremely refreshing to see this new data type joining the Erlang zoo, and hopefully users will appreciate it.

![A bee looking over a map](../img/bee.png "I'm grunting! what a pun, what a pun!")

### The Gritty Details

The details aren't *that* gritty, just a little bit. The inclusion of maps in the language will impact a few things. EEP-43 goes into detail to define potential changes, many somewhat still in the air, for parts such as the distributed Erlang protocol, operator precedence, backwards compatibility, and suggestions to Dialyzer extensions (it is yet to see if the support will go as far as the EEP recommends).

Many of the changes have been proposed such that the user doesn't need to think very hard about them. One that is unavoidable, however, is sorting. Previously in the book, the sorting order was defined as:

``` expand
number < atom < reference < fun < port < pid < tuple < list < bit string
```

Maps now fit in here:

``` expand
number < atom < reference < fun < port < pid < tuple < map < list < bit string
```

Bigger than tuples and smaller than lists. Interestingly enough, maps can be compared to each other based on their keys and values:

```eshell
2> lists:sort([#]).
[#]
```

The sorting is done similarly to lists and tuples: first by size, then by the elements contained. In the case of maps, these elements are in the sorted order of keys, and breaking ties with the values themselves.

::: 
**Don't Drink Too Much Kool-Aid:**\
You may notice that while we can't update the key `1.0` of a map with the key `1`, it is possible to have them both compare as equal that way! This is one of the long-standing warts of Erlang. While it's very convenient to have all numbers compare equal from time to time, for example, when sorting lists so that `1.0` isn't greater than `9121`, this creates confusing expectations when it comes to pattern matching.

For example, while we can expect `1` to be equal to `1.0` (although not *strictly equal*, as with `=:=`), we can't expect to pattern match by doing `1 = 1.0`.

In the case of maps, this means that `Map1 == Map2` isn't a synonym of `Map1 = Map2`. Because Erlang maps respect Erlang sorting order, a map such as `#`, but you won't be able to match them one against the other.
:::

Be careful, because although the content of this section is written as based on EEP-43, the actual implementation might be lagging behind!

## [Stubby Legs for Early Releases]

![A Corgi with a cape, flying](../img/corgi.png)

The maps implementation that came with Erlang 17.x and 18.0 is complete, but only within the confines of the maps module. The major differences come from the syntax. Only a minimal subset of it is available:

-   literal map declarations (`#`)
-   matching on known keys (`# = SomeMap`)
-   Updating and adding elements with a known key in an existing map (`Map#`)
-   Using variables as keys both when assigning (`X = 3, #`), starting in Erlang 18.0

The rest, including accessing single values (`Map#`), whether in matches or declarations, is still not there. Same fate for map comprehensions and Dialyzer support. In fact, the syntax may change and differ from the EEP in the end.

Maps are also still slower than most Erlang developers and the OTP team want them to be. Still, progress is ongoing and it should not take long --- especially compared to how long it's been before maps were added to the language in the first place --- before they get much better.

This early release is still good enough to get familiar with maps, and more importantly, get to know when and where to use them.

## [Mexican Standoff]

Everyone had their opinion on wanting native dictionaries or better records replacement as a priority. When maps were announced, many Erlang developers kind of just assumed that maps, when they came to the language, would solve the problem they wanted solved.

As such, there is some confusion on how maps should be used in Erlang.

### Maps Vs. Records Vs. Dicts

To get it out of the way directly, maps are a replacement for dicts, not records. This can be confusing. At the beginning of this chapter, I identified common complaints, and it turns out that many of the complaints about records would be fixed by maps:

  Issue Solved By Maps                     Issue in Records   Issue in Dicts
  ---------------------------------------- ------------------ ----------------
  Record Name is cumbersome                ✓                   
  No good native k/v store                                    ✓
  Faster k/v store                                            18.x and above
  Converting easier with native type       ✓                  ✓
  More powerful pattern matching                              ✓
  Upgrades with Records                    maybe               
  Usable across modules without includes   ✓                   

![squid aiming two guns mexican-standoff-like, while eating a taco](../img/mexican-standoff.png)

The score is pretty close. The point of maps being faster isn't necessarily the case yet, but optimizations should bring them to a better level. The OTP team is respecting the old slogan: first make it work, then make it beautiful, and only if you need to, make it fast. They're getting semantics and correctness out of the way first.

For upgrades with records being marked as 'maybe', this has to do with the `code_change` functionality. A lot of users are annoyed by having to convert records openly when they change versions similar to what we did with [pq_player.erl](static/erlang/processquest/apps/processquest-1.1.0/src/pq_player.erl.html) and its upgrade code. Maps would instead allow us to just add fields as required to the entry and keep going. The counter-argument to that is that a botched up upgrade would crash early with records (a good thing!) whereas a botched up upgrade with maps may just linger on with the equivalent of corrupted data, and not show up until it's too late.

So why is it we should use maps as dicts and not as records? For this, a second table is necessary. This one is about *semantics*, and which data structure or data type a feature applies to:

  Operations                        Records   Maps   Dict
  --------------------------------- --------- ------ ------
  Immutable                         ✓         ✓      ✓
  Keys of any type                            ✓      ✓
  Usable with maps/folds                      ✓      ✓
  Content opaque to other modules   ✓                
  Has a module to use it                      ✓      ✓
  Supports pattern matching         ✓         ✓      
  All keys known at compile-time    ✓                
  Can merge with other instance               ✓      ✓
  Testing for presence of a key               ✓      ✓
  Extract value by key              ✓         ✓      ✓
  Per-key Dialyzer type-checking    ✓         \*     
  Conversion from/to lists                    ✓      ✓
  Per-element default values        ✓                
  Standalone data type at runtime             ✓      
  Fast direct-index access          ✓                

*\* The EEP recommends making this possible for keys known at compile-time, but has no ETA on when or if this will happen.*

This chart makes it rather obvious that despite the similarity in syntax between maps and records, dicts and maps are much closer together *semantically* than maps are to records. Therefore, using maps to replace records would be similar to trying to replace 'structs' in a language like C with a hash map. It's not impossible, but that doesn't mean it's a good idea given they often have different purposes.

Keys being known at compile time brings advantages with fast access to specific values (faster than what is possible dynamically), additional safety (crash early rather than corrupting state), and easier type checking. These make records absolutely appropriate for a process' internal state, despite the occasional burden of writing a more verbose `code_change` function.

On the other hand, where Erlang users would use records to represent complex nested key/value data structures (oddly similar to objects in object-oriented languages) that would frequently cross module boundaries, maps will help a lot. Records were the wrong tool for that job.

To put it briefly, places where records really felt out of place and cumbersome *could* be replaced by maps, but most record uses *should not* be replaced that way.

::: 
**Don't Drink too Much Kool-Aid:**\
It is often tempting to bring complex nested data structures to the party and use them as one big transparent object to pass in and out of functions or processes, and to then just use pattern matching to extract what you need.

Remember, however, that there is a cost to doing these things in message passing: Erlang data is copied across processes and working that way might be expensive.

Similarly, passing big transparent objects in and out of functions should be done with care. Building a sensible interface (or API) is already difficult; if you marry yourself to your internal data representation, enormous amounts of flexibility for the implementation can be lost.

It is better to think about your interfaces and message-based protocols with great care, and to limit the amount of information and responsibility that gets shared around. Maps are a replacement for dicts, not for proper design.
:::

There are also performance impacts to be expected. Richard O'Keefe mentions it in his proposal:

> You can't make dict good for the record-like uses that frames are meant for without making it bad for its existing uses.

And the EEP from the OTP team also mentions something similar:

> When comparing maps with records the drawbacks are easily remedied by maps, however the positive effects *\[sic\]* is not as easy to replicate in a built-in data-type where values are determined at runtime instead of at compile time.
>
> -   Being faster than direct-indexing array, where indices and possibly the resulting value are determined at compile time, is hard. In fact it is impossible.
> -   Memory model for maps where the efficiency is near that of records could be achieved by essentially using two tuples, one for keys and one for values as demonstrated in frames. This would impact performance of updates on maps with a large number of entries and thus constrain the capability of a dictionary approach.

For the core of your process loop, when you know all keys that should exist, a record would be a smart choice, performance-wise.

### Maps Vs. Proplists

One thing that maps may beat are proplists. A proplist is a rather simple data structure that is extremely well-suited for options passed to a module.

The `inet:setopts/2` call can take a list of options of the form `[`.

![a scale measuring proplists vs. maps](../img/scale.png)

Maps, with their single-key access (whenever implemented) will represent a similar way to define properties. For example, `[`).

It's somewhat to be expected that option lists that are *mostly* pairs will be replaced by maps. On the other hand, literal options such as `read`, `write`, or `append` will remain much nicer with proplists, from the user's perspective.

Given that right now most functions that require options use proplists, keeping things going that way forward may be interesting for consistency's sake. On the other hand, when the options are mostly pairs, using the `proplists` module can get cumbersome fast. Ultimately, the author of a library will have to make a decision between what could be internal clarity of implementation or general ecosystem consistency. Alternatively, the author could decide to support both ways of doing things at once to provide a graceful path towards deprecation of proplists, if that's how they feel about it.

On the other hand, functions that used to pass proplists as a return value from functions should probably switch to maps. There is little legacy there, and usability should be much better for users that way.

::: note
**Note:** maps can use a clever trick to set many default values at once easily. Whereas proplists would require the use of `proplists:get_value(Key, List, Default)`, maps can use their `merge/2` function.

According to the specification, `merge/2` will fuse two maps together, and if two keys are the same, the second map's value will prevail. This allows to call `maps:merge(Default, YourMap)` and get the desired values. For example, `maps:merge(#`.

This makes for an extremely convenient way to attribute default values manually, and then you can just use the resulting map without worrying about missing keys.
:::

## [How This Book Would Be Revised For Maps]

I wanted to add this section because I do not necessarily have the time to update the book in its entirety to retrofit maps back in at this point in time.

For most of the book, however, little would change. It would mostly be replacing calls to the `dict` module and to `gb_trees` (in cases where the smallest/largest values aren't frequently required) with inline maps syntax. Pretty much none of the records used within the book would be changed, for semantics' sake.

I may revisit a few modules once maps are stable and fully implemented, but in the mean time, many examples would be impractical to write that way given the partial maps implementation.
