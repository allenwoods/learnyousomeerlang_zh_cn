# Postscript: Maps

## [About This Chapter]

Oh hi. It's been a while. This is an add-on chapter, one that isn't yet part of the printed version of Learn You Some Erlang for great good! Why does this exist? Was the printed book a huge steaming pile of bad information? I hope not (though you can consult [the errata](http://www.nostarch.com/erlang#updates) to see how wrong it is).
哦嗨。已经有一段时间了。这是一个附加章节，它还不是印刷版的Learn You Some Erlang for great good的一部分！为什么会存在这种情况？这本印刷的书是一大堆糟糕的信息吗？我希望不会（尽管你可以查阅[勘误表](http://www。诺查克。com/erlang#更新）查看它有多错）。

The reason for this chapter is that the Erlang zoo of data types has grown just a bit larger with the R17 release, and reworking the entire thing to incorporate the new type would be a lot of trouble. The R17 release had a lot of changes baked in, and is one of the biggest one in a few years. In fact, the R17 release should be called the 17.0 release, as Erlang/OTP then started changing their `R<Major>B<Minor>` versioning scheme to a [different versioning scheme](http://www.erlang.org/doc/system_principles/versions.html).
这一章的原因是，随着R17的发布，Erlang zoo的数据类型只增加了一点，重新设计整个数据类型以合并新类型将是一个很大的麻烦。R17版本有很多变化，是几年来最大的变化之一。实际上，R17版本应该被称为17。0版本，因为Erlang/OTP随后开始将其'R<Major>B<Minor>`版本控制方案更改为[不同的版本控制方案](http://www。二郎。组织/文件/系统/原则/版本。（html）。

In any case, as with most updates to the language that happened before, I've added notes and a few bits of content here and there through the website so that everything stays up to date. The new data type added, however, deserves an entire chapter on its own.
在任何情况下，就像之前发生的大多数语言更新一样，我通过网站在这里和那里添加了注释和一些内容，以便所有内容都保持最新。然而，新添加的数据类型本身就值得一整章。

![A weird road-runner representing EEP-43](../img/eep-eep.png)

## [EEP, EEP!]

The road that led to maps being added to the language has been long and tortuous. For years, people have been complaining about records and key/value data structures on multiple fronts in the Erlang community:
导致地图被添加到语言中的道路漫长而曲折。多年来，人们一直在抱怨Erlang社区多条战线上的记录和关键/价值数据结构：

-   People don't want to put the name of the record every time when accessing it and want dynamic access
-人们不希望每次访问时都输入记录的名称，而是希望动态访问
-   There is no good canonical arbitrary key/value store to target and use in pattern matches
-   Something faster than `dict`s and `gb_trees` would be nice
-   Converting to and from key/value stores is difficult to do well, and native data types would help with this
-在键/值存储之间进行转换很困难，而本机数据类型将有助于实现这一点
-   Pattern matching is a big one, again!
-   Records are a preprocessor hack and problematic at run time (they are tuples)
-   Upgrades with records are painful
-   Records are bound to a single module
-   And so much more!

There ended up being two competing proposals. The first is [Richard O'Keefe's frames](http://www.cs.otago.ac.nz/staffpriv/ok/frames.pdf), a really deeply thought out proposal on how to replace records, and Joe Armstrong's Structs (O'Keefe's proposal describes them briefly and compares them to frames).
最后出现了两个相互竞争的提案。第一个是[Richard O'Keefe的框架](http://www。反恐精英。奥塔戈。交流电。新西兰/staffpriv/ok/frames。pdf），以及乔·阿姆斯特朗（Joe Armstrong）的结构（O'Keefe的建议简要描述了它们，并将它们与框架进行了比较）。

Later on, the OTP team came up with [Erlang Enhancement Proposal 43 (EEP-43)](http://www.erlang.org/eeps/eep-0043.html)).
后来，OTP团队提出了[Erlang增强方案43（EEP-43）](http://www。二郎。org/eeps/eep-0043。html）。

Maps were the one picked to be implemented by the OTP team, and frames are still hanging with their own proposal, somewhere.
地图是被OTP团队挑选出来实施的，框架仍然挂在他们自己的提案的某处。

The proposal for maps is comprehensive, covers many cases, and highlights many of the design issues in trying to have them fit the language, and for these reasons, their implementation will be gradual. The specification is described in [What Maps Shall Be](maps.html#what-maps-shall-be).
关于地图的建议是全面的，涵盖了许多情况，并强调了试图使其符合语言的许多设计问题，因此，它们的实施将是渐进的。规范在[应使用什么地图]（地图）中描述。html#应该是什么地图）。

## [What Maps Shall Be]

### The Module

Maps are a data type similar to the `dict` data structure in intent, and has been given a module with a similar interface and semantics. The following operations are supported:
Maps是一种与“dict”数据结构意图相似的数据类型，并被赋予了一个具有类似接口和语义的模块。支持以下操作：

-   `maps:new()`: returns a new, empty map.
-   `maps:put(Key, Val, Map)`: adds an entry to a map.
-   `maps:update(Key, Val, Map)`: returns a map with `Key`'s entry modified or raises the error `badarg` if it's not there.
-`maps:update（Key，Val，Map）`：返回一个修改了`Key`'条目的映射，如果不存在，则引发错误`badarg`。
-   `maps:get(Key, Map)` and `maps:find(Key, Map)`: find items in a map. The former returns the value if present or raises the `badkey` error otherwise, whereas the latter returns `` or `error`. Another version of the function is `maps:get(Key, Map, Default)`, which allows to specify the value to return if the key is missing.
-'maps:get（Key，Map）'和'maps:find（Key，Map）'：在地图中查找项目。前者返回值（如果存在）或引发'badkey'错误（否则），而后者返回'`或'error'`。``如果“maps”是另一个键，那么“maps”的默认值是'get version to return。
-   `maps:remove(Key, Map)`: returns a map with the given element deleted. Similarly, `maps:without([Keys], Map)` will delete multiple elements from the returned map. If a key to be deleted isn't present, the functions behave as if they had deleted it --- they return a map without it. The opposite of `maps:without/2` is `maps:with/2`.
-`maps:remove（Key，Map）`：返回一个已删除给定元素的映射。类似地，`maps:without（[Keys]，Map）`将从返回的映射中删除多个元素。如果要删除的密钥不存在，函数的行为就好像删除了它一样——它们返回一个没有该密钥的映射。'maps:without/2'的对立面是'maps:without/2'`。

It also contains the good old functional standards such as `fold/3` and `map/2`, which work similarly to the functions in the `lists` module. Then there is the set of utility functions such as `size/1`, `is_map/1` (also a guard!), `from_list/1` and `to_list/1`, `keys/1` and `values/1`, and set functions like `is_key/2` and `merge/2` to test for membership and fuse maps together.
它还包含良好的旧功能标准，如“fold/3”和“map/2”，其工作原理与“list”模块中的功能类似。还有一组实用函数，比如'size/1'、'is_map/1'（也是一个守卫！）`从_list/1`和_list/1`到_list/1`、`keys/1`和`values/1`，并设置诸如`is_key/2`和`merge/2`等函数，以测试成员资格并将映射融合在一起。

That's not all that exciting, and users want more!

### The Syntax

Despite the promised gains in speed, the most anticipated aspect of maps is the native syntax they have. Here are the different operations compared to their equivalent module call:
尽管承诺会在速度上有所提高，但地图最令人期待的方面是它们的原生语法。以下是与其等效模块调用相比的不同操作：

  Maps Module       Maps Syntax
  ----------------- -----------------------
  `maps:new/1`      `#`
  `maps:put/3`      `Map#`
  `maps:update/3`   `Map#`
  `maps:get/2`      `Map#`
  `maps:find/2`     `# = Map`

Bear in mind that [not all of this syntax has been implemented yet](maps.html#stubby-legs). Fortunately for map users, the pattern matching options go wider than this. It is possible to match more than one item out of a map at once:
请记住[并不是所有这些语法都已经实现]。html#短粗的腿）。幸运的是，对于地图用户来说，模式匹配选项比这更广泛。可以同时从地图中匹配多个项目：

```eshell
1> Pets = #.
#
2> # = Pets.
#
```

Here it's possible to grab the contents of any number of items at a time, regardless of order of keys. You'll note that elements are set with `=>` and matched with `:=`. The `:=` operator can also be used to update an *existing* key in a map:
在这里，无论键的顺序如何，都可以一次抓取任意数量项的内容。您将注意到，元素被设置为“=>”，并与“.”匹配：=`。“：=”运算符还可用于更新地图中的*现有*键：

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
在同一模式中，一个已知键的值可以用来定义一个变量（`Animal`=Map`），因为可能有多个键具有相同的值。

::: note
**Note:** The syntax for accessing a single value (`Map#`) is documented as such in the EEP, but is subject to change in the future once implemented, and might be dropped entirely in favor of different solutions.
**注：*访问单个值（`Map#``的语法在EEP中有相应的记录，但一旦实现，将来可能会发生更改，并且可能会完全放弃，以支持不同的解决方案。
:::

There's something interesting but unrelated that was added with maps. If you remember in [Starting Out For Real](starting-out-for-real.html#list-comprehensions), we introduced list comprehensions:
地图中添加了一些有趣但无关的东西。如果你还记得在[真正开始]（真正开始）中。html#列表理解），我们引入了列表理解：

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
这里，`X:=fog<-Weather`表示一个地图生成器，其形式为`Key:=Val<-map`。这可以像列表生成器和二进制生成器一样组合和替换。地图理解还可以生成新地图：

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
这就是大部分！看到这种新的数据类型加入Erlang zoo非常令人耳目一新，希望用户会欣赏它。

![A bee looking over a map](../img/bee.png "I'm grunting! what a pun, what a pun!")

### The Gritty Details

The details aren't *that* gritty, just a little bit. The inclusion of maps in the language will impact a few things. EEP-43 goes into detail to define potential changes, many somewhat still in the air, for parts such as the distributed Erlang protocol, operator precedence, backwards compatibility, and suggestions to Dialyzer extensions (it is yet to see if the support will go as far as the EEP recommends).
细节不是那么坚韧不拔，只是一点点。在一些地图中包含一些东西会影响。EEP-43详细介绍了分布式Erlang协议、操作员优先级、向后兼容性和透析器扩展建议等部分的潜在变化，其中许多仍在讨论中（目前尚不清楚支持是否会达到EEP建议的程度）。

Many of the changes have been proposed such that the user doesn't need to think very hard about them. One that is unavoidable, however, is sorting. Previously in the book, the sorting order was defined as:
许多修改都是这样提出的，用户不需要仔细考虑。然而，排序是不可避免的。在本书之前，排序顺序被定义为：

``` expand
number < atom < reference < fun < port < pid < tuple < list < bit string
```

Maps now fit in here:

``` expand
number < atom < reference < fun < port < pid < tuple < map < list < bit string
```

Bigger than tuples and smaller than lists. Interestingly enough, maps can be compared to each other based on their keys and values:
比元组大比列表小。有趣的是，地图可以根据其键和值相互比较：

```eshell
2> lists:sort([#]).
[#]
```

The sorting is done similarly to lists and tuples: first by size, then by the elements contained. In the case of maps, these elements are in the sorted order of keys, and breaking ties with the values themselves.
排序类似于列表和元组：首先按大小排序，然后按包含的元素排序。在映射的情况下，这些元素按键的顺序排列，并与值本身断开联系。

::: 
**Don't Drink Too Much Kool-Aid:**\
You may notice that while we can't update the key `1.0` of a map with the key `1`, it is possible to have them both compare as equal that way! This is one of the long-standing warts of Erlang. While it's very convenient to have all numbers compare equal from time to time, for example, when sorting lists so that `1.0` isn't greater than `9121`, this creates confusing expectations when it comes to pattern matching.
您可能会注意到，虽然我们无法更新密钥'1。对于键为“1”的映射，可以通过这种方式将它们进行相等的比较！这是Erlang长期存在的问题之一。然而，让所有的数字时不时地进行相等比较是非常方便的，例如，当对列表进行排序时，`1。0'不大于'9121'，这会在模式匹配方面产生令人困惑的期望。

For example, while we can expect `1` to be equal to `1.0` (although not *strictly equal*, as with `=:=`), we can't expect to pattern match by doing `1 = 1.0`.
例如，我们可以期望'1'等于'1'。0`（虽然不是*严格相等*，就像`=：=`），但我们不能期望通过执行'1=1'来进行模式匹配。0`。

In the case of maps, this means that `Map1 == Map2` isn't a synonym of `Map1 = Map2`. Because Erlang maps respect Erlang sorting order, a map such as `#`, but you won't be able to match them one against the other.
就地图而言，这意味着'Map1==Map2'不是'Map1=Map2'的同义词`。因为Erlang映射尊重Erlang排序顺序，比如“#”之类的映射，但是您无法将它们一一对应。
:::

Be careful, because although the content of this section is written as based on EEP-43, the actual implementation might be lagging behind!
请小心，因为尽管本节的内容是基于EEP-43编写的，但实际实现可能会落后！

## [Stubby Legs for Early Releases]

![A Corgi with a cape, flying](../img/corgi.png)

The maps implementation that came with Erlang 17.x and 18.0 is complete, but only within the confines of the maps module. The major differences come from the syntax. Only a minimal subset of it is available:
Erlang 17附带的地图实现。x和18。0已完成，但仅在“地图”模块的范围内。主要的区别来自语法。只有一个子集可用：

-   literal map declarations (`#`)
-   matching on known keys (`# = SomeMap`)
-   Updating and adding elements with a known key in an existing map (`Map#`)
-   Using variables as keys both when assigning (`X = 3, #`), starting in Erlang 18.0

The rest, including accessing single values (`Map#`), whether in matches or declarations, is still not there. Same fate for map comprehensions and Dialyzer support. In fact, the syntax may change and differ from the EEP in the end.
其余的，包括访问单个值（`Map#`），无论是在匹配还是在声明中，仍然不存在。地图理解和透析器支持同样的命运。实际上，语法可能会发生变化，并最终与EEP不同。

Maps are also still slower than most Erlang developers and the OTP team want them to be. Still, progress is ongoing and it should not take long --- especially compared to how long it's been before maps were added to the language in the first place --- before they get much better.
地图仍然比大多数Erlang开发者和OTP团队希望的慢。地图被添加到第一个位置需要多长时间，尤其是地图被添加到第一个位置。

This early release is still good enough to get familiar with maps, and more importantly, get to know when and where to use them.
这个早期版本仍然足够好，可以熟悉地图，更重要的是，可以知道何时何地使用它们。

## [Mexican Standoff]

Everyone had their opinion on wanting native dictionaries or better records replacement as a priority. When maps were announced, many Erlang developers kind of just assumed that maps, when they came to the language, would solve the problem they wanted solved.
每个人都有自己的观点，希望本地词典或更好的记录作为优先事项。当maps发布时，许多Erlang开发人员都认为maps能够解决他们想要解决的问题。

As such, there is some confusion on how maps should be used in Erlang.

### Maps Vs. Records Vs. Dicts

To get it out of the way directly, maps are a replacement for dicts, not records. This can be confusing. At the beginning of this chapter, I identified common complaints, and it turns out that many of the complaints about records would be fixed by maps:
为了直接解决这个问题，地图取代了dicts，而不是记录。这可能令人困惑。在本章的开头，我指出了常见的投诉，事实证明，许多关于记录的投诉都可以通过地图解决：

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
![墨西哥人一边吃墨西哥玉米卷，一边用两把枪瞄准对方](。。/静态/img/墨西哥对峙。（巴布亚新几内亚）

The score is pretty close. The point of maps being faster isn't necessarily the case yet, but optimizations should bring them to a better level. The OTP team is respecting the old slogan: first make it work, then make it beautiful, and only if you need to, make it fast. They're getting semantics and correctness out of the way first.
比分非常接近。地图更快的意义并不一定是这样，但优化应该会让它们达到更好的水平。OTP团队尊重这句古老的口号：首先让它工作起来，然后让它变得漂亮，只有在你需要的时候，才能让它快速。他们首先要解决语义和正确性问题。

For upgrades with records being marked as 'maybe', this has to do with the `code_change` functionality. A lot of users are annoyed by having to convert records openly when they change versions similar to what we did with [pq_player.erl](static/erlang/processquest/apps/processquest-1.1.0/src/pq_player.erl.html) and its upgrade code. Maps would instead allow us to just add fields as required to the entry and keep going. The counter-argument to that is that a botched up upgrade would crash early with records (a good thing!) whereas a botched up upgrade with maps may just linger on with the equivalent of corrupted data, and not show up until it's too late.
对于记录被标记为“可能”的升级，这与“代码更改”功能有关。许多用户在更改版本时不得不公开转换记录，这让他们很恼火，就像我们对[pq_player]所做的那样。erl]（静态/erlang/processquest/apps/processquest-1。1.。0/src/pq_播放器。呃。html）及其升级代码。相反，“地图”将允许我们只需在条目中添加所需的字段，然后继续。与此相反的观点是，一个糟糕的升级会在有记录时过早崩溃（一件好事！）然而，一个糟糕的地图升级可能只是停留在相当于损坏的数据上，直到为时已晚才出现。

So why is it we should use maps as dicts and not as records? For this, a second table is necessary. This one is about *semantics*, and which data structure or data type a feature applies to:
那么，为什么我们应该把地图当作口述而不是记录呢？为此，需要第二张桌子。这是关于*语义*，以及特性适用于哪种数据结构或数据类型：

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
*\*EEP建议对编译时已知的密钥执行此操作，但没有关于何时或是否会发生这种情况的预计时间。*

This chart makes it rather obvious that despite the similarity in syntax between maps and records, dicts and maps are much closer together *semantically* than maps are to records. Therefore, using maps to replace records would be similar to trying to replace 'structs' in a language like C with a hash map. It's not impossible, but that doesn't mean it's a good idea given they often have different purposes.
这张图表表明，尽管地图和记录在语法上有相似之处，但dicts和地图在语义上比地图和记录更接近。因此，使用映射替换记录类似于尝试用哈希映射替换像C这样的语言中的“structs”。这并非不可能，但这并不意味着这是一个好主意，因为它们通常有不同的目的。

Keys being known at compile time brings advantages with fast access to specific values (faster than what is possible dynamically), additional safety (crash early rather than corrupting state), and easier type checking. These make records absolutely appropriate for a process' internal state, despite the occasional burden of writing a more verbose `code_change` function.
编译时已知的密钥带来了以下优点：快速访问特定值（比动态可能的速度更快）、额外的安全性（崩溃早，而不是破坏状态），以及更容易的类型检查。尽管偶尔会有编写更详细的“code_change”函数的负担，但这些记录绝对适合于流程的内部状态。

On the other hand, where Erlang users would use records to represent complex nested key/value data structures (oddly similar to objects in object-oriented languages) that would frequently cross module boundaries, maps will help a lot. Records were the wrong tool for that job.
另一方面，Erlang用户会使用记录来表示复杂的嵌套键/值数据结构（奇怪地类似于面向对象语言中的对象），这些结构经常会跨越模块边界，映射将非常有用。记录是那项工作的错误工具。

To put it briefly, places where records really felt out of place and cumbersome *could* be replaced by maps, but most record uses *should not* be replaced that way.
简单地说，那些记录确实感觉不合适和笨重的地方可以被地图取代，但大多数记录的使用不应该被地图取代。

::: 
**Don't Drink too Much Kool-Aid:**\
It is often tempting to bring complex nested data structures to the party and use them as one big transparent object to pass in and out of functions or processes, and to then just use pattern matching to extract what you need.
将复杂的嵌套数据结构带到聚会上，并将其用作一个大的透明对象，以便在函数或进程中进行传入和传出，然后仅使用模式匹配来提取所需的内容，这通常是很诱人的。

Remember, however, that there is a cost to doing these things in message passing: Erlang data is copied across processes and working that way might be expensive.
但是，请记住，在消息传递中执行这些操作是有代价的：Erlang数据是跨进程复制的，这样做可能会很昂贵。

Similarly, passing big transparent objects in and out of functions should be done with care. Building a sensible interface (or API) is already difficult; if you marry yourself to your internal data representation, enormous amounts of flexibility for the implementation can be lost.
类似地，在函数中传入和传出大型透明对象时也应小心。构建合理的接口（或API）已经很困难；如果将自己与内部数据表示结合起来，实现的巨大灵活性可能会丢失。

It is better to think about your interfaces and message-based protocols with great care, and to limit the amount of information and responsibility that gets shared around. Maps are a replacement for dicts, not for proper design.
最好仔细考虑您的接口和基于消息的协议，并限制共享的信息量和责任。地图是dicts的替代品，而不是正确的设计。
:::

There are also performance impacts to be expected. Richard O'Keefe mentions it in his proposal:

> You can't make dict good for the record-like uses that frames are meant for without making it bad for its existing uses.
>你不可能让dict在不损害其现有用途的情况下，对帧所要用于的类似于记录的用途有好处。

And the EEP from the OTP team also mentions something similar:

> When comparing maps with records the drawbacks are easily remedied by maps, however the positive effects *\[sic\]* is not as easy to replicate in a built-in data-type where values are determined at runtime instead of at compile time.
>在将映射与记录进行比较时，映射可以很容易地弥补缺点，但是，在内置数据类型中，如果值是在运行时而不是在编译时确定的，那么就不那么容易复制正面影响*\[sic\]*。
>
> -   Being faster than direct-indexing array, where indices and possibly the resulting value are determined at compile time, is hard. In fact it is impossible.
>-比直接索引数组更快，后者的索引和可能的结果值是在编译时确定的，这是很困难的。事实上这是不可能的。
> -   Memory model for maps where the efficiency is near that of records could be achieved by essentially using two tuples, one for keys and one for values as demonstrated in frames. This would impact performance of updates on maps with a large number of entries and thus constrain the capability of a dictionary approach.
>-效率接近记录的地图的内存模型可以通过基本上使用两个元组来实现，一个用于键，一个用于帧中显示的值。这将影响具有大量条目的地图的更新性能，从而限制字典方法的能力。

For the core of your process loop, when you know all keys that should exist, a record would be a smart choice, performance-wise.
对于流程循环的核心，当您知道应该存在的所有密钥时，记录将是一个明智的选择，性能方面也是如此。

### Maps Vs. Proplists

One thing that maps may beat are proplists. A proplist is a rather simple data structure that is extremely well-suited for options passed to a module.
地图可能打败的一件事是道具列表。proplist是一种非常简单的数据结构，非常适合传递给模块的选项。

The `inet:setopts/2` call can take a list of options of the form `[`.

![a scale measuring proplists vs. maps](../img/scale.png)

Maps, with their single-key access (whenever implemented) will represent a similar way to define properties. For example, `[`).
带有单键访问（无论何时实现）的映射将代表定义属性的类似方式。例如，`[`）。

It's somewhat to be expected that option lists that are *mostly* pairs will be replaced by maps. On the other hand, literal options such as `read`, `write`, or `append` will remain much nicer with proplists, from the user's perspective.
在某种程度上可以预期，大多数成对的选项列表将被地图所取代。另一方面，从用户的角度来看，诸如'read'、'write'或'append'等文字选项在proplist中仍然会更好。

Given that right now most functions that require options use proplists, keeping things going that way forward may be interesting for consistency's sake. On the other hand, when the options are mostly pairs, using the `proplists` module can get cumbersome fast. Ultimately, the author of a library will have to make a decision between what could be internal clarity of implementation or general ecosystem consistency. Alternatively, the author could decide to support both ways of doing things at once to provide a graceful path towards deprecation of proplists, if that's how they feel about it.
考虑到目前大多数需要选项的函数都使用PropList，为了保持一致性，保持这种方式可能很有趣。另一方面，当选项大多是成对的时，使用“PropList”模块可能会很快变得很麻烦。最终，图书馆的作者必须在实现的内部清晰性和总体生态系统的一致性之间做出决定。或者，作者可以决定同时支持这两种方式，以提供一种优雅的方式来反对PropList，如果这是他们的感受的话。

On the other hand, functions that used to pass proplists as a return value from functions should probably switch to maps. There is little legacy there, and usability should be much better for users that way.
另一方面，用于将PropList作为函数返回值传递的函数可能应该切换到maps。那里几乎没有遗留下来的东西，这样对用户来说，可用性应该更好。

::: note
**Note:** maps can use a clever trick to set many default values at once easily. Whereas proplists would require the use of `proplists:get_value(Key, List, Default)`, maps can use their `merge/2` function.
**注：*“地图”可以使用巧妙的技巧同时轻松设置许多默认值。PropList需要使用“PropList:get_value（Key，List，Default）”，而maps可以使用其“merge/2”功能。

According to the specification, `merge/2` will fuse two maps together, and if two keys are the same, the second map's value will prevail. This allows to call `maps:merge(Default, YourMap)` and get the desired values. For example, `maps:merge(#`.
根据规范，`merge/2`将两个映射融合在一起，如果两个键相同，则以第二个映射的值为准。这允许调用'maps:merge（默认，YourMap）'并获得所需的值。例如，`maps:merge(#`。

This makes for an extremely convenient way to attribute default values manually, and then you can just use the resulting map without worrying about missing keys.
这提供了一种非常方便的方法，可以手动为默认值设置属性，然后您可以直接使用生成的贴图，而不用担心缺少关键点。
:::

## [How This Book Would Be Revised For Maps]

I wanted to add this section because I do not necessarily have the time to update the book in its entirety to retrofit maps back in at this point in time.
我想添加这一部分，因为我不一定有时间更新整本书，以便在此时重新修改地图。

For most of the book, however, little would change. It would mostly be replacing calls to the `dict` module and to `gb_trees` (in cases where the smallest/largest values aren't frequently required) with inline maps syntax. Pretty much none of the records used within the book would be changed, for semantics' sake.
然而，对于这本书的大部分内容来说，几乎没有什么变化。它主要是用内联映射语法替换对“dict”模块和“gb_树”的调用（在不经常需要最小/最大值的情况下）。为了语义起见，书中使用的记录几乎都不会被更改。

I may revisit a few modules once maps are stable and fully implemented, but in the mean time, many examples would be impractical to write that way given the partial maps implementation.
一旦maps稳定并完全实现，我可能会重新访问一些模块，但同时，考虑到部分maps的实现，以这种方式编写许多示例是不切实际的。
