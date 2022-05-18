# A Short Visit to Common Data Structures

## [Won't be too long, promised!]

Chances are you now understand the functional subset of Erlang pretty well and could read many programs without a problem. However, I could bet it's still a bit hard to think about how to build a real useful program even though the last chapter was about solving problems in a functional manner. I'm saying this because it's how I felt like at about that point in my learning, but if you're doing better, congratulations!
现在，您很可能已经很好地理解了Erlang的函数子集，并且可以毫无问题地阅读许多程序。然而，我敢打赌，尽管最后一章是关于用函数的方式解决问题，但思考如何构建一个真正有用的程序仍然有点困难。我之所以这么说，是因为这是我在学习过程中的感受，但如果你做得更好，恭喜你！

Anyway, the point I'm coming to is that we've seen a bunch of things: most basic data types, the shell, how to write modules and functions (with recursion), different ways to compile, control the flow of the program, handle exceptions, abstract away some common operations, etc. We've also seen how to store data with tuples, lists and an incomplete implementation of a binary search tree. What we haven't seen is the other data structures provided to the programmer in the Erlang standard library.
总之，我要说的是，我们已经看到了很多东西：最基本的数据类型、shell、如何编写模块和函数（使用递归）、不同的编译方式、控制程序流、处理异常、抽象掉一些常见操作等等。我们还了解了如何使用元组、列表和二叉搜索树的不完整实现来存储数据。我们没有看到的是Erlang标准库中提供给程序员的其他数据结构。

![a phonograph](../img/record-player.png "tee hee, broken records")

## [Records]

Records are, first of all, a hack. They are more or less an afterthought to the language and can have their share of inconveniences. I'll cover that later. They're still pretty useful whenever you have a small data structure where you want to access the attributes by name directly. As such, Erlang records are a lot like structs in C (if you know C.)
首先，记录是一种黑客行为。它们或多或少都是语言的后遗症，可能会带来一些不便。我以后再谈。当你有一个小的数据结构，你想直接通过名字访问属性时，它们仍然非常有用。因此，Erlang记录与C中的结构非常相似（如果您了解C的话）。)

They're declared as module attributes in the following manner:

```erl
-module(records).
-compile(export_all).

-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).
```

So here we have a record representing robots with 4 fields: name, type, hobbies and details. There is also a default value for type and details, `industrial` and `[]`, respectively. Here's how to declare a record in the module [records](static/erlang/records.erl.html):
这里我们有一个记录，代表机器人，有4个字段：名称、类型、爱好和细节。type和details还有一个默认值，分别是'industrial'和'[]'。下面是如何在模块[records]（static/erlang/records）中声明记录。呃。html）：

```erl
first_robot() ->
    #robot{name="Mechatron",
           type=handmade, 
           details=["Moved by a small man inside"]}.
```

And running the code:

```eshell
1> c(records).

2> records:first_robot().
{robot,"Mechatron",handmade,undefined,
       ["Moved by a small man inside"]}
```

Woops! Here comes the hack! Erlang records are just syntactic sugar on top of tuples. Fortunately, there's a way to make it better. The Erlang shell has a command `rr(Module)` that lets you load record definitions from `Module`:
呜呜！黑客来了！Erlang记录只是元组之上的语法糖。幸运的是，有办法让它变得更好。Erlang shell有一个命令'rr（Module）'，可以从'Module'加载记录定义：

```eshell
3> rr(records).
[robot]
4> records:first_robot().         
#robot{name = "Mechatron",type = handmade,
       hobbies = undefined,
       details = ["Moved by a small man inside"]}
```

Ah there! This makes it much easier to work with records that way. You'll notice that in `first_robot/0`, we had not defined the `hobbies` field and it had no default value in its declaration. Erlang, by defaults, sets the value to `undefined` for you.
啊！这使得以这种方式处理记录变得更加容易。您会注意到，在“first_robot/0”中，我们没有定义“嗜好”字段，并且在其声明中没有默认值。默认情况下，Erlang会将值设置为“undefined”。

To see the behavior of the defaults we set in the `robot` definition, let's compile the following function:
要查看我们在“robot”定义中设置的默认值的行为，让我们编译以下函数：

```erl
car_factory(CorpName) ->
    #robot.
```

And run it:

```eshell
5> c(records).

6> records:car_factory("Jokeswagen").
#robot{name = "Jokeswagen",type = industrial,
       hobbies = "building cars",details = []}
```

And we have an industrial robot that likes to spend time building cars.

::: note
**Note:** The function `rr()` can take more than a module name: it can take a wildcard (like `rr("*")`) and also a list as a second argument to specify which records to load.
**注意：*函数'rr（）'可以使用不止一个模块名：它可以使用一个通配符（如'rr（“*”））和一个列表作为第二个参数来指定要加载的记录。

There are a few other functions to deal with records in the shell: `rd(Name, Definition)` lets you define a record in a manner similar to the `-record(Name, Definition)` used in our module. You can use `rf()` to 'unload' all records, or `rf(Name)` or `rf([Names])` to get rid of specific definitions.
shell中还有一些其他函数可以处理记录：`rd（Name，Definition）`允许您以类似于模块中使用的`-record（Name，Definition）`的方式定义记录。您可以使用'rf（）'来“卸载”所有记录，或者使用'rf（Name）'或'rf（[Names]）来删除特定的定义。

You can use `rl()` to print all record definitions in a way you could copy-paste into the module or use `rl(Name)` or `rl([Names])` to restrict it to specific records.
可以使用`rl（）`打印所有记录定义，方法是将粘贴复制到模块中，或者使用`rl（Name）`或`rl（[Name]）`将其限制为特定的记录。

Finally, `rp(Term)` lets you convert a tuple to a record (given the definition exists).
:::

Writing records alone won't do much. We need a way to extract values from them. There are basically two ways to do this. The first one is with a special 'dot syntax'. Assuming you have the record definition for robots loaded:
光写记录没什么用。我们需要一种从中提取价值的方法。基本上有两种方法。第一个是特殊的“点语法”。假设已加载机器人的记录定义：

```eshell
5> Crusher = #robot. 
#robot{name = "Crusher",type = industrial,
       hobbies = ["Crushing people","petting cats"],
       details = []}
6> Crusher#robot.hobbies.
["Crushing people","petting cats"]
```

Ugh, not a pretty syntax. This is due to the nature of records as tuples. Because they're just some kind of compiler trick, you have to keep keywords around defining what record goes with what variable, hence the `#robot` part of `Crusher#robot.hobbies`. It's sad, but there's no way out of it. Worse than that, nested records get pretty ugly:
这可不是个好主意。这是由于记录作为元组的性质。因为它们只是某种编译器技巧，你必须保留关键字来定义什么记录和什么变量，因此“crumer”robot中的“robot”部分。爱好`。这很悲伤，但是没有办法摆脱它。更糟糕的是，嵌套记录变得非常丑陋：

```eshell
7> NestedBot = #robot.
#robot{name = undefined,type = industrial,
       hobbies = undefined,
       details = #robot{name = "erNest",type = industrial,
                        hobbies = undefined,details = []}}
8> (NestedBot#robot.details)#robot.name. 
"erNest"
```

And yes, the parentheses are mandatory.

::: 
**Update:**\
Starting with revision R14A, it is now possible to nest records without the parentheses. The `NestedBot` example above could also be written as `NestedRobot#robot.details#robot.name` and work the same.
从R14A修订版开始，现在可以嵌套不带括号的记录。上面的“NestedBot”示例也可以写成“NestedBot#robot”。详细信息#机器人。名字和工作都一样。
:::

To further show the dependence of records on tuples, see the following:

```eshell
9> #robot.type.
3
```

What this outputs is which element of the underlying tuple it is.

One saving feature of records is the possibility to use them in function heads to pattern match and also in guards. Declare a new record as follows on top of the file, and then add the functions under:
记录的一个保存功能是可以在功能头中使用它们进行模式匹配，也可以在警卫中使用。在文件顶部声明一条新记录，如下所示，然后在下面添加函数：

```erl
-record(user, ).

%% use pattern matching to filter
admin_panel(#user) ->
    Name ++ " is allowed!";
admin_panel(#user) ->
    Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user) when U#user.age >= 18 ->
    %% Show stuff that can't be written in such a text
    allowed;
adult_section(_) ->
    %% redirect to sesame street site
    forbidden.
```

The syntax to bind a variable to any field of a record is demonstrated in the `admin_panel/1` function (it's possible to bind variables to more than one field). An important thing to note about the `adult_section/1` function is that you need to do `SomeVar = #some_record` in order to bind the whole record to a variable. Then we do the compiling as usual:
“admin_panel/1”函数演示了将变量绑定到记录的任何字段的语法（可以将变量绑定到多个字段）。关于`成人_section/1`函数，需要注意的一点是，为了将整个记录绑定到一个变量，需要执行` SomeVar=#someu record'。然后我们像往常一样进行编译：

```eshell
10> c(records).

11> rr(records).
[robot,user]
12> records:admin_panel(#user).
"ferd is allowed!"
13> records:admin_panel(#user). 
"you is not allowed"
14> records:adult_section(#user).
allowed
15> records:adult_section(#user).
forbidden
```

What this lets us see is how it is not necessary to match on all parts of the tuple or even know how many there are when writing the function: we can only match on the age or the group if that's what's needed and forget about all the rest of the structure. If we were to use a normal tuple, the function definition might need to look a bit like `function() -> ...`. Then, whenever someone decides to add an element to the tuple, someone else (probably angry about it all) would need to go around and update all functions where that tuple is used.
这让我们看到，在编写函数时，不需要匹配元组的所有部分，甚至不需要知道有多少部分：我们只能在需要的情况下匹配年龄或组，而忽略结构的所有其他部分。如果要使用普通元组，函数定义可能需要看起来有点像'function（）->。。。`。然后，每当有人决定向元组中添加一个元素时，其他人（可能对这一切都感到愤怒）就需要到处更新使用该元组的所有函数。

The following function illustrates how to update a record (they wouldn't be very useful otherwise):

```erl
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot,
    .
```

And then:

```eshell
16> c(records).

17> records:repairman(#robot).
{repaired,#robot{name = "Ulbert",type = industrial,
                 hobbies = ["trying to have feelings"],
                 details = ["Repaired by repairman"]}}
```

And you can see my robot has been repaired. The syntax to update records is a bit special here. It looks like we're updating the record in place (`Rob#robot`) but it's all compiler trickery to call the underlying `erlang:setelement/3` function.
你可以看到我的机器人已经修好了。这里更新记录的语法有点特殊。看起来我们正在就地更新记录（'Rob#robot'），但调用底层的'erlang:setelement/3'函数完全是编译器的诡计。

One last thing about records. Because they're pretty useful and code duplication is annoying, Erlang programmers frequently share records across modules with the help of *header files*. Erlang header files are pretty similar to their C counter-part: they're nothing but a snippet of code that gets added to the module as if it were written there in the first place. Create a file named [records.hrl](static/erlang/records.hrl.html) with the following content:
关于唱片的最后一件事。因为它们非常有用，而且代码复制很烦人，Erlang程序员经常在*头文件的帮助下跨模块共享记录*。Erlang头文件与它们的C计数器部分非常相似：它们只是一段代码，被添加到模块中，就好像它最初是在模块中编写的一样。创建一个名为[records]的文件。hrl]（静态/erlang/records）。hrl。html），并包含以下内容：

```erl
%% this is a .hrl (header) file.
-record(included, {some_field,
                   some_default = "yeah!",
                   unimaginative_name}).
```

To include it in [records.erl](static/erlang/records.erl.html), just add the following line to the module:
把它列入[记录]。erl]（静态/erlang/records）。呃。html），只需在模块中添加以下行：

```erl
-include("records.hrl").
```

And then the following function to try it:

```erl
included() -> #included.
```

Now, try it as usual:

```eshell
18> c(records).

19> rr(records).
[included,robot,user]
20> records:included().
#included{some_field = "Some value",some_default = "yeah!",
          unimaginative_name = undefined}
```

Hooray! That's about it for records; they're ugly but useful. Their syntax is not pretty, they're not much but a hack, but they're relatively important for the maintainability of your code.
好极了这就是记录；它们很难看，但很有用。它们的语法并不漂亮，它们只是一种黑客行为，但对于代码的可维护性来说，它们相对重要。

::: note
**Note:** You will often see open source software using the method shown here of having a project-wide `.hrl` file for records that are shared across all modules. While I felt obligated to document this use, I strongly recommend that you keep all record definitions local, within one module. If you want some other module to look at a record's innards, write functions to access its fields and keep its details as private as possible. This helps prevent name clashes, avoids problems when upgrading code, and just generally improves the readability and maintainability of your code.
**注：**您经常会看到使用此处所示的方法在项目范围内开发开源软件`。hrl`所有模块共享的记录文件。虽然我觉得有义务记录这种使用，但我强烈建议您将所有记录定义保存在一个模块内的本地。如果您希望其他模块查看记录的内部，请编写函数来访问其字段，并尽可能保持其详细信息的私密性。这有助于防止名称冲突，避免升级代码时出现问题，并且通常会提高代码的可读性和可维护性。
:::

## [Key-Value Stores]

![key and keyhole, another terrible pun](../img/key.png)

I've had you build a tree back a few chapters, and the use was to use it as a key-value store for an address book. That book sucked: we couldn't delete or convert it to anything useful. It was a good demonstration of recursion, but not much more. Now is the time to introduce you to a bunch of useful data structures and modules to store data under a certain key. I won't define what every function does nor go through all the modules. I will simply link to the doc pages. Consider me as someone responsible about 'raising awareness about key-value stores in Erlang' or something. Sounds like a good title. I just need one of these ribbons.
我已经让你在几章前建立了一棵树，其用途是将它用作地址簿的键值存储。那本书糟透了：我们无法删除或转换成任何有用的东西。这是递归的一个很好的演示，但仅此而已。现在是向您介绍一些有用的数据结构和模块的时候了，这些数据结构和模块将数据存储在特定的密钥下。我不会定义每个函数的功能，也不会遍历所有模块。我将简单地链接到文档页面。把我看作是负责“提高对Erlang关键价值商店的认识”之类的人。听起来是个不错的标题。我只需要一条缎带。

For small amounts of data, there are basically two data structures that can be used. The first one is called a *proplist*. A proplist is any list of tuples of the form `[ module. It contains functions such as `proplists:delete/2`, `proplists:get_value/2`, `proplists:get_all_values/2`, `proplists:lookup/2` and `proplists:lookup_all/2`.
对于少量数据，基本上可以使用两种数据结构。第一个被称为*proplist*。proplist是`[module]形式的元组列表。它包含诸如“PropList:delete/2”、“PropList:get_value/2”、“PropList:get_all_value/2”、“PropList:lookup/2”和“PropList:lookup_all/2”等函数`。

You'll notice there is no function to add or update an element of the list. This shows how loosely defined proplists are as a data structure. To get these functionalities, you must cons your element manually (`[NewElement|OldList]`) and use functions such as `lists:keyreplace/4`. Using two modules for one small data structure is not the cleanest thing, but because proplists are so loosely defined, they're often used to deal with configuration lists, and general description of a given item. Proplists are not exactly complete data structures. They're more of a common pattern that appears when using lists and tuples to represent some object or item; the proplists module is a bit of a toolbox over such a pattern.
您会注意到，没有添加或更新列表元素的函数。这显示了作为一个数据结构，PropList的定义是多么松散。要获得这些功能，必须手动控制元素（“[NewElement|OldList]”），并使用诸如“lists:keyplace/4”之类的函数`。在一个小数据结构中使用两个模块并不是最干净的事情，但由于PropList的定义非常松散，它们通常用于处理配置列表和给定项的一般描述。PropList并不是完全完整的数据结构。它们更像是使用列表和元组来表示某个对象或项目时出现的一种常见模式；PropList模块在这种模式上有点像工具箱。

If you do want a more complete key-value store for small amounts of data, the [orddict](http://erldocs.com/18.0/stdlib/orddict.html) module is what you need. Orddicts (ordered dictionaries) are proplists with a taste for formality. Each key can be there once, the whole list is sorted for faster average lookup, etc. Common functions for the CRUD usage include `orddict:store/3`, `orddict:find/2` (when you do not know whether the key is in the dictionaries), `orddict:fetch/2` (when you know it is there or that it **must** be there) and `orddict:erase/2`.
如果您确实希望为少量数据提供更完整的键值存储，[orddict](http://erldocs。com/18。0/stdlib/orddict。html）模块是您所需要的。Orddicts（有序字典）是一种讲究形式的道具列表。每个键可以出现一次，整个列表会被排序，以便更快地进行平均查找，等等。CRUD用法的常见函数包括`orddict:store/3`、`orddict:find/2`（当您不知道密钥是否在字典中时）、`orddict:fetch/2`（当您知道它在那里或它**必须**在那里时）和`orddict:erase/2``。

![A dictionary with the definition of 'Awesome' being 'it's you!'](../img/dict.png "Dictionary says: Awesome -- it's you!")
![一本定义为“就是你！”的字典](。。/静态/img/dict。png“字典上说：太棒了，是你！”）

Orddicts are a generally good compromise between complexity and efficiency up to about 75 elements (see [my benchmark](static/erlang/keyval_benchmark.erl.html)). After that amount, you should switch to different key-value stores.
Orddict通常是复杂度和效率之间的一个很好的折衷方案，最多有75个元素（请参见[my benchmark]（static/erlang/keyval_benchmark）。呃。html）。在这个数量之后，你应该切换到不同的键值存储。

There are basically two key-value structures/modules to deal with larger amounts of data: [dicts](http://erldocs.com/18.0/stdlib/dict.html). Dictionaries have the same interface as orddicts: `dict:store/3`, `dict:find/2`, `dict:fetch/2`, `dict:erase/2` and every other function, such as `dict:map/2` and `dict:fold/2` (pretty useful to work on the whole data structure!) Dicts are thus very good choices to scale orddicts up whenever it is needed.
基本上有两个键值结构/模块来处理大量数据：[dicts](http://erldocs。com/18。0/stdlib/dict。（html）。字典与orddicts有相同的接口：`dict:store/3`、`dict:find/2`、`dict:fetch/2`、`dict:erase/2`以及所有其他功能，例如`dict:map/2`和`dict:fold/2`（对整个数据结构非常有用！）因此，无论何时，只要需要，都是很好的选择。

General Balanced Trees, on the other hand, have a bunch more functions leaving you more direct control over how the structure is to be used. There are basically two modes for gb_trees: the mode where you know your structure in and out (I call this the 'smart mode'), and the mode where you can't assume much about it (I call this one the 'naive mode'). In naive mode, the functions are `gb_trees:enter/3`, `gb_trees:lookup/2` and `gb_trees:delete_any/2`. The related smart functions are `gb_trees:insert/3`, `gb_trees:get/2`, `gb_trees:update/3` and `gb_trees:delete/2`. There is also `gb_trees:map/2`, which is always a nice thing when you need it.
另一方面，一般平衡树有更多的功能，让你可以更直接地控制结构的使用方式。gb_树基本上有两种模式：一种是你知道自己的结构的模式（我称之为“智能模式”），另一种是你不能对它做太多假设的模式（我称之为“天真模式”）。在naive模式下，函数是'gb_trees:enter/3'、'gb_trees:lookup/2'和'gb_trees:delete_any/2'`。相关的智能功能有'gb_树：插入/3'、'gb_树：获取/2'、'gb_树：更新/3'和'gb_树：删除/2'`。还有“gb_trees:map/2”，当你需要它时，它总是一件好事。

The disadvantage of 'naive' functions over 'smart' ones is that because gb_trees are balanced trees, whenever you insert a new element (or delete a bunch), it might be possible that the tree will need to balance itself. This can take time and memory (even in useless checks just to make sure). The 'smart' function all assume that the key is present in the tree: this lets you skip all the safety checks and results in faster times.
“naive”函数比“smart”函数的缺点是，由于gb_树是平衡树，因此每当插入新元素（或删除一组元素）时，可能需要平衡树本身。这可能需要时间和内存（即使是为了确保安全而进行的无用检查）。“智能”功能都假设密钥存在于树中：这可以让你跳过所有安全检查，从而加快时间。

When should you use gb_trees over dicts? Well, it's not a clear decision. As the [benchmark module](static/erlang/keyval_benchmark.erl.html) I have written will show, gb_trees and dicts have somewhat similar performances in many respects. However, the benchmark demonstrates that dicts have the best read speeds while the gb_trees tend to be a little quicker on other operations. You can judge based on your own needs which one would be the best.
什么时候应该在dicts上使用gb_树？嗯，这不是一个明确的决定。作为[基准模块]（静态/erlang/keyval_基准。呃。html）我写过的文章将显示，gb_树和DICT在很多方面都有类似的表现。然而，基准测试表明，DICT具有最好的读取速度，而gb_树在其他操作中往往更快一些。你可以根据自己的需要来判断哪一个是最好的。

Oh and also note that while dicts have a fold function, gb_trees don't: they instead have an *iterator* function, which returns a bit of the tree on which you can call `gb_trees:next(Iterator)` to get the following values in order. What this means is that you need to write your own recursive functions on top of gb_trees rather than use a generic fold. On the other hand, gb_trees let you have quick access to the smallest and largest elements of the structure with `gb_trees:smallest/1` and `gb_trees:largest/1`.
哦，还要注意的是，虽然dict有一个fold函数，但gb_树没有：它们有一个*iterator*函数，它返回树的一部分，您可以在树上调用`gb_树：next（iterator）`来按顺序获得以下值。这意味着您需要在gb_树上编写自己的递归函数，而不是使用通用折叠。另一方面，使用“gb_树：最小的/1”和“gb_树：最大的/1”，可以快速访问结构中最小和最大的元素`。

I would therefore say that your application's needs is what should govern which key-value store to choose. Different factors such as how much data you've got to store, what you need to do with it and whatnot all have their importance. Measure, profile and benchmark to make sure.
因此，我想说，应用程序的需求应该决定选择哪个键值存储。不同的因素，比如你需要存储多少数据，你需要用它做什么，等等，都有其重要性。测量、配置和基准以确保。

::: note
**Note:** some special key-value stores exist to deal with resources of different size. Such stores are [ETS tables](http://erldocs.com/18.0/stdlib/ets.html "ETS tables are pretty neat!"). However, their use is strongly related to the concepts of multiple processes and distribution. Because of this, they'll only be approached later on. I'm leaving this as a reference to pique your curiosity and for those interested.
**注：*存在一些特殊的键值存储，用于处理不同大小的资源。这样的商店是[ETS表格](http://erldocs。com/18。0/stdlib/ets。html“ETS表格非常整洁！”）。然而，它们的使用与多进程和分布的概念密切相关。因此，他们只能在以后接触。我留下这篇文章是为了激起你的好奇心，也为了那些感兴趣的人。
:::

::: 
**Update:**\
Starting with version 17.0, the language supports a new native key-value data type, described in [Postscript: Maps](maps.html). They should be the new de-facto replacement for `dict`s.
从第17版开始。0时，该语言支持新的本机键值数据类型，如[Postscript:Maps]（映射）中所述。（html）。它们应该成为“dict”的新的事实上的替代品。
:::

## [Arrays]

But what about code that requires data structures with nothing but numeric keys? Well for that, there are [arrays](http://erldocs.com/18.0/stdlib/array.html). They allow you to access elements with numerical indices and to fold over the whole structure while possibly ignoring undefined slots.
但对于只需要数字键的数据结构的代码呢？对于这一点，有[数组](http://erldocs。com/18。0/stdlib/阵列。（html）。它们允许您访问带有数字索引的元素，并折叠整个结构，同时可能忽略未定义的插槽。

::: 
**Don't drink too much kool-aid:**\
Erlang arrays, at the opposite of their imperative counterparts, are not able to have such things as constant-time insertion or lookup. Because they're usually slower than those in languages which support destructive assignment and that the style of programming done with Erlang doesn't necessary lend itself too well to arrays and matrices, they are rarely used in practice.
Erlang数组与它们的命令式数组相反，它们不能有常数时间插入或查找之类的功能。因为它们通常比支持破坏性赋值的语言慢，而且用Erlang完成的编程风格不一定适合数组和矩阵，所以它们很少在实践中使用。

Generally, Erlang programmers who need to do matrix manipulations and other uses requiring arrays tend to use concepts called [Ports](http://www.erlang.org/doc/tutorial/c_port.html) (Experimental, R13B03+).
通常，需要进行矩阵操作和其他需要数组的使用的Erlang程序员倾向于使用称为[端口]的概念(http://www。二郎。org/doc/tutorial/c_port。html）（实验版，R13B03+）。

Arrays are also weird in the sense that they're one of the few data structures to be 0-indexed (at the opposite of tuples or lists), along with indexing in the [regular expressions module](http://erldocs.com/18.0/stdlib/re.html). Be careful with them.
数组也很奇怪，因为它们是为数不多的要进行0索引（与元组或列表相反）的数据结构之一，同时也是[regular expressions module]中的索引(http://erldocs。com/18。0/stdlib/re。（html）。小心他们。
:::

## [A Set of Sets]

![a swingSET](../img/swingset.png "Sometimes I lie awake in my bed at night. Then I think about the puns I make. And I cry with shame")
![摇摆舞](。。/静态/img/旋转设置。png“有时我晚上躺在床上睡不着。然后我想到我的双关语。我羞愧地哭泣

If you've ever studied set theory in whatever mathematics class you have an idea about what sets can do. If you haven't, you might want to skip over this. However, I'll just say that sets are groups of unique elements that you can compare and operate on: find which elements are in two groups, in none of them, only in one or the other, etc. There are more advanced operations letting you define relations and operate on these relations and much more. I'm not going to dive into the theory (again, it's out of the scope of this book) so I'll just describe them as it is.
如果你曾经在任何一门数学课上学习过集合论，你就会对集合的功能有一个概念。如果你没有，你可能想跳过这个。然而，我只想说集合是一组独特的元素，你可以对它们进行比较和操作：找出哪些元素在两个组中，没有一个组中，只有一个或另一个组中，等等。还有更高级的操作可以让你定义关系，并对这些关系进行操作等等。我不打算深入探讨这个理论（同样，它不在本书的范围内），所以我只会如实地描述它们。

There are 4 main modules to deal with sets in Erlang. This is a bit weird at first, but it makes more sense once you realize that it's because it was agreed by implementers that there was no 'best' way to build a set. The four modules are [ordsets](http://erldocs.com/18.0/stdlib/ordsets.html) (sets of sets):
Erlang中有4个主要模块用于处理集合。这一点一开始有点奇怪，但一旦你意识到这是因为实现者一致认为没有“最佳”方法来构建一个集合，它就更有意义了。这四个模块是[ordsets](http://erldocs。com/18。0/stdlib/ordset。html）（集合）：

ordsets
:   Ordsets are implemented as a sorted list. They're mainly useful for small sets, are the slowest kind of set, but they have the simplest and most readable representation of all sets. There are standard functions for them such as `ordsets:new/0`, `ordsets:is_element/2`, `ordsets:add_element/2`, `ordsets:del_element/2`, `ordsets:union/1`, `ordsets:intersection/1`, and a bunch more.
：ordset作为排序列表实现。它们主要用于小集合，是最慢的集合，但它们是所有集合中最简单、最可读的表示。它们有一些标准函数，比如“ordset:new/0”、“ordset:is_element/2”、“ordset:add_element/2”、“ordset:del_element/2”、“ordset:union/1”、“ordset:intersection/1”等等。

sets
:   Sets (the module) is implemented on top of a structure really similar to the one used in `dict`. They implement the same interface as ordsets, but they're going to scale much better. Like dictionaries, they're especially good for read-intensive manipulations, like checking whether some element is part of the set or not.
：set（模块）是在与dict中使用的结构非常相似的结构上实现的`。它们实现了与Ordset相同的接口，但它们的可扩展性会更好。像字典一样，它们特别适合阅读密集型操作，比如检查某个元素是否是集合的一部分。

gb_sets
:   Gb_sets themselves are constructed above a General Balanced Tree structure similar to the one used in the gb_trees module. gb_sets are to sets what gb_tree is to dict; an implementation that is faster when considering operations different than reading, leaving you with more control. While gb_sets implement the same interface as sets and ordsets, they also add more functions. Like gb_trees, you have smart vs. naive functions, iterators, quick access to the smallest and largest values, etc.
：Gb_集本身是在一个与Gb_树模块中使用的结构类似的一般平衡树结构之上构建的。gb_集是用来设置gb_树是用来记录的；在考虑与读取不同的操作时，实现速度更快，让您拥有更多控制权。虽然gb_集合实现了与集合和Ordset相同的接口，但它们也添加了更多功能。就像gb_树一样，你有智能vs。朴素的函数、迭代器、快速访问最小值和最大值等。

sofs
:   Sets of sets (sofs) are implemented with sorted lists, stuck inside a tuple with some metadata. They're the module to use if you want to have full control over relationships between sets, families, enforce set types, etc. They're really what you want if you need mathematics concept rather than 'just' groups of unique elements.
：集合集合（SOF）是通过排序列表实现的，它们被固定在带有一些元数据的元组中。如果你想完全控制集合、族、强制集合类型等之间的关系，可以使用它们。如果你需要数学概念，而不是“仅仅”一组独特的元素，它们就是你真正想要的。

::: 
**Don't drink too much kool-aid:**\
While such a variety can be seen as something great, some implementation details can be downright frustrating. As an example, gb_sets, ordsets and sofs all use the `==` operator to compare values: if you have the numbers `2`, they'll both end up seen as the same one.
虽然这种多样性可以被视为伟大的东西，但一些实现细节可能会让人彻底失望。例如，gb_集、ordsets和SOF都使用“==”运算符来比较值：如果有数字“2”，它们最终都会被视为同一个数字。

However, sets (the module) uses the `=:=` operator, which means you can't necessarily switch over every implementation as you wish. There are cases where you need one precise behavior and at that point, you might lose the benefit of having multiple implementations.
但是，sets（模块）使用“=：=”运算符，这意味着您不必按照自己的意愿切换每个实现。在某些情况下，您需要一个精确的行为，此时，您可能会失去拥有多个实现的好处。
:::

It's a bit confusing to have that many options available. Björn Gustavsson, from the Erlang/OTP team and programmer of [Wings3D](http://www.wings3d.com/ "Impressive and unexpected work of Erlang").)
有这么多的选择让人有点困惑。Björn Gustavsson，来自Erlang/OTP团队，[Wings3D]的程序员(http://www。翅膀3d。com/“Erlang令人印象深刻且出人意料的作品”）。)

In any case, like for key-value stores, the best solution is usually to benchmark and see what fits your application better.
在任何情况下，就像关键价值商店一样，最好的解决方案通常是基准测试，看看什么更适合您的应用程序。

## [Directed Graphs]

There is one other data structure that I want to mention here (not that there are not more than what's mentioned in this chapter, on the contrary): [directed graphs](http://en.wikipedia.org/wiki/Directed_graph). Again, this data structure is more for readers who already know the mathematical theory that goes with it.
我想在这里提到另一个数据结构（并不是说没有比本章提到的更多的数据结构，相反）：[有向图](http://en。维基百科。_/org/wiki）。同样，这种数据结构更适合那些已经了解与之相关的数学理论的读者。

Directed graphs in Erlang are implemented as two modules, [digraph](http://erldocs.com/18.0/stdlib/digraph.html). The digraph module basically allows the construction and modification of a directed graph: manipulating edges and vertices, finding paths and cycles, etc. On the other hand, digraph_utils allows you to navigate a graph (postorder, preorder), testing for cycles, arborescences or trees, finding neighbors, and so on.
Erlang中的有向图实现为两个模块[digraph](http://erldocs。com/18。0/stdlib/digraph。（html）。有向图模块基本上允许构造和修改有向图：操作边和顶点，查找路径和循环等。另一方面，有向图_utils允许您导航一个图（后序、前序），测试循环、树形或树，查找邻居，等等。

Because directed graphs are closely related to set theory, the 'sofs' module contains a few functions letting you convert [families to digraphs](http://erldocs.com/18.0/stdlib/sofs.html#family_to_digraph/2).
由于有向图与集合论密切相关，“sofs”模块包含几个函数，可以将[族转换为有向图](http://erldocs。com/18。0/stdlib/sofs。html#家庭_至_有向图/2）。

## [Queues]

The [queue module](http://erldocs.com/18.0/stdlib/queue.html)) queue:

![Drawing representing the implementation of a functional queue](../img/fifo.png "Total reuse from the 'Types or Lack Thereof' chapter!")
![表示功能队列实现的图纸](。。/静态/img/fifo。png“从‘类型或缺乏类型’一章中完全重用！”）

They're implemented a bit as illustrated above: two lists (in this context, stacks) that allow to both append and prepend elements rapidly.
它们的实现有点如上所示：两个列表（在本文中为堆栈），允许快速附加和前置元素。

The queue module basically has different functions in a mental separation into 3 interfaces (or APIs) of varying complexity, called 'Original API', 'Extended API' and 'Okasaki API':
队列模块基本上有不同的功能，可以分为3个复杂度不同的接口（或API），分别称为“原始API”、“扩展API”和“冈崎API”：

Original API
:   The original API contains the functions at the base of the queue concept, including: `new/0`, for creating empty queues, `in/2`, for inserting new elements, `out/1`, for removing elements, and then functions to convert to lists, reverse the queue, look if a particular value is part of it, etc.
：原始API包含以队列概念为基础的函数，包括：`new/0`，用于创建空队列，`in/2`，用于插入新元素，`out/1`，用于删除元素，然后是用于转换为列表、反转队列、查看特定值是否是其一部分的函数，等等。

Extended API
:   The extended API mainly adds some introspection power and flexibility: it lets you do things such as looking at the front of the queue without removing the first element (see `get/1` or `peek/1`), removing elements without caring about them (`drop/1`), etc. These functions are not essential to the concept of queues, but they're still useful in general.
：扩展的API主要增加了一些自省功能和灵活性：它允许您在不删除第一个元素的情况下查看队列的前端（请参见“get/1”或“peek/1”）、在不关心元素的情况下删除元素（“drop/1”）等。这些函数对于队列的概念来说并不是必不可少的，但它们在一般情况下仍然很有用。

Okasaki API
:   The Okasaki API is a bit weird. It's derived from Chris Okasaki's *[Purely Functional Data Structures](http://books.google.ca/books?id=SxPzSTcTalAC&lpg=PP1&dq=chris%20okasaki%20purely%20functional%20data%20structures&pg=PP1#v=onepage&q=&f=false)*. The API provides operations similar to what was available in the two previous APIs, but some of the function names are written backwards and the whole thing is relatively peculiar. Unless you do know you want this API, I wouldn't bother with it.
：冈崎的API有点奇怪。它源自Chris Okasaki的*[纯功能数据结构](http://books。谷歌。ca/图书？id=SxPzSTcTalAC&lpg=PP1&dq=chris%20okasaki%20purely%20functional%20data%20structures&pg=PP1#v=onepage&q=&f=false）*。该API提供的操作与前两个API中的操作类似，但有些函数名是反向编写的，整个过程相对比较特殊。除非你知道你想要这个API，否则我不想麻烦你。

You'll generally want to use queues when you'll need to ensure that the first item ordered is indeed the first one processed. So far, the examples I've shown mainly used lists as a accumulators that would then be reversed. In cases where you can't just do all the reversing at once and elements are frequently added, the queue module is what you want (well, you should test and measure first! Always test and measure first!)
当您需要确保订购的第一件物品确实是第一件处理过的物品时，通常需要使用队列。到目前为止，我展示的示例主要使用列表作为累加器，然后将其反转。如果您不能一次完成所有的反转，并且经常添加元素，那么队列模块就是您想要的（好吧，您应该先测试和测量！始终先测试和测量！）

## [End of the short visit]

That's about it for the data structures trip of Erlang. Thank you for having kept your arms inside the vehicles the whole time. Of course, there are a few more data structures available than that to solve different problems. I've only covered those that you're likely to encounter or need the most given the strengths of general use cases of Erlang. I encourage you to explore the [standard library](http://www.erlang.org/doc/apps/stdlib/index.html) too to find more information.
这就是Erlang的数据结构之旅。谢谢你一直把胳膊放在车里。当然，还有更多的数据结构可用于解决不同的问题。鉴于Erlang的通用用例的优势，我只介绍了您可能会遇到或最需要的情况。我鼓励您探索[标准库](http://www。二郎。org/doc/apps/stdlib/index。html）来查找更多信息。

You might be glad to learn that this completes our trip into sequential (functional) Erlang. I know a lot of people get in Erlang to see all the concurrency and processes and whatnot. It's understandable, given it's really where Erlang shines. Supervision trees, fancy error management, distribution, and more. I know I've been very impatient to write about these subjects, so I guess some readers were very impatient to read about them.
您可能会高兴地了解到，这就完成了我们进入顺序（功能）Erlang的旅程。我知道很多人进入Erlang是为了看到所有的并发性、流程等等。这是可以理解的，因为它确实是Erlang的亮点。监督树、花式错误管理、分发等。我知道我已经迫不及待地想写这些主题了，所以我猜一些读者已经迫不及待地想读这些主题了。

However, I judged it made more sense to be comfortable with functional Erlang before moving on to concurrent Erlang. It will be easier to move on afterwards and focus on all the new concepts. Here we go!
然而，我认为在转向并发Erlang之前，先熟悉功能性Erlang更有意义。之后更容易继续前进，专注于所有新概念。开始！

![The splash screen's squid riding a rocket towards concurrency](../img/squid-concurrency.png)
![闪屏上的乌贼骑着火箭走向并发](。。/静态/img/squid并发。（巴布亚新几内亚）
