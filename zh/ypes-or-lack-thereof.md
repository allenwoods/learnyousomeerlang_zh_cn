# Types (or lack thereof)

## [Dynamite-strong Typing]

As you might have noticed when typing in examples from [Starting Out (for real)](starting-out-for-real.html "Starting Out (for real)")` or really anything at all.
正如你在[Starting Out（for real）]（Starting Out for real）中输入示例时可能已经注意到的那样。html“开始（真的）”`或者什么都可以。

When it didn't work, an error was thrown in your face, but only once you ran the code. This is because Erlang is *dynamically typed*: every error is caught at runtime and the compiler won't always yell at you when compiling modules where things may result in failure, like in [Starting Out (for real)](starting-out-for-real.html#bool-and-compare "Boolean Algebra & Comparison Operators")'s `"llama + 5"` example.
当它不起作用时，一个错误就会出现在你的脸上，但只有在你运行代码的时候。这是因为Erlang是*动态类型的*：每个错误都会在运行时被捕获，编译器在编译可能导致失败的模块时不会总是对您大喊大叫，比如[Starting Out（for real）]（Starting Out for real）。html#bool and compare“布尔代数与比较运算符”的`“llama+5”`示例。

![A knife slicing ham.](../img/ham.png "I'll let you decide which is static and which is dynamic")
![切火腿的小刀]。](。。/静态/img/ham。png“我会让你决定哪个是静态的，哪个是动态的”）

One classic friction point between proponents of static and dynamic typing has to do with the safety of the software being written. A frequently suggested idea is that good static type systems with compilers enforcing them with fervor will catch most errors waiting to happen before you can even execute the code. As such, statically typed languages are to be seen as safer than their dynamic counterparts. While this might be true when comparing with many dynamic languages, Erlang begs to differ and certainly has a track record to prove it. The best example is the often reported *nine nines* (99.9999999%) of availability offered on the [Ericsson AXD 301 ATM switches](http://www.erlang.se/publications/Ulf_Wiger.pdf "Ulf Wiger - Four-fold Increase in Productivity and Quality"), consisting of over 1 million lines of Erlang code. Please note that this is not an indication that none of the components in an Erlang-based system failed, but that a general switch system was available 99.9999999% of the time, planned outages included. This is partially because Erlang is built on the notion that a failure in one of the components should not affect the whole system. Errors coming from the programmer, hardware failures or \[some\] network failures are accounted for: the language includes features which will allow you to distribute a program over to different nodes, handle unexpected errors, and *never* stop running.
静态和动态类型的支持者之间的一个典型摩擦点与正在编写的软件的安全性有关。一个经常被建议的想法是，好的静态类型系统，如果编译器能够热情地执行它们，那么在您甚至可以执行代码之前，就会捕捉到大多数等待发生的错误。因此，静态类型的语言被认为比动态类型的语言更安全。虽然与许多动态语言相比，这可能是正确的，但Erlang请求有所不同，而且肯定有一个历史记录来证明这一点。最好的例子是经常报道的九九（99）。[Ericsson AXD 301 ATM交换机]提供的可用性(http://www。二郎。se/publications/Ulf_Wiger。pdf“Ulf Wiger——生产率和质量提高了四倍”），包含超过100万行Erlang代码。请注意，这并不表示基于Erlang的系统中没有任何组件出现故障，而是表示通用交换机系统可用。99999999%的时间，包括计划内停机。这在一定程度上是因为Erlang建立在一个概念之上，即其中一个组件的故障不应影响整个系统。来自程序员的错误、硬件故障或\[某些\]网络故障都会被考虑在内：该语言包含一些功能，允许您将程序分发到不同的节点，处理意外错误，并且“永不”停止运行。

To make it short, while most languages and type systems aim to make a program error-free, Erlang uses a strategy where it is assumed that errors will happen anyway and makes sure to cover these cases: Erlang's dynamic type system is not a barrier to reliability and safety of programs. This sounds like a lot of prophetic talking, but you'll see how it's done in the later chapters.
简而言之，虽然大多数语言和类型系统的目标是使程序无错误，但Erlang使用了一种策略，即假设错误无论如何都会发生，并确保涵盖这些情况：Erlang的动态类型系统并不是程序可靠性和安全性的障碍。这听起来像是很多预言，但你将在后面的章节中看到它是如何实现的。

::: note
**Note:** Dynamic typing was historically chosen for simple reasons; those who implemented Erlang at first mostly came from dynamically typed languages, and as such, having Erlang dynamic was the most natural option to them.
**注：*历史上选择动态类型的原因很简单；最初实现Erlang的人大多来自动态类型语言，因此，拥有Erlang动态是他们最自然的选择。
:::

Erlang is also strongly typed. A weakly typed language would do implicit type conversions between terms. If Erlang were to be weakly typed we could possibly do the operation `6 = 5 + "1".` while in practice, an exception for bad arguments will be thrown:
Erlang也是强类型的。弱类型语言可以在术语之间进行隐式类型转换。如果Erlang是弱类型的，我们可能会执行操作'6=5+“1”。`在实践中，将抛出错误参数的异常：

```eshell
1> 6 + "1".
** exception error: bad argument in an arithmetic expression
     in operator  +/2
        called as 6 + "1"
```

Of course, there are times when you could want to convert one kind of data to another one: changing regular strings into bit strings to store them or an integer to a floating point number. The Erlang standard library provides a number of functions to do it.
当然，有时可能需要将一种数据转换为另一种数据：将常规字符串转换为位字符串以存储它们，或将整数转换为浮点数。Erlang标准库提供了许多函数来实现这一点。

## [Type conversions]

Erlang, like many languages, changes the type of a term by casting it into another one. This is done with the help of built-in functions, as many of the conversions could not be implemented in Erlang itself. Each of these functions take the form \<type\>\_to\_\<type\> and are implemented in the `erlang` module. Here are a few of them:
像许多语言一样，Erlang通过将一个术语转换为另一个术语来改变它的类型。这是在内置函数的帮助下完成的，因为许多转换无法在Erlang本身中实现。这些函数中的每一个都采用\<type\>\\ u到\\u\<type\>的形式，并在“erlang”模块中实现。以下是其中一些：

```eshell
1> erlang:list_to_integer("54").
54
2> erlang:integer_to_list(54).
"54"
3> erlang:list_to_integer("54.32").
** exception error: bad argument
     in function  list_to_integer/1
        called as list_to_integer("54.32")
4> erlang:list_to_float("54.32").
54.32
5> erlang:atom_to_list(true).
"true"
6> erlang:list_to_bitstring("hi there").
<<"hi there">>
7> erlang:bitstring_to_list(<<"hi there">>).
"hi there"
```

And so on. We're hitting on a language wart here: because the scheme \<type\>\_to\_\<type\> is used, every time a new type is added to the language, a whole lot of conversion BIFs need to be added! Here's the whole list already there:
等等。我们在这里遇到了一个语言难题：因为使用了\<type\>\\\\\\\\\\\\<type\>方案，每次向语言中添加新类型时，都需要添加大量转换BIF！以下是已经存在的完整列表：

`atom_to_binary/2, atom_to_list/1, binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1, bitstring_to_list/1, binary_to_term/1, float_to_list/1, fun_to_list/1, integer_to_list/1, integer_to_list/2, iolist_to_binary/1, iolist_to_atom/1, list_to_atom/1, list_to_binary/1, list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1, list_to_integer/2, list_to_pid/1, list_to_tuple/1, pid_to_list/1, port_to_list/1, ref_to_list/1, term_to_binary/1, term_to_binary/2 and tuple_to_list/1.`
`二、二、二、二、二、二、二、二、二、二、二、二、二、三、二、二、二、二、二、二、二、二、二、二、二、二、二、二、三、三、三、三、三、三、三、三、三、三、三、三、二、二、二、二、二、二、二、三、二、二、二、三、二、二、二、二、二、三、二、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、三、原子原子原子、一、一、一、一、一、一、三原子、一、一、一、一、三原子、三原子、一、一、三原子、一、一、三原子、三原子、一、三原子、一、三原子、一、三原子、一、一、一、一、一、一、列表到整数/2，列表到pid/1，列表到元组/1，pid_to_list/1、port_to_list/1、ref_to_list/1、term_to_binary/1、term_to_binary/2和tuple_to_list/1。`

That's a lot of conversion functions. We'll see most if not all of these types through this book, although we probably won't need all of these functions.
这是很多转换函数。虽然我们可能不需要所有这些函数，但我们将在本书中看到大部分（如果不是全部的话）这些类型。

## [To Guard a Data Type]

Erlang basic data types are easy to spot, visually: tuples have the curly brackets, lists the square brackets, strings are enclosed in double quotation marks, etc. Enforcing a certain data type has thus been possible with pattern matching: a function `head/1` taking a list could only accept lists because otherwise, the matching (`[H|_]`) would have failed.
Erlang基本数据类型在视觉上很容易识别：元组有花括号，列表有方括号，字符串用双引号括起来，等等。因此，通过模式匹配可以强制执行特定的数据类型：获取列表的函数'head/1'只能接受列表，因为否则，匹配（`H | |]`）就会失败。

![Hi, My name is Tuple](../img/my-name-is.png)

However, we've had a problem with numeric values because we couldn't specify ranges. Consequently, we used guards in functions about temperature, the age to drive, etc. We're hitting another roadblock now. How could we write a guard that ensures that patterns match against data of a single specific type, like numbers, atoms or bitstrings?
但是，由于无法指定范围，我们在数值方面遇到了问题。因此，我们在有关温度、驾驶年龄等功能中使用了防护装置。我们现在又遇到了一个障碍。我们如何编写一个保护程序来确保模式与单个特定类型的数据（如数字、原子或位字符串）匹配？

There are functions dedicated to this task. They will take a single argument and return true if the type is right, false otherwise. They are part of the few functions allowed in guard expressions and are named the *type test BIFs*:
有专门用于此任务的功能。它们将接受一个参数，如果类型正确，则返回true，否则返回false。它们是guard表达式中允许的少数函数的一部分，被命名为*type test BIFs*：

```eshell
is_atom/1           is_binary/1         
is_bitstring/1      is_boolean/1        is_builtin/3        
is_float/1          is_function/1       is_function/2       
is_integer/1        is_list/1           is_number/1         
is_pid/1            is_port/1           is_record/2         
is_record/3         is_reference/1      is_tuple/1          
```

They can be used like any other guard expression, wherever guard expressions are allowed. You might be wondering why there is no function just giving the type of the term being evaluated (something akin to `type_of(X) -> Type`). The answer is pretty simple. Erlang is about programming for the right cases: you only program for what you know will happen and what you expect. Everything else should cause errors as soon as possible. Although this might sound insane, the explanations you'll get in [Errors and Exceptions](errors-and-exceptions.html) will hopefully make things clearer. Until then, just trust me on that.
它们可以像任何其他保护表达式一样使用，只要允许使用保护表达式。你可能想知道为什么没有函数只给出被评估的术语的类型（类似于'type_of（X）->type`）。答案很简单。Erlang是为正确的情况编程：你只为你知道会发生的事情和你期望的事情编程。其他一切都应该尽快导致错误。虽然这听起来可能很疯狂，但你会在[错误和例外]（错误和例外）中得到解释。html）将有望让事情变得更清楚。在那之前，请相信我。

::: note
**Note:** type test BIFs constitute more than half of the functions allowed in guard expressions. The rest are also BIFs, but do not represent type tests. These are:\
**注：*型式试验BIF构成了guard表达式中允许的函数的一半以上。其余部分也是BIF，但不代表型式试验。这些是：\
`abs(Number), bit_size(Bitstring), byte_size(Bitstring), element(N, Tuple), float(Term), hd(List), length(List), node(), node(Pid|Ref|Port), round(Number), self(), size(Tuple|Bitstring), tl(List), trunc(Number), tuple_size(Tuple).`
`abs（数字）、bit_size（位字符串）、byte_size（位字符串）、element（N，元组）、float（术语）、hd（列表）、length（列表）、node（）、node（Pid | Ref | Port）、round（数字）、self（）、size（元组|位字符串）、tl（列表）、trunc（数字）、Tuple_size（元组）。`

The functions `node/1` and `self/0` are related to distributed Erlang and processes/actors. We'll eventually use them, but we've still got other topics to cover before then.
函数“node/1”和“self/0”与分布式Erlang和进程/参与者相关。我们最终会使用它们，但在此之前我们还有其他话题要讨论。
:::

It may seem like Erlang data structures are relatively limited, but lists and tuples are usually enough to build other complex structures without worrying about anything. As an example the basic node of a binary tree could be represented as ` are either similar nodes or empty tuples. I could also represent myself as:
看起来Erlang数据结构相对有限，但列表和元组通常足以构建其他复杂结构，而无需担心任何问题。例如，二叉树的基本节点可以表示为“相似节点或空元组”。我也可以把自己描述为：

```erl
,
         ,
         ,
         .
```

Which shows that by nesting tuples and list and filling them with data, we can obtain complex data structures and build functions to operate on them.
这表明，通过嵌套元组和列表并用数据填充它们，我们可以获得复杂的数据结构，并构建函数对其进行操作。

::: 
**Update:**\
The release R13B04 saw the addition of the BIF `binary_to_term/2`, which lets you unserialize data the same way `binary_to_term/1` would, except the second argument is an option list. If you pass in `[safe]`, the binary won't be decoded if it contains unknown atoms or [anonymous functions](higher-order-functions.html "Higher Order Functions chapter"), which could exhaust memory.
R13B04版本中增加了BIF“binary_to_term/2”，它允许您以与“binary_to_term/1”相同的方式取消序列化数据，但第二个参数是一个选项列表。如果传入“[safe]”，如果二进制包含未知原子或[anonymous functions]（高阶函数），则不会对其进行解码。html“高阶函数章节”），这可能会耗尽内存。
:::

## [For Type Junkies]

![A sign for homeless people: 'Will dance for types'](../img/type-dance.png "You know you would")
![无家可归者的标志：'Will dance for types'](。。/静态/img/类型舞蹈。png“你知道你会的”）

This section is meant to be read by programmers who can not live without a static type system for one reason or another. It will include a little bit more advanced theory and everything may not be understood by everyone. I will briefly describe tools used to do static type analysis in Erlang, defining custom types and getting more safety that way. These tools will be described for anyone to understand much later in the book, given that it is not necessary to use any of them to write reliable Erlang programs. Because we'll show them later, I'll give very little details about installing, running them, etc. Again, this section is for those who really can't live without advanced type systems.
本节旨在供程序员阅读，因为这样或那样的原因，没有静态类型系统就无法生存。它将包含一点更先进的理论，可能并非所有人都理解。我将简要描述用于在Erlang中进行静态类型分析的工具，定义自定义类型并通过这种方式获得更多安全性。考虑到编写可靠的Erlang程序不需要使用这些工具中的任何一个，本书稍后将对这些工具进行描述，以供任何人理解。关于安装，我稍后会给出一些细节。同样，本节针对的是那些没有高级类型系统就无法生活的人。

Through the years, there were some attempts to build type systems on top of Erlang. One such attempt happened back in 1997, conducted by Simon Marlow, one of the lead developers of the Glasgow Haskell Compiler, and Philip Wadler, who worked on Haskell's design and has contributed to the theory behind monads ([Read the paper](http://homepages.inf.ed.ac.uk/wadler/papers/erlang/erlang.pdf "A Practical Subtyping System for Erlang"):
多年来，有人试图在Erlang上构建类型系统。1997年，格拉斯哥哈斯克尔编译器的首席开发人员西蒙·马洛和菲利普·瓦德勒进行了一次这样的尝试。菲利普·瓦德勒致力于哈斯克尔的设计，并对单子背后的理论做出了贡献（【阅读论文】(http://homepages。inf。预计起飞时间。交流电。英国/wadler/papers/erlang/erlang。pdf“Erlang的实用子类型系统”）：

> One day Phil phoned me up and announced that a) Erlang needed a type system, b) he had written a small prototype of a type system and c) he had a one year's sabbatical and was going to write a type system for Erlang and "were we interested?" Answer ---"Yes."
>一天，菲尔打电话给我，宣布a）Erlang需要一个类型系统，b）他已经编写了一个类型系统的小型原型，c）他有一年的休假，打算为Erlang编写一个类型系统，并问“我们感兴趣吗？”回答——“是的。"
>
> Phil Wadler and Simon Marlow worked on a type system for over a year and the results were published in \[20\]. The results of the project were somewhat disappointing. To start with, only a subset of the language was type-checkable, the major omission being the lack of process types and of type checking inter-process messages.
>菲尔·瓦德勒（Phil Wadler）和西蒙·马洛（Simon Marlow）研究了一年多的打字系统，结果发表在\[20\]。这个项目的结果有些令人失望。首先，该语言只有一个子集是可进行类型检查的，主要的遗漏是缺少进程类型和进程间消息的类型检查。

Processes and messages both being one of the core features of Erlang, it may explain why the system was never added to the language. Other attempts at typing Erlang failed. The efforts of the HiPE project (attempts to make Erlang's performances much better) produced Dialyzer, a static analysis tool still in use today, with its very own type inference mechanism.
进程和消息都是Erlang的核心特性之一，这可能解释了为什么该系统从未添加到语言中。其他键入Erlang的尝试都失败了。HiPE项目的努力（试图使Erlang的性能更好）产生了Dialyzer，这是一种静态分析工具，至今仍在使用，它有自己的类型推断机制。

The type system that came out of it is based on success typings, a concept different from Hindley-Milner or soft-typing type systems. Success types are simple in concept: the type-inference will not try to find the exact type of every expression, but it will guarantee that the types it infers are right, and that the type errors it finds are really errors.
由此产生的打字系统是基于成功的打字，这一概念不同于欣德利·米尔纳或软打字系统。成功类型在概念上很简单：类型推断不会试图找到每个表达式的确切类型，但它将保证它推断的类型是正确的，并且它找到的类型错误是真正的错误。

The best example would come from the implementation of the function `and`, which will usually take two Boolean values and return 'true' if they're both true, 'false' otherwise. In Haskell's type system, this would be written `and :: bool -> bool -> bool`. If the `and` function had to be implemented in Erlang, it could be done the following way:
最好的例子是函数“and”的实现，它通常会接受两个布尔值，如果它们都为真，则返回“true”，否则返回“false”。在Haskell的类型系统中，这将被写为“and:：bool->bool->bool”`。如果必须在Erlang中实现“and”函数，可以通过以下方式实现：

```erl
and(false, _) -> false;
and(_, false) -> false;
and(true,true) -> true.
```

Under success typing, the inferred type of the function would be `and(_,_) -> bool()`, where `_`, which explains the rationale behind the behavior. I really encourage any type junkies out there to read it, it's an interesting and practical implementation definition.
在success typing下，函数的推断类型将是` and（，）->bool（）`，其中` `，这解释了行为背后的基本原理。我真的鼓励任何类型的瘾君子阅读它，这是一个有趣而实用的实现定义。

The details about type definitions and function annotations are described in the Erlang Enhancement Proposal 8 ([EEP 8](http://www.erlang.org/eeps/eep-0008.html "EEP 8") and Dialyzer, both part of the standard distribution. To use them, type in `$ typer --help` and `$ dialyzer --help` (`typer.exe --help` and `dialyzer.exe --help` for Windows, if they're accessible from the directory you are currently in).
有关类型定义和函数注释的详细信息，请参见Erlang增强方案8（[EEP 8](http://www。二郎。org/eeps/eep-0008。html“EEP 8”）和透析器，都是标准分发的一部分。要使用它们，请输入“$typer--help”和“$dialyzer--help”（'typer。exe——帮助和透析器。exe--help`适用于Windows，如果可以从您当前所在的目录访问它们）。

TypEr will be used to generate type annotations for functions. Used on this small [FIFO implementation](static/erlang/fifo.erl.html "A Queue module"), it spits the following type annotations:
TypEr将用于为函数生成类型注释。用于这个小型[FIFO实现]（静态/erlang/FIFO）。呃。html“一个队列模块”），它提供以下类型注释：

```erl
%% File: fifo.erl
%% --------------
-spec new() -> .
-spec push(.
-spec pop(.
-spec empty() -> bool().
```

![Implementation of fifo (queues): made out of two stacks (last-in first-out).](../img/fifo.png "Implementation of fifo (queues): made out of two stacks (last-in first-out).")
![fifo（队列）的实现：由两个堆栈组成（后进先出）。](。。/静态/img/fifo。png“fifo（队列）的实现：由两个堆栈组成（后进先出）。")

Which is pretty much right. Improper lists should be avoided because `lists:reverse/1` doesn't support them, but someone bypassing the module's interface would be able to get through it and submit one. In this case, the functions `push/2` and `pop/2` might still succeed for a few calls before they cause an exception. This either tells us to add guards or refine our type definitions manually. Suppose we add the signature `-spec push( is output.
这是非常正确的。应该避免不正确的列表，因为'lists:reverse/1'不支持它们，但是绕过模块接口的人可以通过它并提交一个。在这种情况下，函数“push/2”和“pop/2”在引发异常之前可能仍会成功进行一些调用。这要么告诉我们添加保护，要么手动优化类型定义。假设我们添加签名`-spec push（输出）。

Dialyzer will complain only when code will break other code, and if it does, it'll usually be right (it will complain about more stuff too, like clauses that will never match or general discrepancies). Polymorphic data types are also possible to write and analyze with Dialyzer: the `hd()` function could be annotated with `-spec([A]) -> A.` and be analyzed correctly, although Erlang programmers seem to rarely use this type syntax.
透析器只有在代码破坏其他代码时才会抱怨，如果它破坏了其他代码，它通常是对的（它也会抱怨更多的东西，比如永远不匹配的条款或一般性差异）。多态数据类型也可以用透析器写入和分析：`hd（）`函数可以用`-spec（[A]）->A注释。`尽管Erlang程序员似乎很少使用这种类型的语法，但仍需要正确地分析。

::: 
**Don't drink too much Kool-Aid:**\
Some of the things you can't expect Dialyzer and TypEr to do is type classes with constructors, first order types and recursive types. The types of Erlang are only annotations without effects or restrictions on actual compiling unless you enforce them yourself. The type checker will never tell you a program that can run right now (or has run for two years) has a type bug when it effectively causes no error when running (although you could have buggy code running correctly\...)
Dialyzer和TypEr不能做的一些事情是使用构造函数、一阶类型和递归类型来类型类。Erlang的类型只是注释，对实际编译没有影响或限制，除非您自己强制执行它们。如果一个程序现在可以运行（或者已经运行了两年）而在运行时没有导致错误，那么类型检查器永远不会告诉你该程序有类型错误（尽管你可以正确运行有错误的代码）\。。。)

While recursive types are something that would be really interesting to have, they're unlikely to ever appear in the current forms of TypEr and Dialyzer (the paper above explains why). Defining your own types to simulate recursive types by adding one or two levels manually is the best you can do at the moment.
虽然递归类型确实很有趣，但它们不太可能出现在当前的打字机和透析器中（上面的文章解释了原因）。通过手动添加一个或两个级别来定义自己的类型来模拟递归类型，这是目前最好的方法。

It's certainly not a full-blown type system, not as strict or powerful as what languages like Scala, Haskell or Ocaml propose. Its warning and error messages are also usually a bit cryptic and not really user friendly. However, it's still a very good compromise if you really can't live in a dynamic world or wish for additional safety; just expect it to be a tool in your arsenal, not too much more.
它当然不是一个成熟的类型系统，不像Scala、Haskell或Ocaml等语言所建议的那样严格或强大。它的警告和错误信息通常也有点晦涩难懂，不太方便用户使用。然而，如果你真的不能生活在一个充满活力的世界，或者不希望有额外的安全感，这仍然是一个很好的妥协；只是希望它成为你武器库中的一个工具，而不是更多。
:::

::: 
**Update:**\
Since version R13B04, recursive types are now available as an experimental feature for Dialyzer. This makes the previous *Don't drink too much Kool-aid* partially wrong. Shame on me.
自R13B04版本以来，递归类型现在可以作为透析器的实验功能使用。这使得之前的“不要喝太多Kool aid”部分错误。真丢脸。

Note that the [type documentation has also become official](http://erlang.org/doc/reference_manual/typespec.html "Official Types and Functions Specifications spec") (although it remains subject to change) and is more complete than what can be found in EEP8.
请注意，[类型文档也已成为正式文档](http://erlang。组织/文件/参考手册/类型规范。html“官方类型和功能规范规范”）（尽管仍有可能更改），比EEP8中的内容更完整。
:::
