# 模块

## [什么是模块](#what-are-modules)

![一个写有函数的盒子](../img/modules.png "And yes, I do believe functions are squiggly little colorfoul lines stuffed in a cardboard box.")

使用交互式shell通常被认为是使用动态编程语言的一个重要部分。它对测试各种代码和程序都很有用。使用Erlang的大多数基本数据类型时，甚至不需要打开文本编辑器或保存文件。你可以放下键盘，出去打球，结束一天的工作，但如果你就停在那里，你将是一个糟糕的Erlang程序员。代码需要保存在某个地方才能使用！

This is what modules are for. Modules are a bunch of functions regrouped in a single file, under a single name. Additionally, all functions in Erlang must be defined in modules. You have already used modules, perhaps without realizing it. The BIFs mentioned in the previous chapter, like `hd` or `tl`, actually belong to the `erlang` module, as well as all of the arithmetic, logic and Boolean operators. BIFs from the `erlang` module differ from other functions as they are automatically imported when you use Erlang. Every other function defined in a module you will ever use needs to be called with the form `Module:Function(Arguments)`.
这就是模块的用途。模块是在单个文件中以单个名称重新组合的一组函数。此外，Erlang中的所有函数都必须在模块中定义。您已经使用了模块，可能还没有意识到。上一章中提到的BIF，如`hd`或`tl`，实际上属于`erlang`模块，以及所有算术、逻辑和布尔运算符。`erlang`模块中的BIF与其他函数不同，因为它们是在使用erlang时自动导入的。在模块中定义的所有其他函数都需要以`Module:Function(Arguments)`的形式调用。

```erlang
1> erlang:element(2, {a,b,c}).
b
2> element(2, {a,b,c}).
b
3> lists:seq(1,4).
[1,2,3,4]
4> seq(1,4).
** exception error: undefined shell command seq/2
```

在这里，列表模块中的`seq`函数不是自动导入的，而`element`是自动导入的。错误`undefined shell command`来自于shell查找类似`f()`的shell命令，但找不到它。`erlang`模块中的一些函数不是自动导入的，但它们没有被频繁使用。

从逻辑上讲，您应该将类似的功能放在一个模块中。列表上的常见操作保存在`lists`模块中，而输入和输出功能（如写入终端或文件）在`io`模块中重新组合。您将遇到的唯一不尊重该模式的模块之一是前面提到的`erlang`模块，该模块具有进行数学运算、转换、处理多线程、修改虚拟机设置等功能。除了内置功能外，它们没有任何共同点。您应该避免创建像`erlang`这样的模块，而是关注干净的逻辑分隔。

## [模块声明](#module-declaration)

![上面有小文字的卷轴](../img/declaration.png "I HEREBY DECLARE THIS MODULE AS INDEPENDENT FROM THE OTHERS")

编写模块时，可以声明两种内容：*函数*和*属性*。属性是描述模块本身的元数据，如其名称、外部世界应可见的函数、代码作者等。这种元数据很有用，因为它向编译器提供了如何完成其工作的提示，还因为它允许人们从编译后的代码中检索有用的信息，而无需查阅源代码。

There is a large variety of module attributes currently used in Erlang code across the world; as a matter of fact, you can even declare your own attributes for whatever you please. There are some pre-defined attributes that will appear more frequently than others in your code. All module attributes follow the form `-Name(Attribute).`. Only one of them is necessary for your module to be compilable:
目前世界各地的Erlang代码中使用了大量的模块属性；事实上，你甚至可以随心所欲地声明自己的属性。有些预定义属性在代码中会比其他属性更频繁地出现。所有模块属性都遵循`-Name(Attribute).`。要使模块可编译，只需要其中一个：
> -module(Name).
> 这始终是文件的第一个属性（和语句），理由很充分：它是当前模块的名称，其中`Name`是参数。

是时候编码了！我们的第一个模块将非常简单和无用。打开文本编辑器，输入以下内容，然后将其保存在`useless.erl`中:

```erlang
-module(useless).
```

这行文本是有效的模块。当然，没有功能的模块是没有用的。让我们首先决定从“无用”模块中导出哪些函数。为此，我们将使用另一个属性：
> -export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity])..
   用于定义外部世界可以调用模块的哪些功能。它需要一系列函数及其各自的算术运算。函数的arity是一个整数，表示可以向函数传递多少个参数。这是关键信息，因为在一个模块中，当且仅当定义的不同函数具有不同的算术数时，它们可以共享同一个名称。因此，`add(X,Y)`和`add(X,Y,Z)`将被视为不同的函数，并分别以`add/2`和`add/3`的形式编写。

**注意：**导出的函数代表模块的接口。重要的是定义一个接口，严格说明使用该接口所需的内容，仅此而已。这样做可以让您在不破坏可能依赖于您的模块的代码的情况下处理实现的所有其他“隐藏”细节。

我们的无用模块将首先导出一个名为'add'的有用函数，该函数将包含两个参数。在模块声明之后可以添加以下`-export`属性：

```erlang
-export([add/2]).
```

函数体的内容为：

```erl
add(A,B) ->
    A + B.
```

函数的语法遵循`Name(Args) -> Body.`的形式。，其中`Name`可以是一个或多个用逗号分隔的Erlang表达式。函数以句点结束。请注意，Erlang没有使用“return”关键字。“返回”没用！相反，执行函数的最后一个逻辑表达式将自动将其值返回给调用方，而无需您提及它。

添加以下功能（是的，每个教程都需要一个“Hello world”示例！即使在第四章中也是如此！），不要忘记将其添加到`-export`属性中。

```erlang
%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").
```

我们从这个函数中看到的是，注释只有一行，并且以`%`符号开头（使用`%%`纯粹是一个风格问题）。)`hello/0`函数还演示了如何从内部的外部模块调用函数。在本例中，`io:format/1`是输出文本的标准函数，如注释中所述。

最后我们再加入一个函数，结合了`add/2` 和 `hello/0`：

```erlang
greet_and_add_two(X) ->
    hello(),
    add(X,2).
```

![A box being put in another one](../img/imports.png "Yes, I'm reusing drawings on that one")

不要忘记将`greet_and_add_two/1`添加到导出的功能列表中。对`hello/0`和`add/2`的调用不需要预先添加模块名，因为它们是在模块本身中声明的。

如果希望能够以与`add/2`或模块中定义的任何其他函数相同的方式调用`io:format/1`，则可以在文件开头添加以下模块属性：`-import(io, [format/1]).`。然后你可以直接调用`format("Hello, World!~n").`。一般来说，`-import`属性遵循以下方法：

```erlang
-import(Module, [Function1/Arity, ..., FunctionN/Arity]).
```

导入函数只不过是程序员编写代码时的一种捷径。Erlang程序员经常被劝阻不要使用 `-import` 属性，因为有些人发现它会降低代码的可读性。对于 `io:format/2`，函数`io_lib:format/2`也存在。查找所使用的模块意味着转到文件的顶部，查看它是从哪个模块导入的。因此，保留模块名称被认为是良好的做法。通常，你会看到导入的功能都来自`lists`模块：它的功能使用频率高于其他大多数模块。

你的 `useless` 模块看起来应该是这样的：

```erlang
-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).

add(A,B) ->
    A + B.

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).
```

我们已经完成了“useless”模块。你可以用`useless.erl`这个名字保存文件。文件名应该是在`-module`属性中定义的模块名，后跟'.erl'，这是标准的Erlang源文件后缀名。

在展示如何编译模块并最终尝试其所有令人兴奋的功能之前，我们将了解如何定义和使用宏。Erlang宏与C的 '#define' 语句非常相似，主要用于定义短函数和常量。它们是由文本表示的简单表达式，在为VM编译代码之前，这些文本将被替换。这样的宏主要用于避免在模块周围浮动魔法值。宏定义为以下形式的模块属性：`-define(MACRO, some_value).` 在模块中定义的任何函数中，使用`?MACRO`调用。例如，一个“函数”宏可以写成`-define(sub(X,Y), X-Y).` 用起来像 `?sub(23,47)`，后来被编译器替换为`23-47`。有些人会使用更复杂的宏，但基本语法保持不变。

## [编译代码](#compiling-the-code)

Erlang代码需要被编译成字节码，以便虚拟机使用。您可以从许多地方调用编译器：在命令行中使用`$ erlc flags file.erl`，在shell或模块中使用`compile:file(FileName)`，在shell中使用`c()`，等等。

现在，让我们打开Erlang shell，编译一下我们的“无用”模块吧：

```shell
1> cd("/path/to/where/you/saved/the-module/").
"Path Name to the directory you are in"
ok
```

默认情况下，shell只会在启动时所在的目录和标准库中查找文件：`cd/1`是专门为Erlang shell定义的函数，告诉它将目录更改为新目录，这样浏览文件就不那么麻烦了。Windows用户应该记住使用前斜杠`/`。完成后，请执行以下操作：

```shell
2> c(useless).

```

如果您有另一条消息，请确保文件的名称正确，您位于正确的目录中，并且您在[module](../erlang/useless.erl.html "check against THIS!")中没有写错什么。一旦你成功编译了代码，你会发现。在`useless.erl`旁边多了个`useless.beam`文件。这是编译模块。让我们尝试我们的第一个功能：

```eshell
3> useless:add(7,2).
9
4> useless:hello().
Hello, world!
ok
5> useless:greet_and_add_two(-3).
Hello, world!
-1
6> useless:not_a_real_function().
** exception error: undefined function useless:not_a_real_function/0
```

这些函数按预期工作：`add/2`对数字执行加法，`hello/0`输出"Hello, world!"，而`greet_and_add_two/1`两者都做！当然，您可能会问为什么`hello/0`在输出文本后返回了atom'ok'。这是因为Erlang函数和表达式必须**始终**返回某些内容，即使它们不需要在其他语言中返回。因此，`io:format/1`返回'ok'以表示正常情况，即没有错误。

表达式6显示由于函数不存在而引发的错误。如果忘记导出函数，则在尝试时会出现这种错误消息。

> **注：** 如果你想知道，'.beam'代表 `Bogdan/Björn's Erlang Abstract Machine`，即虚拟机本身。Erlang的其他虚拟机也存在，但它们不再真正被使用，例如：JAM (Joe's Abstract Machine, inspired by Prolog's [WAM](http://en.wikipedia.org/wiki/Warren_Abstract_Machine "Warren Abstract Machin") ，以及old BEAM，他们试图将Erlang编译成C，然后再编译成本机代码。基准测试在这种实践中几乎没有什么好处，因此放弃了这个概念。

为了更好地控制模块的编译方式，存在大量编译flag。您可以在[Erlang文档](http://erlang.org/doc/man/compile.html)中获得所有这些flag的列表。最常见的标志有：

- **-debug_info**
  - Erlang工具，如调试器、代码覆盖率和静态分析工具，将使用模块的调试信息来完成工作。
- **-{outdir,Dir}**
  - 默认情况下，Erlang编译器将在当前目录中创建“beam”文件。这将允许您选择将编译文件放在何处。
- **-export_all**
  - 将忽略“-export”模块属性，而是导出定义的所有函数。这在测试和开发新代码时非常有用，但不应用于生产。
- **-{d,Macro} or {d,Macro,Value}**
- 定义要在模块中使用的宏，其中宏是原子。这在处理单元测试时更常用，以确保模块仅在明确需要时创建和导出其测试函数。默认情况下，如果未将值定义为元组的第三个元素，则该值为“true”。

我们可以在shell中使用flag进行编译：

```shell
7> compile:file(useless, [debug_info, export_all]).
{ok,useless}
8> c(useless, [debug_info, export_all]).
{ok,useless}
```

您也可以偷偷地从模块中定义编译标志，并使用模块属性。为了得到与表达式7和8相同的结果，可以在模块中添加以下行：

```erlang
-compile([debug_info, export_all]).
```

然后只需编译，就会得到与手动传递标志相同的结果。现在我们已经能够写下函数，编译它们并执行它们，现在是时候看看我们能走多远了！

> **注：**另一个选项是将Erlang模块编译为原生字节码(native code)。本机代码编译**不**适用于所有平台和操作系统，但在支持本机代码编译的平台和操作系统上，本机代码编译可以使您的程序运行更快（根据传闻，大约快20%）。要编译为原生字节码，需要使用`hipe`模块，并按以下方式调用它：`hipe:c(Module,OptionsList).`你也可以使用`c(Module,[native]).`在eshell中达到类似的效果。请注意生成的.beam文件将包含原生代码和非原生代码，原生部分将无法跨平台移植。

## [更多有关模块的内容](#more-about-modules)

在进一步学习编写函数和几乎没有用处的代码片段之前，我想讨论一下其他一些对您将来可能有用的信息。

第一个是关于模块的元数据。我在本章开头提到，模块属性是描述模块本身的元数据。当我们无法访问源文件时，在哪里可以找到这些元数据？编译器对我们很好：编译模块时，它会提取大多数模块属性，并将它们（以及其他信息）存储在`module_info/0`函数中。您可以通过以下方式查看`useless`模块的元数据：

```erlang
9> useless:module_info().
[{exports,[{add,2},
           {hello,0},
           {greet_and_add_two,1},
           {module_info,0},
           {module_info,1}]},
 {imports,[]},
 {attributes,[{vsn,[174839656007867314473085021121413256129]}]},
 {compile,[{options,[]},
           {version,"4.6.2"},
           {time,{2009,9,9,22,15,50}},
           {source,"/home/ferd/learn-you-some-erlang/useless.erl"}]}]
10> useless:module_info(attributes).
[{vsn,[174839656007867314473085021121413256129]}]
```

上面的代码片段还显示了一个附加函数`module_info/1`，它可以让您获取一条特定的信息。您可以看到导出的函数、导入的函数（本例中没有！），属性（这是自定义元数据的位置），以及编译选项和信息。你可以自行决定添加`-author("An Erlang Champ").`的信息，对于你的模块来说，它应该和`vsn`放在同一个部分`。当涉及到生产内容时，模块属性的用途有限，但在做一些小技巧来帮助自己时，它们可能会很好：我在我的[testing script](../erlang/tester.erl.html "Automatic testing module. See warnings/0.")中使用它们，以便本书对单元测试的函数进行更好地注释；脚本将查找模块属性，找到注释的函数，并显示有关它们的警告。

> **注：**`vsn`是自动生成的唯一值，用于区分代码的每个版本，不包括注释。它用于代码热加载（在应用程序运行时升级应用程序，而不停止它）以及一些与版本处理相关的工具。如果需要，您也可以自己指定一个'vsn'值：只需将`-vsn(VersionNumber)`添加到模块中即可。

![一个有三个节点的小图：妈妈、爸爸和你。爸爸妈妈是你的父母，你是爸爸的兄弟。下面的文字：'如果循环依赖在现实生活中是不合理的，也许它们在你的程序中也应该是令人厌恶的'](../img/circular-dependencies.png "Seems clear enough.")

另一个很好的方法是关于通用模块设计：避免循环依赖！一个模块`A`不应该调用一个模块`B`，如果`B`还好会调用`A`。这种依赖性通常会导致代码维护困难。事实上，依赖太多的模块，即使它们不是循环依赖的，也会使维护变得更加困难。你最不想做的事就是半夜醒来，却发现一个疯狂的软件工程师或计算机科学家因为你写的糟糕代码而试图挖出你的眼睛。

出于类似的原因，通常认为将具有相似角色的功能重新组合在一起是一种很好的做法。启动和停止应用程序，或在某些数据库中创建和删除记录就是这种情况的例子。

好了，说教就到此为止，让我们再进一步探索Erlang吧。
