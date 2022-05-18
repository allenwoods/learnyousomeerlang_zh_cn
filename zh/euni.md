# EUnited Nations Council

## [The Need for Tests]

![G-Unit logo, parodied to be say 'E-Unit'](../img/eunit.png "Gangsta-style unit testing")

The software we've written has gotten progressively bigger and somewhat more complex with time. When that happens, it becomes rather tedious to start an Erlang shell, type things in, look at results, and make sure things work after code has been changed. As time goes on, it becomes simpler for everyone to run tests that are all prepared and ready in advance rather than following checklists of stuff to check by hand all the time. These are usually pretty good reasons to want tests in your software. It's also possible that you're a fan of test-driven development and so will also find tests useful.
随着时间的推移，我们编写的软件越来越大，也越来越复杂。当这种情况发生时，启动Erlang shell、输入内容、查看结果，并确保代码更改后工作正常，会变得相当乏味。随着时间的推移，对每个人来说，运行提前准备好的测试变得更加简单，而不是一直按照清单上的东西手动检查。这些通常是在你的软件中进行测试的很好的理由。你也可能是测试驱动开发的粉丝，因此也会发现测试很有用。

If you recall the chapter where we wrote a [RPN calculator](functionally-solving-problems.html#rpn-calculator), we had a few tests that we had manually written. They were simply a set of pattern matches of the form `Result = Expression` that would crash if something went wrong, or would succeed otherwise. That works for simple bits of code you write for yourself, but when we get to more serious tests, we will definitely want something better, like a framework.
如果你还记得我们写[RPN计算器]（功能性解决问题）的那一章。html#rpn计算器），我们有一些手工编写的测试。它们只是一组“Result=Expression”形式的模式匹配，如果出现问题，它们会崩溃，否则就会成功。这适用于你自己编写的简单代码，但当我们进行更严格的测试时，我们肯定会想要更好的东西，比如一个框架。

For unit tests, we'll tend to stick to *EUnit* (which we see in this chapter). For integration tests, EUnit as well as *Common Test* can both do the job. In fact, Common Test can do everything from unit tests up to system tests, and even testing of external software, not written in Erlang. For now we'll go with EUnit, given how simple it is for the good results it yields.
对于单元测试，我们倾向于使用*EUnit*（我们在本章中看到）。对于集成测试，EUnit和*Common Test*都可以完成这项工作。事实上，通用测试可以做从单元测试到系统测试的所有事情，甚至可以测试外部软件，而不是用Erlang编写的。现在我们将使用EUnit，因为它非常简单，可以产生良好的效果。

## [EUnit, What's a EUnit?]

EUnit, in its simplest form, is just a way to automate running functions that end in `_test()` in a module by assuming they are unit tests. If you go dig out that RPN calculator I mentioned above, you'll find the following code:
EUnit以其最简单的形式，只是一种通过假设函数是单元测试来自动运行以模块中的`_test（）`结尾的函数的方法。如果你去挖掘我上面提到的RPN计算器，你会发现以下代码：

```erl
rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try
        rpn("90 34 12 33 55 66 + * - +")
    catch
        error: -> ok
    end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 =  rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.
```

This was the test function we wrote to make sure the calculator worked fine. Find the old module and try this:
这是我们为确保计算器正常工作而编写的测试函数。找到旧模块并尝试以下操作：

```eshell
1> c(calc).

2> eunit:test(calc).
  Test passed.
ok
```

Calling `eunit:test(Module).` was all we needed! Yay, we now know EUnit! Pop the champagne and let's head to a different chapter!
调用'eunit:test（模块）。`这就是我们所需要的！耶，我们现在认识尤妮特了！开香槟，让我们进入另一个章节！

Obviously a testing framework that only does this little wouldn't be very useful, and in technical programmer jargon, it might be described as 'not very good'. EUnit does more than automatically exporting and running functions ending in `_test()`. For one, you can move the tests out to a different module so that your code and its tests are not mixed together. This means you can't test private functions anymore, but also means that if you develop all your tests against the module's interface (the exported functions), then you won't need to rewrite tests when you refactor your code. Let's try separating tests and code with two simple modules:
显然，一个只做这一点的测试框架不会非常有用，用程序员的术语来说，它可能被描述为“不太好”。EUnit不仅仅自动导出和运行以`_test（）结尾的函数`。首先，您可以将测试转移到另一个模块，这样您的代码及其测试就不会混合在一起。这意味着你不能再测试私有函数了，但也意味着如果你针对模块的接口（导出的函数）开发了所有的测试，那么你重构代码时就不需要重写测试了。让我们尝试用两个简单的模块来分离测试和代码：

```erl
-module(ops).
-export([add/2]).

add(A,B) -> A + B.
```

```erl
-module(ops_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    4 = ops:add(2,2).
```

So we have [ops](static/erlang/ops.erl.html), where the second includes tests related to the first. Here's a thing EUnit can do:
所以我们有[ops]（static/erlang/ops）。呃。html），其中第二个包含与第一个相关的测试。尤尼特可以做一件事：

```eshell
3> c(ops).

4> c(ops_tests).

5> eunit:test(ops).
  Test passed.
ok
```

Calling `eunit:test(Mod)` automatically looks for `Mod`\_tests and runs the tests within that one. Let's change the test a bit (make it `3 = ops:add(2,2)`) to see what failures look like:
调用'eunit:test（Mod）'会自动查找'Mod`\\ u测试并在其中运行测试。让我们稍微改变一下测试（使其为'3=ops:add（2,2）`），看看失败是什么样子的：

```eshell
6> c(ops_tests). 

7> eunit:test(ops).
ops_tests: add_test (module 'ops_tests')...*failed*
::error:
  in function ops_tests:add_test/0


=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
```

![A lie detector with a red light lit up, meaning someone lied](../img/lies.png)

We can see what test failed (`ops_tests: add_test...`) and why it did (`::error:`). We also get a full report of how many tests passed or failed. The output is pretty bad though. At least as bad as regular Erlang crashes: no line numbers, no clear explanation (`4` didn't match with what, exactly?), etc. We're left helpless by a test framework that runs tests but doesn't tell you much about them.
我们可以看到哪些测试失败（`ops_tests:add_test）。。。`)为什么会这样（`：：错误：`）。我们还得到了一份完整的报告，说明有多少测试通过或失败。但是输出非常糟糕。至少和常规的Erlang崩溃一样糟糕：没有行号，没有明确的解释（'4'与什么不匹配，确切地说？），等。我们被一个运行测试的测试框架弄得束手无策，但它并没有告诉你太多关于测试的信息。

For this reason, EUnit introduces a few macros to help us. Each of them will give us cleaner reporting (including line numbers) and clearer semantics. They're the difference between knowing that something goes wrong and knowing *why* something goes wrong:
因此，EUnit引入了一些宏来帮助我们。它们中的每一个都将为我们提供更清晰的报告（包括行号）和更清晰的语义。它们是知道出了问题和知道为什么出了问题之间的区别：

`?assert(Expression), ?assertNot(Expression)`
:   Will test for boolean values. If any value other than `true` makes it into `?assert`, an error will be shown. Same for `?assertNot`, but for negative values. This macro is somewhat equivalent to `true = X` or `false = Y`.
：将测试布尔值。如果除'true'以外的任何值都可以转换为`？断言`，将显示一个错误。“也一样吗？”？assertNot`，但用于负值。这个宏在某种程度上相当于'true=X'或'false=Y'`。

`?assertEqual(A, B)`
:   Does a strict comparison (equivalent to `=:=`) between two expressions, `A`. If they are different, a failure will occur. This is roughly equivalent to `true = X =:= Y`. Since R14B04, the macro `?assertNotEqual` is available to do the opposite of `?assertEqual`.
：在两个表达式“a”之间进行严格比较（相当于“=：=”）`。如果它们不同，就会发生故障。这大致相当于'true=X=：=Y'`。从R14B04开始，宏`？assertNotEqual'可用于执行与`？相反的操作？阿塞特奎尔`。

`?assertMatch(Pattern, Expression)`
:   This allows us to match in a form similar to `Pattern = Expression`, without variables ever binding. This means that I could do something like `?assertMatch( would not be bound.
：这允许我们以类似于“Pattern=Expression”的形式进行匹配，而不需要绑定变量。这意味着我可以做像`？assertMatch（将不绑定）。
:   This is to say that rather than properly being like `Pattern = Expression`, what we have is closer to `(fun (Pattern) -> true; (_) -> erlang:error(nomatch) end)(Expression)`: variables in the pattern's head *never* get bound across multiple assertions. The macro `?assertNotMatch` has been added to EUnit in R14B04.
：这就是说，我们所拥有的不是正确的“模式=表达”，而是更接近于“有趣（模式）->真实”；（->erlang:error（nomatch）end）（表达式）`：模式头部的变量*从不*跨多个断言绑定。宏`？assertNotMatch`已添加到R14B04中的EUnit中。

`?assertError(Pattern, Expression)`
:   Tells EUnit that `Expression` should result in an error. As an example, `?assertError(badarith, 1/0)` would be a successful test.
：告诉尤尼特“Expression”应该导致错误。例如，`？assertError（巴达利斯，1/0）`将是一次成功的测试。

`?assertThrow(Pattern, Expression)`
:   Exactly the same as `?assertError`, but with `throw(Pattern)` instead of `erlang:error(Pattern)`.
：与“”完全相同？assertError`，但用'throw（Pattern）`代替'erlang:error（Pattern）`。

`?assertExit(Pattern, Expression)`
:   Exactly the same as `?assertError`, but with `exit(Pattern)` (and not `exit/2`) instead of `erlang:error(Pattern)`.
：与“”完全相同？assertError`，但带有'exit（Pattern）'（而不是'exit/2`），而不是'erlang:error（Pattern）`。

`?assertException(Class, Pattern, Expression)`
:   A general form of the three previous macros. As an example, `?assertException(error, Pattern, Expression)` is the same as `?assertError(Pattern, Expression)`. Starting with R14B04, there is also the macro `?assertNotException/3` available for tests.
：前三个宏的一般形式。例如，`？assertException（错误、模式、表达式）`与`？assertError（模式、表达式）`。从R14B04开始，还有宏`？AssertNoteException/3`可用于测试。

Using these macros, we could write better tests in our module:

```erl
-module(ops_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    4 = ops:add(2,2).

new_add_test() ->
    ?assertEqual(4, ops:add(2,2)),
    ?assertEqual(3, ops:add(1,2)),
    ?assert(is_number(ops:add(1,2))),
    ?assertEqual(3, ops:add(1,1)),
    ?assertError(badarith, 1/0).
```

And running them:

```eshell
8> c(ops_tests).
./ops_tests.erl:12: Warning: this expression will fail with a 'badarith' exception

9> eunit:test(ops).
ops_tests: new_add_test...*failed*
::error:,
                           ,
                           ,
                           ,
                           
  in function ops_tests:'-new_add_test/0-fun-3-'/1
  in call from ops_tests:new_add_test/0


=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
error
```

See how much nicer the error reporting is? We know that the `assertEqual` on line 11 of `ops_tests` failed. When we called `ops:add(1,1)`, we thought we'd receive `3`. Of course you've got to read these values as Erlang terms, but at least they're there.
看看错误报告有多好？我们知道“ops_测试”第11行的“assertEqual”失败了。当我们调用'ops:add（1,1）'时，我们以为会收到'3'`。当然，你必须把这些值理解为Erlang术语，但至少它们是存在的。

What's annoying with this, however, is that even though we had 5 assertions, only one failed but the whole test was still considered a failure. It would be nicer to know that some assertion failed without behaving as if all the others after it failed too. Our test is the equivalent of taking an exam in school, and as soon as you make a mistake, you fail and get thrown out of school. Then your dog dies and you just have a horrible day.
然而，令人恼火的是，尽管我们有5个断言，但只有一个失败，但整个测试仍然被认为是失败的。如果知道有些断言失败了，而其他断言也失败了，那就更好了。我们的考试相当于在学校参加考试，一旦你犯了错误，你就会失败并被学校开除。然后你的狗死了，你就过了糟糕的一天。

## [Test Generators]

Because of this common need for flexibility, EUnit supports something called *test generators*. Test generators are pretty much shorthand for assertions wrapped in functions that can be run later, in clever manners. Instead of having functions ending with `_test()` with macros that are of the form `?assertSomething`, we will use functions that end in `_test_()` and macros of the form `?_assertSomething`. Those are small changes, but they make things much more powerful. The two following tests would be equivalent:
由于这种对灵活性的共同需求，EUnit支持一种叫做*测试生成器的东西*。测试生成器在很大程度上是封装在函数中的断言的简写，这些函数可以稍后以巧妙的方式运行。而不是让函数以`_test（）`结尾，宏的形式是`？assertSomething，我们将使用以“\u test\u（）”结尾的函数和形式为”的宏_断言某事`。这些都是微小的改变，但它们让事情变得更加强大。以下两项测试相当：

```erl
function_test() -> ?assert(A == B).
function_test_() -> ?_assert(A == B).
```

Here, `function_test_()` is called a *test generator function*, while `?_assert(A == B)` is called a *test generator*. It is called that way, because secretly, the underlying implementation of `?_assert(A == B)` is `fun() -> ?assert(A == B) end`. That is to say, a function that generates a test.
这里，`function_test_（）被称为*test generator function*，而`_assert（A==B）`称为*测试生成器*。之所以这样称呼，是因为暗地里，`_断言（A==B）`is`fun（）->？断言（A==B）结束`。也就是说，一个生成测试的函数。

The advantage of test generators, compared to regular assertions, is that they are funs. This means that they can be manipulated without being executed. We could, in fact, have *test sets* of the following form:
与常规断言相比，测试生成器的优势在于它们是FUN。这意味着他们可以在不被执行的情况下被操纵。事实上，我们可以有以下形式的测试集：

```erl
my_test_() ->
    [?_assert(A),
     [?_assert(B),
      ?_assert(C),
      [?_assert(D)]],
     [[?_assert(E)]]].
```

Test sets can be deeply nested lists of test generators. We could have functions that return tests! Let's add the following to [ops_tests](static/erlang/ops_tests.erl.html):
测试集可以是测试生成器的深度嵌套列表。我们可以有返回测试的函数！让我们在[ops_测试]（静态/erlang/ops_测试）中添加以下内容。呃。html）：

```erl
add_test_() ->
    [test_them_types(),
     test_them_values(),
     ?_assertError(badarith, 1/0)].

test_them_types() ->
    ?_assert(is_number(ops:add(1,2))).

test_them_values() ->
    [?_assertEqual(4, ops:add(2,2)),
     ?_assertEqual(3, ops:add(1,2)),
     ?_assertEqual(3, ops:add(1,1))].
```

Because only `add_test_()` ends in `_test_()`, the two functions `test_them_Something()` will not be seen as tests. In fact, they will only be called by `add_test_()` to generate tests:
因为只有“add_test_（）”以“_test_（）”结尾，所以两个函数“test_them_Something（）”将不会被视为测试。事实上，它们只会被'add_test_z（）'调用以生成测试：

```eshell
1> c(ops_tests).
./ops_tests.erl:12: Warning: this expression will fail with a 'badarith' exception
./ops_tests.erl:17: Warning: this expression will fail with a 'badarith' exception

2> eunit:test(ops).
ops_tests:25: test_them_values...*failed*
[...]
ops_tests: new_add_test...*failed*
[...]

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 5.
error
```

So we still get the expected failures, and now you see that we jumped from 2 tests to 7. The magic of test generators.
所以我们仍然得到了预期的失败，现在你看到我们从2个测试跳到了7个。测试生成器的魔力。

What if we only wanted to test some parts of the suite, maybe just `add_test_/0`? Well EUnit has a few tricks up its sleeves:
如果我们只想测试套件的某些部分，也许只是“添加测试”呢？嗯，尤尼特有一些窍门：

```eshell
3> eunit:test(). 
ops_tests:25: test_them_values...*failed*
::error:,
                           ,
                           ,
                           ,
                           
  in function ops_tests:'-test_them_values/0-fun-4-'/1

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
```

Note that this only works with test generator functions. What we have here as `` is what EUnit parlance calls a *test representation*. We have a few other representations:
请注意，这仅适用于测试生成器功能。我们这里所说的“是尤尼特·帕兰斯所说的*测试表示法”*。我们还有其他一些陈述：

-   `
-   `
-   `` runs all the tests found in a single compiled module
-   `` runs a single generator function as a test, as seen above
-   `'s `.app` file.

These different test representations can make it easy to run test suites for entire applications or even releases.
这些不同的测试表示可以使整个应用程序甚至是发布版的测试套件运行变得容易。

![a light fixture](../img/fixture.png "using a light fixture to shed some light on fixtures. Dur hurr")
![灯具](。。/静态/img/夹具。png“使用灯具照亮灯具”。杜尔·胡尔）

## [Fixtures]

It would still be pretty hard to test entire applications just by using assertions and test generators. This is why *fixtures* were added. Fixtures, while not being a catch-all solution to getting your tests up and running to the application level, allow you to build a certain scaffolding around tests.
仅仅使用断言和测试生成器来测试整个应用程序仍然非常困难。这就是为什么增加了固定装置。fixture虽然不是让测试启动并运行到应用程序级别的全面解决方案，但允许您围绕测试构建特定的框架。

The scaffolding in question is a general structure that allows us to define setup and teardown functions for each of the test. These functions will allow you to build the state and environment required for each of the tests to be useful. Moreover, the scaffolding will let you specify how to run the tests (do you want to run them locally, in separate processes, etc.?)
所讨论的脚手架是一种通用结构，允许我们为每个测试定义设置和拆卸功能。这些函数将允许您构建每个测试有用所需的状态和环境。此外，脚手架将允许您指定如何运行测试（您是否希望在本地、在单独的流程中运行测试，等等）。?)

There are a few types of fixtures available, with variations to them. The first type is simply called the *setup* fixture. A setup fixture takes one of the many following forms:
有几种类型的固定装置可供选择，但各有不同。第一种类型简单地称为“设置”夹具。安装夹具采用以下多种形式之一：

``` expand




```

Argh! It appears we need a little bit of EUnit vocabulary in order to understand this (this will be useful if you need to go read the EUnit documentation):
啊！为了理解这一点，我们似乎需要一些EUnit词汇（如果您需要阅读EUnit文档，这将非常有用）：

Setup
:   A function that takes no argument. Each of the tests will be passed the value returned by the setup function.
：一个不带参数的函数。每个测试都将通过设置函数返回的值。

Cleanup
:   A function that takes the result of a setup function as an argument, and takes care of cleaning up whatever is needed. If in OTP `terminate` does the opposite of `init`, then cleanup functions are the opposite of setup functions for EUnit.
：一个函数，它将设置函数的结果作为参数，并负责清理所需的任何内容。如果在OTP中，terminate与init相反，那么cleanup函数与EUnit的setup函数相反。

Instantiator
:   It's a function that takes the result of a setup function and returns a test set (remember, test sets are possibly deeply nested lists of `?_Macro` assertions).
：它是一个函数，接收设置函数的结果并返回测试集（请记住，测试集可能是深度嵌套的“？_宏”断言列表）。

Where
:   Specifies how to run the tests: `local`, `spawn`, ``.

Alright, so what does this look like in practice? Well, let's imagine some test to make sure that a fictive process registry correctly handles trying to register the same process twice, with different names:
好吧，那么这在实践中是什么样子的呢？好吧，让我们设想一些测试，以确保虚拟进程注册表正确处理尝试注册同一个进程两次（使用不同名称）的操作：

```erl
double_register_test_() ->
    {setup,
     fun start/0,               % setup function
     fun stop/1,                % teardown function
     fun two_names_one_pid/1}.  % instantiator

start() ->
     = registry:start_link(),
    Pid.

stop(Pid) ->
    registry:stop(Pid).

two_names_one_pid(Pid) ->
    ok = registry:register(Pid, quite_a_unique_name, self()),
    Res = registry:register(Pid, my_other_name_is_more_creative, self()),
    [?_assertEqual(, Res)].
```

This fixture first starts the [registry](static/erlang/processquest/apps/regis-1.0.0/src/regis_server.erl.html) server within the `start/0` function. Then, the instantiator `two_names_one_pid(ResultFromSetup)` is called. In that test, the only thing I do is try to register the current process twice.
该装置首先启动[registry]（static/erlang/processquest/apps/regis-1）。0。0/src/regis_服务器。呃。“开始/0”函数中的html）服务器。然后，实例化器'two_names_one_pid（ResultFromSetup）'被调用。在那个测试中，我唯一要做的就是尝试注册当前进程两次。

That's where the instantiator does its work. The result of the second registration is stored in the variable `Res`, Res)`). That test set will be run by EUnit. Then, the teardown function `stop/1` will be called. Using the pid returned by the setup function, it'll be able to shut down the registry that we had started beforehand. Glorious!
这就是实例化器工作的地方。第二次注册的结果存储在变量'Res'，Res）`）中。该测试集将由EUnit运行。然后，将调用拆卸函数'stop/1'。使用设置函数返回的pid，它将能够关闭我们之前启动的注册表。值得称道的

What's even better is that this whole fixture itself can be put inside a test set:

```erl
some_test_() ->
    [,
     ,
     ...
     ].
```

And this will work! What's annoying there is the need to always repeat that setup and teardown functions, especially when they're always the same. That's where the second type of fixture, the *foreach* fixture, enters the stage:
这就行了！令人恼火的是，总是需要重复设置和拆卸功能，尤其是当它们总是相同的时候。这就是第二类夹具，*foreach*夹具进入阶段的地方：

``` expand




```

The foreach fixture is quite similar to the setup fixture, with the difference that it takes lists of instantiators. Here's the `some_test_/0` function written with a foreach fixture:
foreach fixture与setup fixture非常相似，不同之处在于它需要实例化器列表。下面是用foreach fixture编写的'some_test_/0'函数：

```erl
some2_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun some_instantiator1/1,
      fun some_instantiator2/1,
      ...
      fun some_instantiatorN/1]}.
```

That's better. The foreach fixture will then take each of the instantiators and run the setup and teardown function for each of them.
那更好。然后，foreach fixture将获取每个实例化器，并为每个实例化器运行setup和teardown函数。

Now we know how to have a fixture for one instantiator, then a fixture for many of them (each getting their setup and teardown function calls). What if I want to have one setup function call, and one teardown function calls for many instantiators?
现在我们知道了如何为一个实例化器创建一个fixture，然后为其中的许多实例化器创建一个fixture（每个实例化器都得到了设置和拆卸函数调用）。如果我想要一个设置函数调用，一个拆卸函数调用多个实例化器，该怎么办？

In other words, what if I have many instantiators, but I want to set some state only once? There's no easy way for this, but here's a little trick that might do it:
换句话说，如果我有很多实例化器，但我只想设置一次某个状态，该怎么办？要做到这一点并不容易，但这里有一个小技巧：

```erl
some_tricky_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (SetupData) ->
        [some_instantiator1(SetupData),
         some_instantiator2(SetupData),
         ...
         some_instantiatorN(SetupData)]
     end}.
```

By using the fact that test sets can be deeply nested lists, we wrap a bunch of instantiators with an anonymous function behaving like an instantiator for them.
通过使用测试集可以是深度嵌套的列表这一事实，我们用一个匿名函数包装了一组实例化器，其行为类似于实例化器。

![A fat Spawn (the anti-hero comic book character)](../img/fatspawn.png)

Tests can also have some finer grained control into how they should be running when you use fixtures. Four options are available:
测试还可以对使用fixture时应该如何运行进行更细粒度的控制。有四种选择：

``
:   Runs tests in a separate process than the main test process. The test process will wait for all of the spawned tests to finish
：在主测试进程之外的单独进程中运行测试。测试过程将等待所有生成的测试完成

``
:   The tests will run for `Seconds` to finish, they will be terminated without further ado.

``
:   This tells EUnit to run the tests within the test set strictly in the order they are returned.

``
:   Where possible, the tests will be run in parallel.

As an example, the `some_tricky_test_/0` test generator could be rewritten as follows:

```erl
some_tricky2_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) ->
       {inparallel,
        [some_instantiator1(SetupData),
         some_instantiator2(SetupData),
         ...
         some_instantiatorN(SetupData)]}
     end}.
```

That's really most of it for fixtures, but there's one more nice trick I've forgot to show for now. You can give descriptions of tests in a neat way. Check this out:
这是赛程的大部分内容，但还有一个我现在忘了展示的好技巧。你可以用简洁的方式描述测试。看看这个：

```erl
double_register_test_() ->
    {"Verifies that the registry doesn't allow a single process to "
     "be registered under two names. We assume that each pid has the "
     "exclusive right to only one name",
     {setup,
      fun start/0,
      fun stop/1,
      fun two_names_one_pid/1}}.
```

Nice, huh? You can wrap a fixture by doing `` in order to get readable tests. Let's put this in practice.
不错吧？您可以通过执行``来包装夹具，以获得可读的测试。让我们把它付诸实践。

## [Testing Regis]

Because just seeing fake tests as above isn't the most entertaining thing to do, and because pretending to test software that doesn't exist is even worse, we'll instead study the tests I have written for the [regis-1.0.0](static/erlang/regis-1.0.0.zip) process registry, the one used by Process Quest.
因为仅仅看到上面的假测试并不是最有趣的事情，而且因为假装测试不存在的软件更糟糕，我们将研究我为[regis-1]编写的测试。0。0]（静态/erlang/regis-1）。0。0。zip）process registry，process Quest使用的一个。

![A portrait of Regis Philbin](../img/regis.png "Regis Philbin!")

Now, the development of `regis` was done in a test-driven manner. Hopefully you don't hate TDD (Test-Driven Development), but even if you do, it shouldn't be too bad because we'll look at the test suite after the fact. By doing this, we cut through the few trial-and-error sequences and backpedaling that I might have had writing it the first time and I'll look like I'm really competent, thanks to the magic of text editing.
现在，“regis”的开发是以测试驱动的方式完成的。希望你不会讨厌TDD（测试驱动开发），但即使你讨厌，也不会太糟糕，因为我们会在事后再看看测试套件。通过这样做，我们完成了我第一次写这篇文章时可能遇到的一些尝试和错误序列，并让我看起来真的很有能力，多亏了文本编辑的魔力。

The regis application is made of three processes: a supervisor, a main server, and then an application callback module. Knowing that the supervisor will only check the server and that the application callback module will do nothing except behaving as an interface for the two other modules, we can safely write a test suite focusing on the server itself, without any external dependencies.
regis应用程序由三个进程组成：一个主管、一个主服务器，然后是一个应用程序回调模块。由于知道主管只会检查服务器，而应用程序回调模块除了充当其他两个模块的接口之外，什么都不做，我们可以安全地编写一个针对服务器本身的测试套件，而不需要任何外部依赖项。

Being a good TDD fan, I begun by writing a list of all the features I wanted to cover:

-   Respect an interface similar to the Erlang default process registry
-   The Server will have a registered name so that it can be contacted without tracking its pid
-   A process can be registered through our service and can then be contacted by its name
-   A list of all registered processes can be obtained
-   A name that is not registered by any process should return the atom 'undefined' (much like the regular Erlang registry) in order to crash calls using them
-（使用“未定义”的atom调用在注册表中不应该像“未定义”的进程那样以“未定义”的顺序返回它们
-   A process can not have two names
-   Two processes can not share the same name
-   A process that was registered can be registered again if it was unregistered between calls
-   Unregistering a process never crashes
-   A registered process' crash will unregister its name

That's a respectable list. Doing the elements one by one and adding cases as I went, I transformed each of the specification into a test. The final file obtained was [regis_server_tests](static/erlang/processquest/apps/regis-1.0.0/test/regis_server_tests.erl.html). I wrote things using a basic structure a bit like this:
这是一份值得尊敬的名单。我一个接一个地处理这些元素，并在执行过程中添加案例，然后将每个规范转换为一个测试。最终获得的文件是[regis_server_tests]（static/erlang/processquest/apps/regis-1）。0。0/测试/注册服务器测试。呃。（html）。我用一种基本的结构写东西，有点像这样：

```erl
-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
```

Ok, I give it to you, that looks weird when the module is empty, but as you fill it up, it makes more and more sense.
好吧，我给你们讲讲，当模块是空的时候，这看起来很奇怪，但是当你把它填满时，它变得越来越有意义。

After adding a first test, the initial one being that it should be possible to start a server and access it by name, the file looked like this:
添加第一个测试后（第一个测试是应该可以启动服务器并按名称访问），文件如下所示：

```erl
-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     {setup,
      fun start/0,
      fun stop/1,
      fun is_registered/1}}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
     = regis_server:start_link(),
    Pid.

stop(_) ->
    regis_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(regis_server))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
```

See the organization now? Already so much better. The top part of the file contains only fixtures and top-level description of features. The second part contains setup and cleanup functions that we might need. The last one contains the instantiators returning test sets.
现在见组织了吗？已经好多了。文件的顶部仅包含装置和要素的顶级描述。第二部分包含我们可能需要的设置和清理功能。最后一个包含返回测试集的实例化器。

In this case, the instantiator checks to see whether `regis_server:start_link()` spawned a process that was truly alive, and that it was registered with the name `regis_server`. If it's true, then that will work for the server.
在本例中，实例化器检查“regis_server:start_link（）”是否生成了一个真正处于活动状态的进程，并且该进程已注册为“regis_server”`。如果这是真的，那么这将适用于服务器。

If we look at the current version of the file, it now looks more like this for the two first sections:
如果我们查看文件的当前版本，它现在看起来更像前两部分：

```erl
-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), ).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.

register_test_() ->
    [{"A process can be registered and contacted",
      ?setup(fun register_contact/1)},
     {"A list of registered processes can be obtained",
      ?setup(fun registered_list/1)},
     {"An undefined name should return 'undefined' to crash calls",
      ?setup(fun noregister/1)},
     {"A process can not have two names",
      ?setup(fun two_names_one_pid/1)},
     {"Two processes cannot share the same name",
      ?setup(fun two_pids_one_name/1)}].

unregister_test_() ->
    [{"A process that was registered can be registered again iff it was "
      "unregistered between both calls",
      ?setup(fun re_un_register/1)},
     {"Unregistering never crashes",
      ?setup(fun unregister_nocrash/1)},
     {"A crash unregisters a process",
      ?setup(fun crash_unregisters/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
     = regis_server:start_link(),
    Pid.

stop(_) ->
    regis_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet
```

Nice, isn't it? Note that as I was writing the suite, I ended up seeing that I never needed any other setup and teardown functions than `start/0` and `stop/1`. For this reason, I added the `?setup(Instantiator)` macro, that makes things look a bit better than if all the fixtures were to be fully expanded. It's now pretty obvious that I turned each point of the feature list into a bunch of tests. You'll note that I divided all tests depending on whether they had to do with starting and stopping the server (`start_stop_test_/0`), registering processes (`register_test_/0`) and unregistering processes (`unregister_test_/0`).
很好，不是吗？请注意，在编写该套件时，我发现除了“start/0”和“stop/1”之外，我再也不需要任何其他设置和拆卸功能了`。出于这个原因，我添加了`？setup（Instantiator）`macro，这会让事情看起来比完全扩展所有固定装置要好一点。现在很明显，我把特性列表中的每一点都变成了一系列测试。您会注意到，我根据是否必须启动和停止服务器（`start\u stop\u test\u0`）、注册进程（`register\u test\u0`）和注销进程（`unregister\u test\u0`）来划分所有测试。

By reading the test generators' definitions, we can know what the module is supposed to be doing. The tests become documentation (although they should not replace proper documentation).
通过阅读测试生成器的定义，我们可以知道模块应该做什么。测试成为文件（尽管它们不应取代适当的文件）。

We'll study the tests a bit and see why things were done in a certain way. The first test in the list `start_stop_test_/0`, with the simple requirement that the server can be registered:
我们将稍微研究一下测试，看看为什么事情是以某种方式进行的。列表中的第一个测试是“开始、停止、测试”，简单的要求是服务器可以注册：

```erl
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.
```

The implementation of the test itself is put in the `is_registered/1` function:

```erl
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(regis_server))].
```

![a heart monitor](../img/electrocardiogram.png "An impossible mission!")

As explained earlier when we looked at the first version of the test, this checks whether the process is available or not. There's nothing really special about that one, although the function `erlang:is_process_alive(Pid)` might be new to you. As its name says, it checks whether a process is currently running. I've put that test in there for the simple reason that it might well be possible that the server crashes as soon as we start it, or that it's never started in the first place. We don't want that.
正如前面我们在查看测试的第一个版本时所解释的，这将检查该过程是否可用。虽然函数“erlang:is_process_alive（Pid）”对您来说可能是新函数，但它并没有什么特别之处。顾名思义，它检查进程当前是否正在运行。我把这个测试放在那里，原因很简单，很可能是我们一启动服务器就崩溃了，或者根本就没有启动过。我们不想那样。

The second test is related to being able to register a process:

```erl
{"A process can be registered and contacted",
 ?setup(fun register_contact/1)}
```

Here's what the test looks like:

```erl
register_contact(_) ->
    Pid = spawn_link(fun() -> callback(regcontact) end),
    timer:sleep(15),
    Ref = make_ref(),
    WherePid = regis_server:whereis(regcontact),
    regis_server:whereis(regcontact) ! ,
    Rec = receive
          -> true
         after 2000 -> false
    end,
    [?_assertEqual(Pid, WherePid),
     ?_assert(Rec)].
```

Granted, this isn't the most elegant test around. What it does is that it spawns a process that will do nothing but register itself and reply to some message we send it. This is all done in the `callback/1` helper function defined as follows:
当然，这不是最优雅的测试。它所做的是，它产生了一个进程，除了注册自己并回复我们发送给它的某些消息之外，它什么也不做。这一切都是在“callback/1”助手函数中完成的，该函数定义如下：

```erl
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback(Name) ->
    ok = regis_server:register(Name, self()),
    receive
        
    end.
```

So the function has the module register itself, receives a message, and sends a response back. Once the process is started, the `register_contact/1` instantiator waits 15 milliseconds (just a tiny delay to make sure the other process registers itself), and then tries to use the `whereis` function from `regis_server` to retrieve a Pid and send a message to the process. If the regis server is functioning correctly, a message will be received back and the pids will match in the tests at the bottom of the function.
因此，该函数拥有模块寄存器本身，接收消息，并发送响应。一旦进程启动，“register_contact/1”实例化器等待15毫秒（只是一个微小的延迟，以确保其他进程自己注册），然后尝试使用“regis_server”中的“where is”函数检索Pid并向进程发送消息。如果regis服务器正常工作，将收到一条消息，PID将在功能底部的测试中匹配。

::: 
**Don't Drink Too Much Kool-Aid:**\
By reading that test, you have seen the little timer work we've had to do. Because of the concurrent and time-sensitive nature of Erlang programs, tests will frequently be filled with tiny timers like that that have the sole role of trying to synchronise bits of code.
通过阅读该测试，你已经看到了我们必须做的小计时器工作。由于Erlang程序的并发性和时间敏感性，测试中经常会充满这样的小计时器，它们的唯一作用是尝试同步代码位。

The problem then becomes to try and define what should be considered a good timer, a delay that is long enough. With a system running many tests or even a computer under heavy load, will the timers still be waiting for long enough?
然后问题就变成了试图定义什么应该被认为是好的计时器，一个足够长的延迟。当一个系统运行许多测试，甚至一台计算机处于重载状态时，计时器还会等待足够长的时间吗？

Erlang programmers who write tests sometimes have to be clever in order to minimize how much synchronisation they need to get things to work. There is no easy solution for it.
编写测试的Erlang程序员有时必须很聪明，以尽量减少工作所需的同步量。没有简单的解决办法。
:::

The next tests are introduced as follows:

```erl
{"A list of registered processes can be obtained",
 ?setup(fun registered_list/1)}
```

So when a bunch of processes have been registered, it should be possible to get a list of all the names. This is a functionality similar to Erlang's `registered()` function call:
因此，当注册了一系列进程时，应该可以得到所有名称的列表。这是一个类似于Erlang的'registered（）'函数调用的功能：

```erl
registered_list(_) ->
    L1 = regis_server:get_names(),
    Pids = [spawn(fun() -> callback(N) end) || N <- lists:seq(1,15)],
    timer:sleep(200),
    L2 = regis_server:get_names(),
    [exit(Pid, kill) || Pid <- Pids],
    [?_assertEqual([], L1),
     ?_assertEqual(lists:sort(lists:seq(1,15)), lists:sort(L2))].
```

First of all, we make sure that the first list of registered processes is empty (`?_assertEqual(L1, [])`) so that we've got something that works even when no process has ever tried to register itself. Then 15 processes are created, all of which will try to register themselves with a number (1..15). We make the test sleep a bit to make sure all processes have the time to register themselves, and then call `regis_server:get_names()`. The names should include all integers between 1 and 15, inclusively. Then a slight cleanup is done by eliminating all the registered processes --- we don't want to be leaking them, after all.
首先，我们要确保注册进程的第一个列表是空的（`？_assertEqual（L1，[]）`），这样即使在没有进程尝试注册自己的情况下，我们也能得到一些有效的东西。然后创建15个进程，所有进程都将尝试用一个数字（1）注册自己。。15)。我们让测试稍微休眠一点，以确保所有进程都有时间注册自己，然后调用'regis_server:get_names（）`。名称应包括1到15之间的所有整数。然后通过删除所有注册的进程来进行轻微的清理——毕竟，我们不想泄露它们。

![a pocket watch](../img/watch.png)

You'll notice the tendency of the tests to store state in variables (`L1`) before using them in test sets. The reason for this is that the test set that is returned is executed well after the test initiator (the whole active bit of code) has been running. If you were to try and put function calls that depend on other processes and time-sensitive events in the `?_assert*` macros, you'd get everything out of sync and things would generally be awful for you and the people using your software.
您会注意到，测试倾向于在变量（`L1`）中存储状态，然后再在测试集中使用它们。这是因为返回的测试集在测试发起程序（整个活动代码位）运行之后执行良好。如果要尝试将依赖于其他进程和时间敏感事件的函数调用放入`_断言*`macros，你会让一切都不同步，对你和使用你的软件的人来说，情况通常会很糟糕。

The next test is simple:

```erl
{"An undefined name should return 'undefined' to crash calls",
 ?setup(fun noregister/1)}

...

noregister(_) ->
    [?_assertError(badarg, regis_server:whereis(make_ref()) ! hi),
     ?_assertEqual(undefined, regis_server:whereis(make_ref()))].
```

As you can see, this tests for two things: we return `undefined`, and the specification's assumption that using `undefined` does indeed crash attempted calls. For that one, there is no need to use temporary variables to store the state: both tests can be executed at any time during the life of the regis server given we never change its state.
使用“undefined”和“undefined”规范，我们可以返回“undefined”测试。对于这一个，不需要使用临时变量来存储状态：这两个测试都可以在regis服务器生命周期内的任何时间执行，因为我们永远不会更改其状态。

Let's keep going:

```erl
{"A process can not have two names",
 ?setup(fun two_names_one_pid/1)},

...

two_names_one_pid(_) ->
    ok = regis_server:register(make_ref(), self()),
    Res = regis_server:register(make_ref(), self()),
    [?_assertEqual(, Res)].
```

That's pretty much the same test we used in a demo in the previous section of the chapter. In this one, we're just looking to see whether we get the right output and that the test process can't register itself twice with different names.
这与我们在本章前一节的演示中使用的测试基本相同。在这个例子中，我们只是想看看我们是否得到了正确的输出，测试过程不能用不同的名称注册两次。

::: note
**Note:** you might have noticed that the tests above tend to use `make_ref()` a whole lot. When possible, it is useful to use functions that generate unique values like `make_ref()` does. If at some point in the future someone wants to run tests in parallel or to run them under a single regis server that never stops, then it will be possible to do so without needing to modify the tests.
**注：**您可能已经注意到，上面的测试经常使用“make_ref（）”。如果可能，可以使用生成唯一值的函数，如“make_ref（）”does。如果将来某个时候有人想并行运行测试，或者在一台永不停止的regis服务器下运行测试，那么不需要修改测试就可以这样做。

If we were to use hard coded names like `a`, `b`, and `c` in all the tests, then it would be very likely that sooner or later, name conflicts would happen if we were to try and run many test suites at once. Not all tests in the `regis_server_tests` suite follow this advice, mostly for demonstration purposes.
如果我们在所有测试中都使用硬编码名称，比如'a'、'b'和'c'，那么如果我们尝试同时运行多个测试套件，很可能迟早会发生名称冲突。并非“regis_server_tests”套件中的所有测试都遵循此建议，主要是为了演示。
:::

The next tests is the opposite of `two_names_one_pid`:

```erl
{"Two processes cannot share the same name",
 ?setup(fun two_pids_one_name/1)}].

...

two_pids_one_name(_) ->
    Pid = spawn(fun() -> callback(myname) end),
    timer:sleep(15),
    Res = regis_server:register(myname, self()),
    exit(Pid, kill),
    [?_assertEqual(, Res)].
```

Here, because we need two processes and the results of only one of them, the trick is to spawn one process (the one whose results we do not need), and then do the critical part ourselves.
在这里，因为我们需要两个过程，并且只需要其中一个的结果，所以诀窍是生成一个过程（其结果我们不需要），然后自己完成关键部分。

You can see that timers are used to make sure that the other process tries registering a name first (within the `callback/1` function), and that the test process itself waits to try at its turn, expecting an error tuple (``) as a result.
您可以看到，计时器用于确保另一个进程先尝试注册一个名称（在`callback/1`函数中），而测试进程本身则等待轮到它进行尝试，并期望结果是一个错误元组（``）。

This covers all the features for the tests related to the registration of processes. Only those related to unregistering processes are left:
这涵盖了与流程注册相关的测试的所有功能。只剩下与注销流程相关的内容：

```erl
unregister_test_() ->
    [{"A process that was registered can be registered again iff it was "
      "unregistered between both calls",
      ?setup(fun re_un_register/1)},
     {"Unregistering never crashes",
      ?setup(fun unregister_nocrash/1)},
     {"A crash unregisters a process",
      ?setup(fun crash_unregisters/1)}].
```

Let's see how they are to be implemented. The first one is kind of simple:

```erl
re_un_register(_) ->
    Ref = make_ref(),
    L = [regis_server:register(Ref, self()),
         regis_server:register(make_ref(), self()),
         regis_server:unregister(Ref),
         regis_server:register(make_ref(), self())],
    [?_assertEqual([ok, , ok, ok], L)].
```

This way of serializing all the calls in a list is a nifty trick I like to do when I need to test the results of all the events. By putting them in a list, I can then compare the sequence of actions to the expected `[ok, , ok, ok]` to see how things went. Note that there is nothing specifying that Erlang should evaluate the list in order, but the trick above has pretty much always worked.
当我需要测试所有事件的结果时，这种序列化列表中所有调用的方法是我喜欢做的一个巧妙的技巧。通过将它们放在一个列表中，我可以将动作序列与预期的“[ok，ok，ok]”进行比较，看看事情是如何进行的。请注意，并没有规定Erlang应该按顺序计算列表，但上面的技巧基本上一直有效。

The following test, the one about never crashing, goes like this:

```erl
unregister_nocrash(_) ->
    ?_assertEqual(ok, regis_server:unregister(make_ref())).
```

Whoa, slow down here, buddy! That's it? Yes it is. If you look back at `re_un_register`, you'll see that it already handles testing the 'unregistration' of processes. For `unregister_nocrash`, we really only want to know if it will work to try and remove a process that's not there.
慢点，伙计！就这样？是的。如果你回顾一下're_un_register'，你会发现它已经在测试进程的'unregistation'。对于“unregister_nocrash”，我们只想知道尝试删除一个不存在的进程是否有效。

Then comes the last test, and one of the most important ones for any test registry you'll ever have: a named process that crashes will have the name unregistered. This has serious implications, because if you didn't remove names, you'd end up having an ever growing registry server with an ever shrinking name selection:
然后是最后一个测试，也是对任何测试注册表来说最重要的一个测试：一个崩溃的命名进程的名称将被取消注册。这有着严重的影响，因为如果你不删除名称，你最终会拥有一个不断增长的注册表服务器，其名称选择会不断减少：

```erl
crash_unregisters(_) ->
    Ref = make_ref(),
    Pid = spawn(fun() -> callback(Ref) end),
    timer:sleep(150),
    Pid = regis_server:whereis(Ref),
    exit(Pid, kill),
    timer:sleep(95),
    regis_server:register(Ref, self()),
    S = regis_server:whereis(Ref),
    Self = self(),
    ?_assertEqual(Self, S).
```

This one reads sequentially:

1.  Register a process
2.  Make sure the process is registered
3.  Kill that process
4.  Steal the process' identity (the true spy way)
5.  Check whether we do hold the name ourselves.

In all honesty, the test could have been written in a simpler manner:

```erl
crash_unregisters(_) ->
    Ref = make_ref(),
    Pid = spawn(fun() -> callback(Ref) end),
    timer:sleep(150),
    Pid = regis_server:whereis(Ref),
    exit(Pid, kill),
    ?_assertEqual(undefined, regis_server:whereis(Ref)).
```

That whole part about stealing the identity of the dead process was nothing but a petty thief's fantasy.
关于盗取死者身份的整个过程只是一个小偷的幻想。

That's it! If you've done things right, you should be able to compile the code and run the test suite:
就这样！如果你做得对，你应该能够编译代码并运行测试套件：

```eshell
$ erl -make
Recompile: src/regis_sup
...
$ erl -pa ebin/
1> eunit:test(regis_server).
  All 13 tests passed.
ok
2> eunit:test(regis_server, [verbose]).
======================== EUnit ========================
module 'regis_server'
  module 'regis_server_tests'
    The server can be started, stopped and has a registered name
      regis_server_tests:49: is_registered...ok
      regis_server_tests:50: is_registered...ok
      [done in 0.006 s]
...
  [done in 0.520 s]
=======================================================
  All 13 tests passed.
ok
```

Oh yeah, see how adding the 'verbose' option will add test descriptions and run time information to the reports? That's neat.
哦，是的，看看添加“verbose”选项如何将测试描述和运行时信息添加到报告中？太好了。

![a ball of yarn](../img/knit.png)

## [He Who Knits EUnits]

In this chapter, we've seen how to use most features of EUnit, how to run suites written in them. More importantly, we've seen a few techniques related to how to write tests for concurrent processes, using patterns that make sense in the real world.
在本章中，我们已经了解了如何使用EUnit的大多数功能，以及如何运行用它们编写的套件。更重要的是，我们已经看到了一些与如何使用在现实世界中有意义的模式为并发进程编写测试相关的技术。

One last trick should be known: when you feel like testing processes such as `gen_server`s and `gen_fsm`s, you might feel like inspecting the internal state of the processes. Here's a nice trick, courtesy of the [sys](http://erldocs.com/18.0/stdlib/sys.html) module:
最后一个诀窍是：当你想测试诸如“gen_server”和“gen_fsm”之类的进程时，你可能会想检查进程的内部状态。这是一个很好的技巧，来自[sys](http://erldocs。com/18。0/stdlib/sys。html）模块：

```eshell
3> regis_server:start_link().

4> regis_server:register(shell, self()).
ok
5> sys:get_status(whereis(regis_server)).
{status,<0.160.0>,
        ,
        [[,
          ],
         running,<0.31.0>,[],
         [,
          ,
                 ,
                 ,
          {data,[{"State",
                  ,
                         
```

Neat, huh? Everything that has to do with the server's innards is given to you: you can now inspect everything you need, all the time!
整洁，嗯？与服务器内部有关的一切都交给了您：您现在可以随时检查所需的一切！

If you feel like getting more comfortable with testing servers and whatnot, I recommend reading the [tests written for Process Quests' player module](static/erlang/processquest/apps/processquest-1.1.0/test/pq_player_tests.erl.html). They test the gen_server using a different technique, where all individual calls to `handle_call`, `handle_cast` and `handle_info` are tried independently. It was still developed in a test-driven manner, but the needs of that one forced things to be done differently.
如果你想更自如地测试服务器等等，我建议你阅读[tests Write for Process Quests'player module]（static/erlang/processquest/apps/processquest-1）。1.。0/测试/pq_玩家_测试。呃。（html）。他们使用不同的技术测试gen_服务器，其中对“handle_call”、“handle_cast”和“handle_info”的所有单独调用都是独立尝试的。它仍然是以测试驱动的方式开发的，但这种方式的需求迫使事情以不同的方式进行。

In any case, we'll see the true value of tests when we rewrite the process registry to use ETS, an in-memory database available for all Erlang processes.
在任何情况下，当我们重写进程注册表以使用ETS时，我们都会看到测试的真正价值，ETS是一个可用于所有Erlang进程的内存数据库。
