# What is OTP?

## [It's The Open Telecom Platform!]

![A telephone with someone on the other end saying 'Hullo'](../img/hullo.png "THE QUESTION IS COMING FROM INSIDE THE HOUSE")
![电话另一端有人在说‘你好’](。。/静态/img/hullo。png“问题来自房子内部”）

OTP stands for *Open Telecom Platform*, although it's not that much about telecom anymore (it's more about software that has the property of telecom applications, but yeah.) If half of Erlang's greatness comes from its concurrency and distribution and the other half comes from its error handling capabilities, then the OTP framework is the third half of it.
OTP代表“开放的电信平台”，虽然它不再是关于电信的了（它更多的是关于具有电信应用属性的软件，但是是的）。)如果Erlang的伟大之处有一半来自它的并发性和分布，另一半来自它的错误处理能力，那么OTP框架就是它的第三部分。

During the previous chapters, we've seen a few examples of common practices on how to write concurrent applications with the languages' built-in facilities: links, monitors, servers, timeouts, trapping exits, etc. There were a few 'gotchas' here and there on the order things need to be done, on how to avoid race conditions or to always remember that a process could die at any time. There was also hot code loading, naming processes and adding supervisors, to name a few.
在前几章中，我们已经看到了一些关于如何使用语言内置功能编写并发应用程序的常见实践示例：链接、监视器、服务器、超时、陷阱出口等。在事情需要完成的顺序上，在如何避免种族条件或始终记住一个过程随时可能死亡的问题上，到处都有一些“陷阱”。此外，还有热代码加载、命名流程和添加主管等。

Doing all of this manually is time consuming and sometimes prone to error. There are corner cases to be forgotten about and pits to fall into. The OTP framework takes care of this by grouping these essential practices into a set of libraries that have been carefully engineered and battle-hardened over years. Every Erlang programmer should use them.
手动执行所有这些操作非常耗时，有时还容易出错。有一些角落的案子要忘记，也有一些深坑要掉进去。OTP框架通过将这些基本实践分组到一组经过多年精心设计和实战验证的库中来解决这一问题。每个Erlang程序员都应该使用它们。

The OTP framework is also a set of modules and standards designed to help you build applications. Given most Erlang programmers end up using OTP, most Erlang applications you'll encounter in the wild will tend to follow these standards.
OTP框架也是一组模块和标准，旨在帮助您构建应用程序。鉴于大多数Erlang程序员最终使用OTP，您在野外遇到的大多数Erlang应用程序都会遵循这些标准。

## [The Common Process, Abstracted]

One of the things we've done many times in the previous process examples is divide everything in accordance to very specific tasks. In most processes, we had a function in charge of spawning the new process, a function in charge of giving it its initial values, a main loop, etc.
在前面的流程示例中，我们多次做的一件事是根据非常具体的任务划分所有内容。在大多数进程中，我们有一个函数负责生成新进程，一个函数负责为其提供初始值，一个主循环，等等。

These parts, as it turns out, are usually present in all concurrent programs you'll write, no matter what the process might be used for.
事实证明，这些部分通常存在于您将要编写的所有并发程序中，无论该过程可能用于什么目的。

![common process pattern: spawn -\> init -\> loop -\> exit](../img/common-pattern.png)

The engineers and computer scientists behind the OTP framework spotted these patterns and included them in a bunch of common libraries. These libraries are built with code that is equivalent to most of the abstractions we used (like using references to tag messages), with the advantage of being used for years in the field and also being built with far more caution than we were with our implementations. They contain functions to safely spawn and initialize processes, send messages to them in a fault-tolerant manner and many other things. Funnily enough, you should rarely need to use these libraries yourself. The abstractions they contain are so basic and universal that a lot more interesting things were built on top of them. Those libraries are the ones we'll use.
OTP框架背后的工程师和计算机科学家发现了这些模式，并将它们包含在一系列公共库中。这些库使用的代码相当于我们使用的大多数抽象（比如使用引用来标记消息），其优点是在该领域使用了多年，而且在构建时比我们的实现要谨慎得多。它们包含安全生成和初始化进程、以容错方式向进程发送消息等功能。有趣的是，您应该很少需要自己使用这些库。它们包含的抽象是如此基本和普遍，以至于在它们之上构建了许多有趣的东西。我们将使用这些库。

![graph of Erlang/OTP abstraction layers: Erlang -\> Basic Abstraction Libraries (gen, sys, proc_lib) -\> Behaviours (gen\_\*, supervisors)](../img/abstraction-layers.png)
![Erlang/OTP抽象层图：Erlang-\>基本抽象库（gen、sys、proc_lib）->行为（gen\\\*、supervisors）](。。/静态/img/抽象层。（巴布亚新几内亚）

In the following chapters we'll see a few of the common uses of processes and then how they can be abstracted, then made generic. Then for each of these we'll also see the corresponding implementation with the OTP framework's behaviours and how to use each of them.
在接下来的章节中，我们将看到流程的一些常见用法，以及如何将其抽象，然后使其通用。然后，我们还将看到OTP框架行为的相应实现，以及如何使用它们。

## [The Basic Server]

The first common pattern I'll describe is one we've already used. When writing the [event server](designing-a-concurrent-application.html), we had what could be called a *client-server model*. The event server would receive calls from the client, act on them and then reply to it if the protocol said to do so.
我将描述的第一个常见模式是我们已经使用过的模式。在编写[事件服务器]（设计并发应用程序）时。html），我们有一个可以被称为*客户机-服务器模型*。事件服务器将接收来自客户机的呼叫，对其进行操作，然后在协议要求的情况下进行回复。

For this chapter, we'll use a very simple server, allowing us to focus on the essential properties of it. Here's the [kitty_server](static/erlang/kitty_server.erl.html):
在本章中，我们将使用一个非常简单的服务器，使我们能够集中讨论它的基本属性。这是[kitty_服务器]（静态/erlang/kitty_服务器。呃。html）：

```erl
%%%%% Naive version
-module(kitty_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, ).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ,
    receive
         ->
            erlang:demonitor(Ref, [flush]),
            Cat;
         ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% This call is asynchronous
return_cat(Pid, Cat = #cat) ->
    Pid ! ,
    ok.

%% Synchronous call
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ,
    receive
         ->
            erlang:demonitor(Ref, [flush]),
            ok;
         ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
    
%%% Server functions
init() -> loop([]).

loop(Cats) ->
    receive
         ->
            if Cats =:= [] ->
                Pid ! ,
                loop(Cats); 
               Cats =/= [] -> % got to empty the stock
                Pid ! ,
                loop(tl(Cats))
            end;
         ->
            loop([Cat|Cats]);
         ->
            Pid ! ,
            terminate(Cats);
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.
```

So this is a kitty server/store. The behavior is extremely simple: you describe a cat and you get that cat. If someone returns a cat, it's added to a list and is then automatically sent as the next order instead of what the client actually asked for (we're in this kitty store for the money, not smiles):
这是一个kitty服务器/商店。行为非常简单：你描述一只猫，你就得到了那只猫。如果有人退回一只猫，它会被添加到列表中，然后自动作为下一个订单发送，而不是客户实际要求的（我们在这家kitty商店是为了钱，不是为了微笑）：

```eshell
1> c(kitty_server).

2> rr(kitty_server).
[cat]
3> Pid = kitty_server:start_link().
<0.57.0>
4> Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
#cat{name = carl,color = brown,
     description = "loves to burn bridges"}
5> kitty_server:return_cat(Pid, Cat1).
ok
6> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = carl,color = brown,
     description = "loves to burn bridges"}
7> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
#cat
8> kitty_server:return_cat(Pid, Cat1).
ok
9> kitty_server:close_shop(Pid).
carl was set free.
ok
10> kitty_server:close_shop(Pid).
** exception error: no such process or port
     in function  kitty_server:close_shop/1
```

Looking back at the source code for the module, we can see patterns we've previously applied. The sections where we set monitors up and down, apply timers, receive data, use a main loop, handle the init function, etc. should all be familiar. It should be possible to abstract away these things we end up repeating all the time.
回顾模块的源代码，我们可以看到之前应用的模式。我们在其中设置监视器、应用定时器、接收数据、使用主循环、处理init函数等。大家都应该熟悉吗。我们应该有可能把这些我们最终一直重复的东西抽象掉。

Let's first take a look at the client API. The first thing we can notice is that both synchronous calls are extremely similar. These are the calls that would likely go in abstraction libraries as mentioned in the previous section. For now, we'll just abstract these away as a single function in a [new module](static/erlang/my_server.erl.html) which will hold all the generic parts of the kitty server:
我们首先来看一下客户端API。我们可以注意到的第一件事是，两个同步调用非常相似。如前一节所述，这些调用可能会出现在抽象库中。现在，我们将把它们抽象为[新模块]（static/erlang/my_server）中的单个函数。呃。html）将保存kitty服务器的所有通用部分：

```erl
-module(my_server).
-compile(export_all).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ,
    receive
         ->
            erlang:demonitor(Ref, [flush]),
            Reply;
         ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
```

This takes a message and a PID, sticks them into in the function, then forwards the message for you in a safe manner. From now on, we can just substitute the message sending we do with a call to this function. So if we were to rewrite a new kitty server to be paired with the abstracted `my_server`, it could begin like this:
这将获取一条消息和一个PID，将它们粘贴到函数中，然后以安全的方式为您转发消息。从现在起，我们可以用调用这个函数来代替发送消息。因此，如果我们要重写一个新的kitty服务器，与抽象的“my_server”配对，它可以这样开始：

```erl
-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, ).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, ).

%% This call is asynchronous
return_cat(Pid, Cat = #cat) ->
    Pid ! ,
    ok.

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).
```

The next big generic chunk of code we have is not as obvious as the `call/2` function. Note that every process we've written so far has a loop where all the messages are pattern matched. This is a bit of a touchy part, but here we have to separate the pattern matching from the loop itself. One quick way to do it would be to add:
我们拥有的下一大块通用代码没有“call/2”函数那么明显。请注意，到目前为止，我们编写的每个流程都有一个循环，其中所有消息都是模式匹配的。这是一个有点棘手的部分，但这里我们必须将模式匹配与循环本身分开。一个快速的方法是添加：

```erl
loop(Module, State) ->
    receive
        Message -> Module:handle(Message, State)
    end.
```

And then the specific module can look like this:

```erl
handle(Message1, State) -> NewState1;
handle(Message2, State) -> NewState2;
...
handle(MessageN, State) -> NewStateN.
```

This is better. There are still ways to make it even cleaner. If you paid attention when reading the `kitty_server` module (and I hope you did!), you will have noticed we have a specific way to call synchronously and another one to call asynchronously. It would be pretty helpful if our generic server implementation could provide a clear way to know which kind of call is which.
这样更好。仍然有办法让它更干净。如果你在阅读“kitty_服务器”模块时注意到了（我希望你注意到了！），您会注意到，我们有一种特定的方式来同步调用，另一种方式是异步调用。如果我们的通用服务器实现能够提供一种明确的方式来知道哪种调用是哪种调用，那将非常有帮助。

In order to do this, we will need to match different kinds of messages in `my_server:loop/2`. This means we'll need to change the `call/2` function a little bit so synchronous calls are made obvious by adding the atom `sync` to the message on the function's second line:
为了做到这一点，我们需要在“my_server:loop/2”中匹配不同类型的消息`。这意味着我们需要稍微更改'call/2'函数，以便通过在函数第二行的消息中添加原子'sync'，使同步调用变得明显：

```erl
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ,
    receive
         ->
            erlang:demonitor(Ref, [flush]),
            Reply;
         ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
```

We can now provide a new function for asynchronous calls. The function `cast/2` will handle this:

```erl
cast(Pid, Msg) ->
    Pid ! ,
    ok.
```

With this done, the loop can now look like this:

```erl
loop(Module, State) ->
    receive
         ->
             loop(Module, Module:handle_cast(Msg, State));
         ->
             loop(Module, Module:handle_call(Msg, Pid, Ref, State))
    end.
```

![A kitchen sink](../img/sink.png "get it? GET IT?")

And then you could also add specific slots to handle messages that don't fit the sync/async concept (maybe they were sent by accident) or to have your debug functions and other stuff like hot code reloading in there.
然后，您还可以添加特定的插槽来处理不符合同步/异步概念的消息（可能是意外发送的），或者在其中添加调试功能和其他类似热代码重新加载的内容。

One disappointing thing with the loop above is that the abstraction is leaking. The programmers who will use `my_server` will still need to know about references when sending synchronous messages and replying to them. That makes the abstraction useless. To use it, you still need to understand all the boring details. Here's a quick fix for it:
上面的循环有一个令人失望的地方，那就是抽象正在泄漏。将使用“my\u server”的程序员在发送同步消息和回复消息时仍需要了解引用。这使得抽象变得毫无用处。要使用它，你仍然需要理解所有无聊的细节。这里有一个快速解决方法：

```erl
loop(Module, State) ->
    receive
         ->
             loop(Module, Module:handle_cast(Msg, State));
         ->
             loop(Module, Module:handle_call(Msg, , State))
    end.
```

By putting both variables `Pid` contains:

```erl
reply(, Reply) ->
    Pid ! .
```

What is left to do is specify the starter functions (`start`, `start_link` and `init`) that pass around the module names and whatnot. Once they're added, the module should look like this:
剩下要做的就是指定传递模块名等的启动函数（`start`、`start_link`和`init`）。添加后，模块应如下所示：

```erl
-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ,
    receive
         ->
            erlang:demonitor(Ref, [flush]),
            Reply;
         ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! ,
    ok.

reply(, Reply) ->
    Pid ! .

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
         ->
             loop(Module, Module:handle_cast(Msg, State));
         ->
             loop(Module, Module:handle_call(Msg, , State))
    end.
```

The next thing to do is reimplement the kitty server, now `kitty_server2` as a callback module that will respect the interface we defined for `my_server`. We'll keep the same interface as the previous implementation, except all the calls are now redirected to go through `my_server`:
接下来要做的事情是重新实现kitty服务器，现在是“kitty_server2”，作为一个回调模块，它将尊重我们为“my_server”定义的接口`。我们将保持与前一个实现相同的接口，只是所有调用现在都被重定向到“my_server”：

```erl
-module(kitty_server2).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, ).

%%% Client API
start_link() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, ).

%% This call is asynchronous
return_cat(Pid, Cat = #cat) ->
    my_server:cast(Pid, ).

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).
```

Note that I added a second `-export()` at the top of the module. Those are the functions `my_server` will need to call to make everything work:
我在第二个模块的顶部添加了`-export（）。这些是“my_server”需要调用的函数，以使一切正常工作：

```erl
%%% Server functions
init([]) -> []. %% no treatment of info here!

handle_call(, From, Cats) ->
    if Cats =:= [] ->
        my_server:reply(From, make_cat(Name, Color, Description)),
        Cats;
       Cats =/= [] ->
        my_server:reply(From, hd(Cats)),
        tl(Cats)
    end;

handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast(, Cats) ->
    [Cat|Cats].
```

And then what needs to be done is to re-add the private functions:

```erl
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    exit(normal).
```

Just make sure to replace the `ok` we had before by `exit(normal)` in `terminate/1`, otherwise the server will keep going on.
只需确保在'terminate/1'中用'exit（normal）'替换之前的'ok'，否则服务器将继续运行。

The code should be compilable and testable, and run in exactly the same manner as it was before. The code is quite similar, but let's see what changed.
代码应该是可编译和可测试的，并以与以前完全相同的方式运行。代码非常相似，但让我们看看发生了什么变化。

## [Specific Vs. Generic]

What we've just done is get an understanding the core of OTP (conceptually speaking). This is what OTP really is all about: taking all the generic components, extracting them in libraries, making sure they work well and then reusing that code when possible. Then all that's left to do is focus on the specific stuff, things that will always change from application to application.
我们刚刚做的是理解OTP的核心（从概念上讲）。这就是OTP真正的意义：获取所有通用组件，在库中提取它们，确保它们工作良好，然后尽可能重用代码。然后剩下要做的就是专注于特定的东西，这些东西在不同的应用程序中总是会发生变化。

Obviously, there isn't much to save by doing things that way with only the kitty server. It looks a bit like abstraction for abstraction's sake. If the app we had to ship to a customer were nothing but the kitty server, then the first version might be fine. If you're going to have larger applications then it might be worth it to separate generic parts of your code from the specific sections.
显然，仅使用kitty服务器这样做并没有多少节约。为了抽象，它看起来有点像抽象。如果我们必须交付给客户的应用程序只不过是kitty服务器，那么第一版就可以了。如果你将有更大的应用程序，那么将代码的通用部分与特定部分分开可能是值得的。

Let's imagine for a moment that we have some Erlang software running on a server. Our software has a few kitty servers running, a veterinary process (you send your broken kitties and it returns them fixed), a kitty beauty salon, a server for pet food, supplies, etc. Most of these can be implemented with a client-server pattern. As time goes, your complex system becomes full of different servers running around.
让我们想象一下，我们在服务器上运行一些Erlang软件。我们的软件有几个小猫服务器在运行，一个兽医程序（你把坏掉的小猫送去，它会把它们修好），一个小猫美容院，一个宠物食品和用品的服务器，等等。其中大部分可以通过客户机-服务器模式实现。随着时间的推移，您的复杂系统中会充满各种各样的服务器。

Adding servers adds complexity in terms of code, but also in terms of testing, maintenance and understanding. Each implementation might be different, programmed in different styles by different people, and so on. However, if all these servers share the same common `my_server` abstraction, you substantially reduce that complexity. You understand the basic concept of the module instantly (\"oh, it's a server!\"), there's a single generic implementation of it to test, document, etc. The rest of the effort can be put on each specific implementation of it.
添加服务器增加了代码方面的复杂性，但也增加了测试、维护和理解方面的复杂性。每个实现可能不同，由不同的人以不同的风格编程，等等。但是，如果所有这些服务器都共享相同的“my_server”抽象，则可以大大降低这种复杂性。你立刻就理解了模块的基本概念（\“哦，它是一个服务器！\”），有一个单一的通用实现来测试、记录等等。剩下的工作可以放在它的每个具体实现上。

![A dung beetle pushing its crap](../img/dung.png "This is me pushing my code in production")

This means you reduce a lot of time tracking and solving bugs (just do it at one place for all servers). It also means that you reduce the number of bugs you introduce. If you were to re-write the `my_server:call/3` or the process' main loop all the time, not only would it be more time consuming, but chances of forgetting one step or the other would skyrocket, and so would bugs. Fewer bugs mean fewer calls during the night to go fix something, which is definitely good for all of us. Your mileage may vary, but I'll bet you don't appreciate going to the office on days off to fix bugs either.
这意味着您减少了大量跟踪和解决bug的时间（只需在一个地方对所有服务器执行此操作）。这也意味着减少了引入的bug数量。如果你一直在重写'my_server:call/3'或process'主循环，不仅会耗费更多时间，而且忘记某一步或另一步的可能性会急剧上升，bug也会如此。更少的bug意味着晚上打更少的电话去修复一些东西，这对我们所有人来说都绝对是好事。你的里程可能会有所不同，但我敢打赌，你也不喜欢在休息日去办公室修复bug。

Another interesting thing about what we did when separating the generic from the specific is that we instantly made it much easier to test our individual modules. If you wanted to unit test the old kitty server implementation, you'd need to spawn one process per test, give it the right state, send your messages and hope for the reply you expected. On the other hand, our second kitty server only requires us to run the function calls over the 'handle_call/3' and 'handle_cast/2' functions and see what they output as a new state. No need to set up servers, manipulate the state. Just pass it in as a function parameter. Note that this also means the generic aspect of the server is much easier to test given you can just implement very simple functions that do nothing else than let you focus on the behaviour you want to observe, without the rest.
另一个有趣的事情是，当我们将泛型和特定模块分开时，我们立即使测试单个模块变得更加容易。如果你想对旧的kitty服务器实现进行单元测试，你需要在每个测试中产生一个进程，给它一个正确的状态，发送消息，并希望得到你期望的回复。另一方面，我们的第二个kitty服务器只需要我们在“handle_call/3”和“handle_cast/2”函数上运行函数调用，并查看它们作为新状态输出的内容。不需要设置服务器，操纵状态。只需将其作为函数参数传入即可。请注意，这也意味着服务器的通用方面更容易测试，因为您只需实现非常简单的功能，这些功能只需让您专注于想要观察的行为，而无需其他功能。

A much more 'hidden' advantage of using common abstractions in that way is that if everyone uses the exact same backend for their processes, when someone optimizes that single backend to make it a little bit faster, every process using it out there will run a little bit faster too. For this principle to work in practice, it's usually necessary to have a whole lot of people using the same abstractions and putting effort on them. Luckily for the Erlang community, that's what happens with the OTP framework.
以这种方式使用公共抽象的一个更“隐藏”的优势是，如果每个人都为自己的流程使用完全相同的后端，当有人优化单个后端以使其更快一点时，每个使用它的流程也会运行得更快一点。为了让这一原则在实践中发挥作用，通常有必要让很多人使用相同的抽象概念，并在这些抽象概念上下功夫。幸运的是，对于Erlang社区来说，OTP框架就是这样。

Back to our modules. There are a bunch of things we haven't yet addressed: named processes, configuring the timeouts, adding debug information, what to do with unexpected messages, how to tie in hot code loading, handling specific errors, abstracting away the need to write most replies, handling most ways to shut a server down, making sure the server plays nice with supervisors, etc. Going over all of this is superfluous for this text, but would be necessary in real products that need to be shipped. Again, you might see why doing all of this by yourself is a bit of a risky task. Luckily for you (and the people who'll support your applications), the Erlang/OTP team managed to handle all of that for you with the gen_server behaviour. `gen_server` is a bit like `my_server` on steroids, except it has years and years of testing and production use behind it.
回到我们的模块。有很多事情我们还没有解决：命名进程、配置超时、添加调试信息、如何处理意外消息、如何绑定热代码加载、处理特定错误、抽象出编写大多数回复的需要、处理关闭服务器的大多数方法、确保服务器与管理员配合良好等等。对于本文来说，回顾所有这些都是多余的，但在需要运输的实际产品中是必要的。同样，你可能会明白为什么独自完成所有这些任务有点冒险。幸运的是，对于您（以及支持您的应用程序的人员），Erlang/OTP团队能够通过gen_服务器行为为您解决所有这些问题。`gen_server有点像类固醇上的my_server，只是它经过了多年的测试和生产使用。
