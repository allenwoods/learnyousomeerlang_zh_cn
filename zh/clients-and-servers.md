# Clients and Servers

## [Callback to the Future]

![Weird version of Marty from Back to The Future](../img/cbttf.png "brrraaains (in the past)")
![回到未来的马蒂的怪异版本](。。/静态/img/cbttf。png“Brrraains（过去）”）

The first OTP behaviour we'll see is one of the most used ones. Its name is `gen_server` and it has an interface a bit similar to the one we've written with `my_server` in [last chapter](what-is-otp.html#the-basic-server); it gives you a few functions to use it and in exchange, your module has to already have a few functions `gen_server` will use.
我们将看到的第一个OTP行为是最常用的行为之一。它的名字是“gen_server”，它的界面有点类似于我们在[上一章]（什么是otp）中用“my_server”编写的界面。html（基本服务器）；它为您提供了一些使用它的功能，作为交换，您的模块必须已经有了“gen_server”将使用的一些功能。

### init

The first one is an `init/1` function. It is similar to the one we've used with `my_server` in that it is used to initialize the server's state and do all of these one-time tasks that it will depend on. The function can return `` or `ignore`.
第一个是'init/1'函数。它与我们在“my_server”中使用的类似，因为它用于初始化服务器的状态，并执行它将依赖的所有这些一次性任务。函数可以返回``或`忽略`。

The normal ` variable is meant to be added to the tuple whenever you need a deadline before which you expect the server to receive a message. If no message is received before the deadline, a special one (the atom `timeout`) is sent to the server, which should be handled with `handle_info/2` (more on this later.)
normal`变量意味着，每当您需要一个截止日期时，就会将其添加到元组中，您希望服务器在截止日期之前收到消息。如果在截止日期之前没有收到任何消息，则会向服务器发送一条特殊消息（atom'timeout'），该消息应使用'handle_info/2'进行处理（稍后将对此进行详细介绍）。)

On the other hand, if you do expect the process to take a long time before getting a reply and are worried about memory, you can add the `hibernate` atom to the tuple. Hibernation basically reduces the size of the process' state until it gets a message, at the cost of some processing power. If you are in doubt about using hibernation, you probably don't need it.
另一方面，如果您确实认为这个过程需要很长时间才能得到回复，并且担心内存问题，那么可以将“hibernate”原子添加到元组中。休眠基本上减少了进程状态的大小，直到它收到一条消息为止，这是以牺牲一些处理能力为代价的。如果你对使用休眠有疑问，你可能不需要它。

Returning `` should be done when something went wrong during the initialization.

::: note
**Note:** here's a more technical definition of process hibernation. It's no big deal if some readers do not understand or care about it. When the BIF [erlang:hibernate(M,F,A)](http://erldocs.com/18.0/erts/erlang.html#hibernate/3) is called, the call stack for the currently running process is discarded (the function never returns). The garbage collection then kicks in, and what's left is one continuous heap that is shrunken to the size of the data in the process. This basically compacts all the data so the process takes less place.
**注：*这里是对进程休眠的更技术性的定义。如果一些读者不理解或不关心它，那也没什么大不了的。当BIF[erlang:hibernate（M，F，A）](http://erldocs。com/18。0/erts/erlang。调用html#hibernate/3），则丢弃当前正在运行的进程的调用堆栈（函数永远不会返回）。然后垃圾收集开始了，剩下的是一个连续的堆，它会缩小到进程中数据的大小。这基本上压缩了所有的数据，因此过程发生得更少。

Once the process receives a message, the function `M:F` with `A` as arguments is called and the execution resumes.
一旦进程收到一条消息，就会调用以“a”为参数的函数“M:F”，然后继续执行。
:::

::: note
**Note:** while `init/1` is running, execution is blocked in the process that spawned the server. This is because it is waiting for a 'ready' message sent automatically by the `gen_server` module to make sure everything went fine.
**注意：*“init/1”运行时，在生成服务器的进程中执行被阻止。这是因为它正在等待“gen_server”模块自动发送的“ready”消息，以确保一切正常。
:::

### handle_call

The function `handle_call/3` is used to work with synchronous messages (we'll see how to send them soon). It takes 3 arguments: `Request`. It's pretty similar to how we programmed our own `handle_call/3` in `my_server`. The biggest difference is how you reply to messages. In our own abstraction of a server, it was necessary to use `my_server:reply/2` to talk back to the process. In the case of `gen_server`s, there are 8 different return values possible, taking the form of tuples.
函数“handle_call/3”用于处理同步消息（我们将很快了解如何发送它们）。它需要3个参数：`Request`。这与我们在my_server中编程自己的“handle_call/3”非常相似`。最大的区别在于你回复信息的方式。在我们自己对服务器的抽象中，有必要使用'my_server:reply/2'来回话这个过程。在'gen_server'的情况下，可能有8个不同的返回值，以元组的形式。

Because there are many of them, here's a simple list instead:

``` expand








```

For all of these, `Timeout` will be sent back to whoever called the server in the first place. Notice that there are three possible `noreply` options. When you use `noreply`, the generic part of the server will assume you're taking care of sending the reply back yourself. This can be done with `gen_server:reply/2`, which can be used in the same way as `my_server:reply/2`.
对于所有这些，“Timeout”将被发送回最初调用服务器的人。请注意，有三种可能的'noreply'选项。当您使用“noreply”时，服务器的通用部分将假定您自己负责发送回复。这可以通过'gen_server:reply/2'完成，它的使用方式与'my_server:reply/2'相同`。

Most of the time, you'll only need the `reply` tuples. There are still a few valid reasons to use `noreply`: whenever you want another process to send the reply for you or when you want to send an acknowledgement ('hey! I received the message!') but still process it afterwards (without replying this time), etc. If this is what you choose to do, it is absolutely necessary to use `gen_server:reply/2` because otherwise the call will time out and cause a crash.
大多数时候，你只需要回复元组。使用“noreply”仍然有一些合理的理由：当你想让另一个进程为你发送回复时，或者当你想发送确认（“嘿！我收到了消息！”\）但之后仍然处理它（这次没有回复），等等。如果您选择这样做，则绝对有必要使用'gen_server:reply/2'，否则呼叫将超时并导致崩溃。

### handle_cast

The `handle_cast/2` callback works a lot like the one we had in `my_server`: it takes the parameters `Message` and is used to handle asynchronous calls. You do whatever you want in there, in a manner quite similar to what's doable with `handle_call/3`. On the other hand, only tuples without replies are valid return values:
“handle_cast/2”回调的工作原理与“my_server”中的回调非常相似：它接受参数“Message”，用于处理异步调用。你可以在那里做任何你想做的事情，方式非常类似于“处理电话”3`。另一方面，只有没有回复的元组才是有效的返回值：

``` expand




```

### handle_info

You know how I mentioned our own server didn't really deal with messages that do not fit our interface, right? Well `handle_info/2` is the solution. It's very similar to `handle_cast/2` and in fact returns the same tuples. The difference is that this callback is only there for messages that were sent directly with the `!` operator and special ones like `init/1`'s `timeout`, monitors' notifications and `'EXIT'` signals.
你知道我怎么提到我们自己的服务器并没有真正处理不符合我们界面的消息，对吧？“handle_info/2”是解决方案。它与“handle_cast/2”非常相似，实际上返回相同的元组。不同之处在于，此回调仅适用于直接使用“！”发送的消息运算符和特殊的运算符，如'init/1''的'timeout'，监视'通知和'EXIT'信号。

### terminate

The callback `terminate/2` is called whenever one of the three `handle_Something` functions returns a tuple of the form `, corresponding to the same values from the `stop` tuples.
每当三个“handle_Something”函数中的一个返回形式为“的元组”时，就会调用回调“terminate/2”，对应于“stop”元组中的相同值。

`terminate/2` will also be called when its parent (the process that spawned it) dies, if and only if the `gen_server` is trapping exits.
`terminate/2在其父进程（产生它的进程）死亡时也会被调用，当且仅当'gen_server'退出时。

::: note
**Note:** if any reason other than `normal`, `shutdown` or `` is used when `terminate/2` is called, the OTP framework will see this as a failure and start logging a bunch of stuff here and there for you.
**注意：*如果在调用'terminate/2'时使用了除'normal'、'shutdown'或''以外的任何原因，OTP框架会将其视为失败，并开始在这里和那里为您记录大量内容。
:::

This function is pretty much the direct opposite of `init/1` so whatever was done in there should have its opposite in `terminate/2`. It's your server's janitor, the function in charge of locking the door after making sure everyone's gone. Of course, the function is helped by the VM itself, which should usually delete all [ETS tables](http://erldocs.com/18.0/stdlib/ets.html), etc. for you. Note that the return value of this function doesn't really matter, because the code stops executing after it's been called.
这个函数几乎与'init/1'正好相反，所以在那里做的任何事情都应该与'terminate/2'相反`。它是服务器的看门人，负责在确保所有人都离开后锁上门。当然，VM本身也有助于实现该功能，它通常应该删除所有[ETS表](http://erldocs。com/18。0/stdlib/ets。html）等。为你。请注意，这个函数的返回值并不重要，因为代码在被调用后会停止执行。

### code_change

The function `code_change/3` is there to let you upgrade code. It takes the form `code_change(PreviousVersion, State, Extra)`. Here, the variable `PreviousVersion` variable holds all of the current's server state so you can convert it.
“code_change/3”功能可以让你升级代码。它的形式是“代码更改”（以前的版本、状态、额外）`。在这里，变量'PreviousVersion'保存当前服务器的所有状态，以便您可以对其进行转换。

Imagine for a moment that we used an orddict to store all of our data. However, as time goes on, the orddict becomes too slow and we decide to change it for a regular dict. In order to avoid the process crashing on the next function call, the conversion from one data structure to the other can be done in there, safely. All you have to do is return the new state with ``.
想象一下，我们使用orddict存储所有数据。然而，随着时间的推移，命令变得太慢，我们决定将其更改为常规命令。为了避免进程在下一次函数调用时崩溃，可以在其中安全地完成从一个数据结构到另一个数据结构的转换。你所要做的就是返回新状态``。

![a cat with an eye patch](../img/kitty.png)

The `Extra` variable isn't something we'll worry about for now. It's mostly used in larger OTP deployment, where specific tools exist to upgrade entire releases on a VM. We're not there yet.
“额外”变量不是我们现在要担心的问题。它主要用于更大的OTP部署，其中存在特定的工具来升级VM上的整个版本。我们还没到。

So now we've got all the callbacks defined. Don't worry if you're a bit lost: the OTP framework is a bit circular sometimes, where to understand part `A` to be useful. The best way to get over that confusion is to actually implement a gen_server.
现在我们已经定义了所有回调。如果你有点迷茫，不要担心：OTP框架有时有点循环，在哪里理解“a”部分是有用的。克服这种困惑的最佳方法是实际实现gen_服务器。

## [.BEAM me up, Scotty!]

This is going to be the `kitty_gen_server`. It's going to be mostly similar to `kitty_server2`, with only minimal API changes. First start a new module with the following lines in it:
这将是kitty_gen_服务器`。它将与“kitty_server2”基本相似，只需对API进行极小的更改。首先启动一个包含以下行的新模块：

```erl
-module(kitty_gen_server).
-behaviour(gen_server).
```

And try to compile it. You should get something like this:

```eshell
1> c(kitty_gen_server).
./kitty_gen_server.erl:2: Warning: undefined callback function code_change/3 (behaviour 'gen_server')
。/kitty_gen_服务器。erl:2：警告：未定义的回调函数代码\u更改/3（行为'gen\u server'）
./kitty_gen_server.erl:2: Warning: undefined callback function handle_call/3 (behaviour 'gen_server')
。/kitty_gen_服务器。erl:2:警告：未定义的回调函数句柄\调用/3（行为'gen\ u server'）
./kitty_gen_server.erl:2: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
。/kitty_gen_服务器。erl:2:警告：未定义的回调函数句柄\u cast/2（行为'gen\u server'）
./kitty_gen_server.erl:2: Warning: undefined callback function handle_info/2 (behaviour 'gen_server')
。/kitty_gen_服务器。erl:2：警告：未定义的回调函数句柄\u info/2（行为'gen\u server'）
./kitty_gen_server.erl:2: Warning: undefined callback function init/1 (behaviour 'gen_server')
./kitty_gen_server.erl:2: Warning: undefined callback function terminate/2 (behaviour 'gen_server')

```

The compilation worked, but there are warnings about missing callbacks. This is because of the `gen_server` behaviour. A behaviour is basically a way for a module to specify functions it expects another module to have. The behaviour is the contract sealing the deal between the well-behaved generic part of the code and the specific, error-prone part of the code (yours).
编译成功了，但也有关于缺少回调的警告。这是因为“gen_server”的行为。行为基本上是一个模块指定它期望另一个模块具有的功能的一种方式。行为是一种契约，将行为良好的代码通用部分与代码中特定的、易出错的部分（您的部分）之间的交易密封起来。

::: note
**Note:** both 'behavior' and 'behaviour' are accepted by the Erlang compiler.
:::

Defining your own behaviours is really simple. You just need to export a function called `behaviour_info/1` implemented as follows:
定义自己的行为非常简单。您只需要导出一个名为“Behavior_info/1”的函数，其实现如下：

```erl
-module(my_behaviour).
-export([behaviour_info/1]).

%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [];
behaviour_info(_) -> undefined.
```

And that's about it for behaviours. You can just use `-behaviour(my_behaviour).` in a module implementing them to get compiler warnings if you forgot a function. Anyway, back to our third kitty server.
这就是行为学。你可以直接使用`-behavior（我的行为）。`在一个实现它们的模块中，如果你忘记了一个函数，就会得到编译器警告。不管怎样，回到我们的第三台kitty服务器。

The first function we had was `start_link/0`. This one can be changed to the following:

```erl
start_link() -> gen_server:start_link(?MODULE, [], []).
```

The first parameter is the callback module, the second one is the list of parameters to pass to `init/1` and the third one is about debugging options that won't be covered right now. You could add a [fourth parameter](http://erldocs.com/18.0/stdlib/gen_server.html#start_link/4)`.
第一个参数是回调模块，第二个参数是要传递给'init/1'的参数列表，第三个参数是关于调试选项的，现在不介绍这些选项。您可以添加[第四个参数](http://erldocs。com/18。0/stdlib/gen_服务器。html#开始_链接/4）`。

Next functions now:

```erl
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
   gen_server:call(Pid, ).

%% This call is asynchronous
return_cat(Pid, Cat = #cat) ->
    gen_server:cast(Pid, ).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).
```

All of these calls are a one-to-one change. Note that a third parameter can be passed to `gen_server:call/2-3` to give a timeout. If you don't give a timeout to the function (or the atom `infinity`), the default is set to 5 seconds. If no reply is received before time is up, the call crashes.
所有这些电话都是一对一的变化。请注意，第三个参数可以传递给'gen_server:call/2-3'以给出超时。如果没有给函数（或原子“无穷大”）一个超时，默认设置为5秒。如果在时间结束前没有收到回复，电话就会崩溃。

Now we'll be able to add the gen_server callbacks. The following table shows the relationship we have between calls and callbacks:
现在我们可以添加gen_服务器回调。下表显示了调用和回调之间的关系：

  gen_server         YourModule
  ------------------ -----------------
  `start/3-4`        `init/1`
  `start_link/3-4`   `init/1`
  `call/2-3`         `handle_call/3`
  `cast/2`           `handle_cast/2`

And then you have the other callbacks, those that are more about special cases:

-   `handle_info/2`
-   `terminate/2`
-   `code_change/3`

Let's begin by changing those we already have to fit the model: `init/1`, `handle_call/3` and `handle_cast/2`.
让我们先改变那些我们已经需要适应的模型：'init/1'、'handle_call/3'和'handle_cast/2'`。

```erl
%%% Server functions
init([]) -> . %% no treatment of info here!

handle_call(, _From, Cats) ->
    if Cats =:= [] ->
        ;
       Cats =/= [] ->
        
    end;
handle_call(terminate, _From, Cats) ->
    .

handle_cast(, Cats) ->
    .
```

Again, very little has changed there. In fact, the code is now shorter, thanks to smarter abstractions. Now we get to the new callbacks. The first one is `handle_info/2`. Given this is a toy module and we have no logging system pre-defined, just outputting the unexpected messages will be enough:
同样，那里几乎没有什么变化。事实上，由于更智能的抽象，代码现在更短了。现在我们来看看新的回调。第一个是“handle_info/2”`。鉴于这是一个玩具模块，我们没有预先定义的日志系统，只输出意外消息就足够了：

```erl
handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    .
```

The next one is the `terminate/2` callback. It will be very similar to the `terminate/1` private function we had:
下一个是'terminate/2'回调。它将非常类似于我们的“终止/1”私有功能：

```erl
terminate(normal, Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.
```

And then the last callback, `code_change/3`:

```erl
code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    . 
```

Just remember to keep in the `make_cat/3` private function:

```erl
%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat.
```

And we can now try the brand new code:

```eshell
1> c(kitty_gen_server).

2> rr(kitty_gen_server).
[cat]
3>  = kitty_gen_server:start_link().

4> Pid ! <<"Test handle_info">>.
Unexpected message: <<"Test handle_info">>
<<"Test handle_info">>
5> Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat").
#cat{name = "Cat Stevens",color = white,
     description = "not actually a cat"}
6> kitty_gen_server:return_cat(Pid, Cat).
ok
7> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
#cat{name = "Cat Stevens",color = white,
     description = "not actually a cat"}
8> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
#cat{name = "Kitten Mittens",color = black,
     description = "look at them little paws!"}
9> kitty_gen_server:return_cat(Pid, Cat).
ok       
10> kitty_gen_server:close_shop(Pid).
"Cat Stevens" was set free.
ok
```

![pair of wool mittens](../img/mittens.png)

Oh and hot damn, it works!

So what can we say about this generic adventure? Probably the same generic stuff as before: separating the generic from the specific is a great idea on every point. Maintenance is simpler, complexity is reduced, the code is safer, easier to test and less prone to bugs. If there are bugs, they are easier to fix. Generic servers are only one of the many available abstractions, but they're certainly one of the most used ones. We'll see more of these abstractions and behaviours in the next chapters.
那么，对于这场普通的冒险，我们能说些什么呢？可能和以前一样：在每一点上，将泛型和具体的分离都是一个好主意。维护更简单，复杂性更低，代码更安全，更容易测试，更不容易出现错误。如果有漏洞，它们更容易修复。通用服务器只是众多可用抽象中的一个，但它们肯定是最常用的抽象之一。我们将在接下来的章节中看到更多这些抽象和行为。
