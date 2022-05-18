# Errors and Processes

## [Links]

A link is a specific kind of relationship that can be created between two processes. When that relationship is set up and one of the processes dies from an unexpected throw, error or exit (see [Errors and Exceptions](errors-and-exceptions.html)), the other linked process also dies.
链接是可以在两个进程之间创建的一种特定关系。当建立了这种关系，其中一个进程因意外抛出、错误或退出而死亡时（请参阅[Errors and Exceptions]（错误和异常）。html），另一个链接进程也会死亡。

This is a useful concept from the perspective of failing as soon as possible to stop errors: if the process that has an error crashes but those that depend on it don't, then all these depending processes now have to deal with a dependency disappearing. Letting them die and then restarting the whole group is usually an acceptable alternative. Links let us do exactly this.
从尽快停止错误的角度来看，这是一个有用的概念：如果有错误的进程崩溃，但依赖它的进程没有崩溃，那么所有这些依赖进程现在都必须处理依赖关系消失的问题。让他们死去，然后重新开始整个团队通常是可以接受的选择。链接让我们做到这一点。

To set a link between two processes, Erlang has the primitive function [link/1](http://erldocs.com/18.0/erts/erlang.html#link/1 "Not the Zelda kind of link"):
为了在两个进程之间设置链接，Erlang有一个基本函数[link/1](http://erldocs。com/18。0/erts/erlang。html#link/1“不是塞尔达那种链接”）：

```erl
myproc() ->
    timer:sleep(5000),
    exit(reason).
```

If you try the next following calls (and wait 5 seconds between each spawn command), you should see the shell crashing for 'reason' only when a link has been set between the two processes.
如果您尝试接下来的调用（并在每个spawn命令之间等待5秒），您应该会看到shell仅在两个进程之间设置了链接时才会因“原因”而崩溃。

```eshell
1> c(linkmon).

2> spawn(fun linkmon:myproc/0).
<0.52.0>
3> link(spawn(fun linkmon:myproc/0)).
true
** exception error: reason
```

Or, to put it in picture:

![A process receiving an exit signal](../img/link-exit.png)

However, this `` message can not be caught with a `try ... catch` as usual. Other mechanisms need to be used to do this. We'll see them later.
然而，这条“消息”不能被“尝试”捕捉到。。。一如往常。需要使用其他机制来实现这一点。我们待会儿见。

It's important to note that links are used to establish larger groups of processes that should all die together:
需要注意的是，链接用于建立更大的流程组，这些流程应该一起消亡：

```erl
chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.
```

This function will take an integer `N` argument to the next 'chain' process (which calls `spawn/1`), I wrap the call inside an anonymous function so it doesn't need arguments anymore. Calling `spawn(?MODULE, chain, [N-1])` would have done a similar job.
这个函数将把一个整数'N'参数带到下一个'chain'进程（调用'spawn/1'），我将调用包装在一个匿名函数中，这样它就不再需要参数了。调用'spawn（？MODULE，chain，[N-1]）也可以完成类似的工作。

Here, I'll have many processes linked together, dying as each of their successors exits:

```eshell
4> c(linkmon).               

5> link(spawn(linkmon, chain, [3])).
true
** exception error: "chain dies here"
```

And as you can see, the shell does receive the death signal from some other process. Here's a drawn representation of the spawned processes and links going down:
正如你所见，外壳确实接收到来自其他过程的死亡信号。下面是生成的进程和链接的绘制表示：

``` expand
[shell] == [3] == [2] == [1] == [0]
[shell] == [3] == [2] == [1] == *dead*
[shell] == [3] == [2] == *dead*
[shell] == [3] == *dead*
[shell] == *dead*
*dead, error message shown*
[shell] <-- restarted
```

After the process running `linkmon:chain(0)` dies, the error is propagated down the chain of links until the shell process itself dies because of it. The crash could have happened in any of the linked processes; because links are bidirectional, you only need one of them to die for the others to follow suit.
在运行'linkmon:chain（0）`的进程死亡后，错误会沿着链接链向下传播，直到shell进程本身因此死亡。崩溃可能发生在任何一个相关过程中；因为链接是双向的，所以你只需要其中一个链接就可以让其他链接效仿。

::: note
**Note:** If you wanted to kill another process from the shell, you could use the function [exit/2](http://erldocs.com/18.0/erts/erlang.html#exit/2 "Erlang's handgun"), which is called this way: `exit(Pid, Reason)`. Try it if you wish.
**注意：*如果您想从shell中终止另一个进程，可以使用函数[exit/2](http://erldocs。com/18。0/erts/erlang。html#exit/2“Erlang的手枪”），它的名字是这样的：`exit（Pid，Reason）`。如果你愿意，试试看。
:::

::: note
**Note:** Links can not be stacked. If you call `link/1` 15 times for the same two processes, only one link will still exist between them and a single call to `unlink/1` will be enough to tear it down.
**注意：*链接不能堆叠。如果对同两个进程调用'link/1'15次，它们之间只会存在一个链接，一次调用'unlink/1'就足以将其删除。
:::

Its important to note that `link(spawn(Function))` or `link(spawn(M,F,A))` happens in more than one step. In some cases, it is possible for a process to die before the link has been set up and then provoke unexpected behavior. For this reason, the function [spawn_link/1-3](http://erldocs.com/18.0/erts/erlang.html#spawn_link/1 "Not the Spawn from comic books (or Zelda)") has been added to the language. It takes the same arguments as `spawn/1-3`, creates a process and links it as if `link/1` had been there, except it's all done as an atomic operation (the operations are combined as a single one, which can either fail or succeed, but nothing else). This is generally considered safer and you save a set of parentheses too.
需要注意的是，`link（spawn（Function））`或`link（spawn（M，F，A））`发生在多个步骤中。在某些情况下，一个进程可能会在链接建立之前终止，然后引发意外行为。因此，函数[spawn_link/1-3](http://erldocs。com/18。0/erts/erlang。html#spawn_link/1“不是漫画中的spawn（或Zelda）”）已添加到该语言中。它采用与“spawn/1-3”相同的参数，创建一个进程并将其链接，就像“link/1”已经存在一样，只是它都是作为一个原子操作完成的（这些操作组合为一个单独的操作，可以失败，也可以成功，但除此之外没有其他操作）。这通常被认为是更安全的，而且还可以保存一组括号。

![Admiral Ackbar](../img/ackbar.jpg "It's a trap! (that took forever to trace)")

## [It's a Trap!]

Now to get back to links and processes dying. Error propagation across processes is done through a process similar to message passing, but with a special type of message called signals. Exit signals are 'secret' messages that automatically act on processes, killing them in the action.
现在回到链接和流程。错误在进程间的传播是通过一个类似于消息传递的进程完成的，但有一种称为信号的特殊类型的消息。退出信号是自动作用于进程的“秘密”消息，在行动中杀死它们。

I have mentioned many times already that in order to be reliable, an application needs to be able to both kill and restart a process quickly. Right now, links are alright to do the killing part. What's missing is the restarting.
我已经多次提到，为了可靠，应用程序需要能够快速终止和重新启动进程。现在，链接可以用来做杀戮部分。缺少的是重新启动。

In order to restart a process, we need a way to first know that it died. This can be done by adding a layer on top of links (the delicious frosting on the cake) with a concept called *system processes*. System processes are basically normal processes, except they can convert exit signals to regular messages. This is done by calling `process_flag(trap_exit, true)` in a running process. Nothing speaks as much as an example, so we'll go with that. I'll just redo the chain example with a system process at the beginning:
为了重新启动一个进程，我们需要一种方法，首先知道它已经死了。这可以通过在链接（蛋糕上美味的糖霜）上添加一层来实现，这个概念叫做*系统进程*。系统进程基本上是正常进程，只是它们可以将退出信号转换为常规消息。这是通过在正在运行的进程中调用“process_flag（trap_exit，true）”来实现的。没有什么比一个例子更能说明问题了，所以我们还是举个例子吧。我将在开始时用一个系统过程重做链示例：

```eshell
1> process_flag(trap_exit, true).
true
2> spawn_link(fun() -> linkmon:chain(3) end).
<0.49.0>
3> receive X -> X end.

```

Ah! Now things get interesting. To get back to our drawings, what happens is now more like this:

``` expand
[shell] == [3] == [2] == [1] == [0]
[shell] == [3] == [2] == [1] == *dead*
[shell] == [3] == [2] == *dead*
[shell] == [3] == *dead*
[shell] <--  -- *dead*
[shell] <-- still alive!
```

And this is the mechanism allowing for a quick restart of processes. By writing programs using system processes, it is easy to create a process whose only role is to check if something dies and then restart it whenever it fails. We'll cover more of this in the next chapter, when we really apply these techniques.
这是一种允许快速重启进程的机制。通过使用系统进程编写程序，很容易创建一个进程，该进程的唯一作用是检查某个进程是否死亡，然后在失败时重新启动它。在下一章中，当我们真正应用这些技术时，我们将介绍更多内容。

For now, I want to come back to the exception functions seen in the [exceptions chapter](errors-and-exceptions.html) and show how they behave around processes that trap exits. Let's first set the bases to experiment without a system process. I'll successively show the results of uncaught throws, errors and exits in neighboring processes:
现在，我想回到[exceptions]一章（错误和异常）中看到的异常函数。html），并展示它们在陷阱出口的进程中的行为。首先，让我们为没有系统流程的实验奠定基础。我将依次显示相邻进程中未捕获的抛出、错误和退出的结果：

Exception source: `spawn_link(fun() -> ok end)`
:   **Untrapped Result**: - nothing -
:   **Trapped Result**: `
:   The process exited normally, without a problem. Note that this looks a bit like the result of `catch exit(normal)`, except a PID is added to the tuple to know what processed failed.
：进程正常退出，没有问题。请注意，这看起来有点像“catch exit（normal）”的结果，只是在元组中添加了一个PID，以了解处理失败的内容。

Exception source: `spawn_link(fun() -> exit(reason) end)`
:   **Untrapped Result**: `** exception exit: reason`
:   **Trapped Result**: `
:   The process has terminated for a custom reason. In this case, if there is no trapped exit, the process crashes. Otherwise, you get the above message.
：进程因自定义原因终止。在这种情况下，如果没有被困的出口，进程就会崩溃。否则，您将收到上述消息。

Exception source: `spawn_link(fun() -> exit(normal) end)`
:   **Untrapped Result**: - nothing -
:   **Trapped Result**: `
:   This successfully emulates a process terminating normally. In some cases, you might want to kill a process as part of the normal flow of a program, without anything exceptional going on. This is the way to do it.
：这成功模拟了正常终止的进程。在某些情况下，您可能希望在不发生任何异常的情况下，将进程作为程序正常流的一部分终止。这就是方法。

Exception source: `spawn_link(fun() -> 1/0 end)`
:   **Untrapped Result**: `Error in process <0.44.0> with exit value: 
:   **Trapped Result**: `
:   The error (`. At this point, it behaves exactly the same as `exit(reason)` did, but with a stack trace giving more details about what happened.
：错误(`。此时，它的行为与'exit（reason）'的行为完全相同，但有一个堆栈跟踪，提供了关于发生了什么的更多细节。

Exception source: `spawn_link(fun() -> erlang:error(reason) end)`
:   **Untrapped Result**: `Error in process <0.47.0> with exit value: 
:   **Trapped Result**: `
:   Pretty much the same as with `1/0`. That's normal, `erlang:error/1` is meant to allow you to do just that.
：与'1/0'几乎相同`。这很正常，`erlang:error/1`的意思就是允许你这么做。

Exception source: `spawn_link(fun() -> throw(rocks) end)`
:   **Untrapped Result**: `Error in process <0.51.0> with exit value: 
:   **Trapped Result**: `
:   Because the `throw` is never caught by a `try ... catch`, it bubbles up into an error, which in turn bubbles up into an `EXIT`. Without trapping exit, the process fails. Otherwise it deals with it fine.
：因为“扔”永远不会被“试”抓住。。。catch`，它冒泡成一个错误，然后冒泡成一个出口`。如果没有陷阱退出，该过程就会失败。否则它会处理得很好。

And that's about it for usual exceptions. Things are normal: everything goes fine. Exceptional stuff happens: processes die, different signals are sent around.
这就是通常的例外情况。一切正常：一切顺利。例外的事情发生了：进程死亡，不同的信号被发送。

Then there's `exit/2`. This one is the Erlang process equivalent of a gun. It allows a process to kill another one from a distance, safely. Here are some of the possible calls:
然后是“出口/2”`。这个过程相当于一把枪。它允许一个进程从远处安全地杀死另一个进程。以下是一些可能的电话：

Exception source: `exit(self(), normal)`
:   **Untrapped Result**: `** exception exit: normal`
:   **Trapped Result**: `
:   When not trapping exits, `exit(self(), normal)` acts the same as `exit(normal)`. Otherwise, you receive a message with the same format you would have had by listening to links from foreign processes dying.
：当不捕获出口时，`exit（self（），normal）`的行为与`exit（normal）相同`。否则，您将收到一条格式与收听来自国外进程的链接时相同的消息。

Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), normal)`
:   **Untrapped Result**: - nothing -
:   **Trapped Result**: - nothing -
:   This basically is a call to `exit(Pid, normal)`. This command doesn't do anything useful, because a process can not be remotely killed with the reason `normal` as an argument.
：呼叫退出（基本上是正常的）`。这个命令没有任何用处，因为不能以“normal”作为参数远程终止进程。

Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), reason)`
:   **Untrapped Result**: `** exception exit: reason`
:   **Trapped Result**: `
:   This is the foreign process terminating for `reason` itself. Looks the same as if the foreign process called `exit(reason)` on itself.
：这是由于'reason'本身而终止的外部进程。看起来就像外部进程本身被称为“退出（原因）”一样。

Exception source: `exit(spawn_link(fun() -> timer:sleep(50000) end), kill)`
:   **Untrapped Result**: `** exception exit: killed`
:   **Trapped Result**: `
:   Surprisingly, the message gets changed from the dying process to the spawner. The spawner now receives `killed` instead of `kill`. That's because `kill` is a special exit signal. More details on this later.
：令人惊讶的是，消息从死亡过程变为产卵过程。产卵者现在收到的是'kill'而不是'kill'`。这是因为“kill”是一个特殊的出口信号。稍后会有更多详细信息。

Exception source: `exit(self(), kill)`
:   **Untrapped Result**: `** exception exit: killed`
:   **Trapped Result**: `** exception exit: killed`
:   Oops, look at that. It seems like this one is actually impossible to trap. Let's check something.
：哎呀，看看那个。看起来这一个实际上是不可能陷阱。让我们检查一下。

Exception source: `spawn_link(fun() -> exit(kill) end)`
:   **Untrapped Result**: `** exception exit: killed`
:   **Trapped Result**: `
:   Now that's getting confusing. When another process kills itself with `exit(kill)` and we don't trap exits, our own process dies with the reason `killed`. However, when we trap exits, things don't happen that way.
：现在这让人困惑。当另一个进程以'exit（kill）'结束自己，而我们没有捕获出口时，我们自己的进程以'kill'结束`。然而，当我们设置出口陷阱时，事情并不是这样发生的。

While you can trap most exit reasons, there are situations where you might want to brutally murder a process: maybe one of them is trapping exits but is also stuck in an infinite loop, never reading any message. The `kill` reason acts as a special signal that can't be trapped. This ensures any process you terminate with it will really be dead. Usually, `kill` is a bit of a last resort, when everything else has failed.
虽然您可以捕获大多数退出原因，但在某些情况下，您可能想要残忍地谋杀一个进程：可能其中一个原因正在捕获退出，但也被困在一个无限循环中，从未读取任何消息。“杀戮”原因是一种特殊的信号，不能被捕获。这确保了你终止的任何进程都会真的死掉。通常，当其他一切都失败时，“kill”是最后的手段。

![A mouse trap with a beige laptop on top](../img/trap.png "You know you want that beige laptop")
![顶部有米色笔记本电脑的捕鼠器](。。/静态/图像/陷阱。png“你知道你想要那台米色的笔记本电脑”）

As the `kill` reason can never be trapped, it needs to be changed to `killed` when other processes receive the message. If it weren't changed in that manner, every other process linked to it would in turn die for the same `kill` reason and would in turn kill its neighbors, and so on. A death cascade would ensue.
由于“kill”原因永远不会被捕获，因此当其他进程收到消息时，需要将其更改为“kill”。如果不以这种方式更改它，与它相关的所有其他进程都会因同样的“杀死”原因而死亡，并反过来杀死它的邻居，以此类推。随之而来的是死亡瀑布。

This also explains why `exit(kill)` looks like `killed` when received from another linked process (the signal is modified so it doesn't cascade), but still looks like `kill` when trapped locally.
这也解释了为什么从另一个链接进程接收到的“exit（kill）”看起来像“kill”（信号经过修改，因此不会级联），但在本地被捕获时仍然像“kill”。

If you find this all confusing, don't worry. Many programmers feel the same. Exit signals are a bit of a funny beast. Luckily there aren't many more special cases than the ones described above. Once you understand those, you can understand most of Erlang's concurrent error management without a problem.
如果你觉得这一切都令人困惑，别担心。许多程序员也有同样的感受。出口信号有点滑稽。幸运的是，没有比上述情况更多的特殊情况。一旦理解了这些，就可以毫无问题地理解Erlang的大部分并发错误管理。

## [Monitors]

So yeah. Maybe murdering processes isn't what you want. Maybe you don't feel like taking the world down with you once you're gone. Maybe you're more of a stalker. In that case, monitors might be what you want.
所以是的。也许谋杀程序不是你想要的。也许一旦你离开了，你就不想让世界跟着你一起倒下。也许你更像一个跟踪者。在这种情况下，监视器可能就是你想要的。

More seriously, monitors are a special type of link with two differences:

-   they are unidirectional;
-   they can be stacked.

![Ugly Homer Simpson parody](../img/homer.png "Not an example of a good monitor")

Monitors are what you want when a process wants to know what's going on with a second process, but neither of them really are vital to each other.
两位监护者都不想知道对方在做什么，但你们都不想知道对方在做什么。

Another reason, as listed above, is stacking the references. Now this might seem useless from a quick look, but it is great for writing libraries which need to know what's going on with other processes.
如上所述，另一个原因是堆叠引用。现在，从快速查看来看，这似乎没什么用，但对于编写需要了解其他进程的库来说，这是非常好的。

You see, links are more of an organizational construct. When you design the architecture of your application, you determine which process will do which jobs, and what will depend on what. Some processes will supervise others, some couldn't live without a twin process, etc. This structure is usually something fixed, known in advance. Links are useful for that and should not necessarily be used outside of it.
你看，链接更像是一种组织结构。在设计应用程序的体系结构时，您要确定哪个进程将完成哪些工作，以及什么将取决于什么。有些过程会监督其他过程，有些过程离不开双过程，等等。这种结构通常是固定的，事先知道的。链接在这方面很有用，不一定要在它之外使用。

But what happens if you have 2 or 3 different libraries that you call and they all need to know whether a process is alive or not? If you were to use links for this, you would quickly hit a problem whenever you needed to unlink a process. Now, links aren't stackable, so the moment you unlink one, you unlink them all and mess up all the assumptions put up by the other libraries. That's pretty bad. So you need stackable links, and monitors are your solution. They can be removed individually. Plus, being unidirectional is handy in libraries because other processes shouldn't have to be aware of said libraries.
但是，如果你调用了2到3个不同的库，它们都需要知道一个进程是否处于活动状态，会发生什么呢？如果要使用链接来实现这一点，那么无论何时需要取消流程链接，都会很快遇到问题。现在，链接是不可堆叠的，所以当你断开一个链接的时候，你就断开了所有链接，并打乱了其他库提出的所有假设。那太糟糕了。所以你需要可堆叠的链接，监视器是你的解决方案。它们可以单独移除。此外，单向性在库中很方便，因为其他流程不必知道所说的库。

So what does a monitor look like? Easy enough, let's set one up. The function is [erlang:monitor/2](http://erldocs.com/18.0/erts/erlang.html#monitor/2 "I seeee youuu") and the second one is the pid:
那么显示器是什么样子的呢？很简单，让我们设置一个。函数是[erlang:monitor/2](http://erldocs。com/18。0/erts/erlang。html#monitor/2“I seeee youuu”），第二个是pid：

```eshell
1> erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
#Ref<0.0.0.77>
2> flush().
Shell got 
ok
```

Every time a process you monitor goes down, you will receive such a message. The message is `:

```eshell
3>  = spawn_monitor(fun() -> receive _ -> exit(boom) end end).

4> erlang:demonitor(Ref).
true
5> Pid ! die.
die
6> flush().
ok
```

In this case, we demonitored the other process before it crashed and as such we had no trace of it dying. The function [demonitor/2](http://erldocs.com/18.0/erts/erlang.html#demonitor/2 "I can't seeee youuuuuu") also exists and gives a little bit more information. The second parameter can be a list of options. Only two exist, `info` and `flush`:
在本例中，我们在另一个进程崩溃之前对其进行了监控，因此我们没有它死亡的痕迹。功能[demonitor/2](http://erldocs。com/18。0/erts/erlang。html#demonitor/2“我看不见youuuuuu”）也存在，并提供了更多信息。第二个参数可以是选项列表。只有两个存在，'info'和'flush'：

```eshell
7> f().
ok
8>  = spawn_monitor(fun() -> receive _ -> exit(boom) end end). 

9> Pid ! die.
die
10> erlang:demonitor(Ref, [flush, info]).
false
11> flush().
ok
```

The `info` option tells you if a monitor existed or not when you tried to remove it. This is why the expression 10 returned `false`. Using `flush` as an option will remove the `DOWN` message from the mailbox if it existed, resulting in `flush()` finding nothing in the current process' mailbox.
“info”选项会告诉您，当您试图删除监视器时，它是否存在。这就是表达式10返回“false”的原因`。使用'flush'作为选项将从邮箱中删除'DOWN'消息（如果存在），从而导致'flush（）'在当前进程'邮箱中找不到任何内容。

## [Naming Processes]

With links and monitors understood, there is another problem still left to be solved. Let's use the following functions of the [linkmon.erl](static/erlang/linkmon.erl.html) module:
了解了链接和监视器后，还有另一个问题有待解决。让我们使用[linkmon]的以下函数。erl]（静态/erlang/linkmon）。呃。html）模块：

```erl
start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! ,
    receive
         -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
         ->
            From ! ;
         ->
            From ! ;
         ->
            From ! ;
         ->
            From ! 
    end,
    critic().
```

Now we'll just pretend we're going around stores, shopping for music. There are a few albums that sound interesting, but we're never quite sure. You decide to call your friend, the critic.
现在我们就假装在逛商店，买音乐。有几张专辑听起来很有趣，但我们从来都不太确定。你决定打电话给你的朋友，评论家。

```eshell
1> c(linkmon).                         

2> Critic = linkmon:start_critic().
<0.47.0>
3> linkmon:judge(Critic, "Genesis", "The Lambda Lies Down on Broadway").
"They are terrible!"
```

Because of a solar storm (I'm trying to find something realistic here), the connection is dropped:

```eshell
4> exit(Critic, solar_storm).
true
5> linkmon:judge(Critic, "Genesis", "A trick of the Tail Recursion").
timeout
```

Annoying. We can no longer get criticism for the albums. To keep the critic alive, we'll write a basic 'supervisor' process whose only role is to restart it when it goes down:
烦人的。我们再也不能因为这些专辑而受到批评了。为了让批评者活着，我们将编写一个基本的“监督者”流程，其唯一作用是在它停止时重新启动：

```erl
start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    receive
         -> % not a crash
            ok;
         -> % manual termination, not a crash
            ok;
         ->
            restarter()
    end.
```

Here, the restarter will be its own process. It will in turn start the critic's process and if it ever dies of abnormal cause, `restarter/0` will loop and create a new critic. Note that I added a clause for `` as a way to manually kill the critic if we ever need to.
在这里，重启者将有自己的流程。它将依次启动批评家的进程，如果它因异常原因死亡，'restarter/0'将循环并创建一个新的批评家。请注意，我为``添加了一个子句，以便在需要时手动杀死批评者。

The problem with our approach is that there is no way to find the Pid of the critic, and thus we can't call him to have his opinion. One of the solutions Erlang has to solve this is to give names to processes.
我们的方法的问题是，没有办法找到批评家的Pid，因此我们不能打电话给他征求他的意见。Erlang必须解决的解决方案之一是为进程命名。

The act of giving a name to a process allows you to replace the unpredictable pid by an atom. This atom can then be used exactly as a Pid when sending messages. To give a process a name, the function [erlang:register/2](http://erldocs.com/18.0/erts/erlang.html#register/2 "here be a title. Enjoy") or a more detailed one with the shell command `regs()`. Here we can rewrite the `restarter/0` function as follows:
给一个进程命名的行为允许你用一个原子来代替不可预测的pid。然后，在发送消息时，这个原子可以完全用作Pid。为了给进程命名，函数[erlang:register/2](http://erldocs。com/18。0/erts/erlang。html#register/2“这里是一个标题。或者使用shell命令'regs（）`。在这里，我们可以按如下方式重写'restarter/0'函数：

```erl
restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
         -> % not a crash
            ok;
         -> % manual termination, not a crash
            ok;
         ->
            restarter()
    end. 
```

So as you can see, `register/2` will always give our critic the name 'critic', no matter what the Pid is. What we need to do is then remove the need to pass in a Pid from the abstraction functions. Let's try this one:
正如你所看到的，'register/2'总是给我们的批评者起一个名为'critic'，不管Pid是什么。然后，我们需要做的是消除从抽象函数中传入Pid的需要。让我们试试这个：

```erl
judge2(Band, Album) ->
    critic ! ,
    Pid = whereis(critic),
    receive
         -> Criticism
    after 2000 ->
        timeout
    end.
```

Here, the line `Pid = whereis(critic)` is used to find the critic's process identifier in order to pattern match against it in the `receive` expression. We want to match with this pid, because it makes sure we will match on the right message (there could be 500 of them in the mailbox as we speak!) This can be the source of a problem though. The code above assumes that the critic's pid will remain the same between the first two lines of the function. However, it is completely plausible the following will happen:
根据表达式中的'Pid'来匹配'it'receive'。我们希望与此pid匹配，因为它确保我们将匹配正确的消息（我们说话时邮箱中可能有500条消息！）但这可能是问题的根源。上面的代码假设批评家的pid在函数的前两行之间保持不变。然而，以下情况完全有可能发生：

``` expand
  1. critic ! Message
                        2. critic receives
                        3. critic replies
                        4. critic dies
  5. whereis fails
                        6. critic is restarted
  7. code crashes
```

Or yet, this is also a possibility:

``` expand
  1. critic ! Message
                           2. critic receives
                           3. critic replies
                           4. critic dies
                           5. critic is restarted
  6. whereis picks up
     wrong pid
  7. message never matches
```

The possibility that things go wrong in a different process can make another one go wrong if we don't do things right. In this case, the value of the `critic` can be accessed *and* modified by different processes at virtually the same time, resulting in inconsistent information and software errors. The common term for such things is a *race condition*. Race conditions are particularly dangerous because they depend on the timing of events. In pretty much every concurrent and parallel language out there, this timing depends on unpredictable factors such as how busy the processor is, where the processes go, and what data is being processed by your program.
如果我们做得不对，在不同的过程中出错的可能性可能会导致另一个过程出错。在这种情况下，“critic”的值几乎可以由不同的进程同时访问*和*修改，从而导致信息不一致和软件错误。这类事情的通用术语是“种族条件”*。比赛条件尤其危险，因为它们取决于比赛的时间安排。在几乎所有并发和并行语言中，这个时间取决于不可预测的因素，比如处理器有多忙，进程去哪里，以及程序正在处理什么数据。

::: 
**Don't drink too much kool-aid:**\
You might have heard that Erlang is usually free of race conditions or deadlocks and makes parallel code safe. This is true in many circumstances, but never assume your code is really that safe. Named processes are only one example of the multiple ways in which parallel code can go wrong.
您可能听说过Erlang通常没有竞争条件或死锁，并且使并行代码安全。这在很多情况下都是正确的，但千万不要假设你的代码真的那么安全。命名进程只是并行代码出错的多种方式之一。

Other examples include access to files on the computer (to modify them), updating the same database records from many different processes, etc.
其他示例包括访问计算机上的文件（修改它们）、从许多不同的进程更新相同的数据库记录等。
:::

Luckily for us, it's relatively easy to fix the code above if we don't assume the named process remains the same. Instead, we'll use references (created with `make_ref()`) as unique values to identify messages. We'll need to rewrite the `critic/0` function into `critic2/0` and `judge/3` into `judge2/2`:
幸运的是，如果我们不假设指定的进程保持不变，那么修复上面的代码相对容易。相反，我们将使用引用（用make_ref（）创建）作为唯一值来标识消息。我们需要将“critic/0”函数重写为“critic2/0”，将“judge/3”重写为“judge2/2”：

```erl
judge2(Band, Album) ->
    Ref = make_ref(),
    critic ! ,
    receive
         -> Criticism
    after 2000 ->
        timeout
    end.

critic2() ->
    receive
         ->
            From ! ;
         ->
            From ! ;
         ->
            From ! ;
         ->
            From ! 
    end,
    critic2().
```

And then change `restarter/0` to fit by making it spawn `critic2/0` rather than `critic/0`. Now the other functions should keep working fine. The user won't see a difference. Well, they will because we renamed functions and changed the number of parameters, but they won't know what implementation details were changed and why it was important. All they'll see is that their code got simpler and they no longer need to send a pid around function calls:
然后通过生成“critic2/0”而不是“critic/0”，将“restarter/0”更改为适合`。现在其他功能应该可以正常工作了。用户看不出有什么不同。他们会的，因为我们重命名了函数并更改了参数的数量，但他们不知道更改了哪些实现细节以及为什么它很重要。他们将看到，他们的代码变得更简单，不再需要在函数调用周围发送pid：

```eshell
6> c(linkmon).

7> linkmon:start_critic2().
<0.55.0>
8> linkmon:judge2("The Doors", "Light my Firewall").
"They are terrible!"
9> exit(whereis(critic), kill).
true
10> linkmon:judge2("Rage Against the Turing Machine", "Unit Testify").     
"They are great!"
```

And now, even though we killed the critic, a new one instantly came back to solve our problems. That's the usefulness of named processes. Had you tried to call `linkmon:judge/2` without a registered process, a `bad argument` error would have been thrown by the `!` operator inside the function, making sure that processes that depend on named ones can't run without them.
现在，即使我们杀了那个批评家，一个新的批评家马上回来解决我们的问题。这就是命名过程的用处。如果您试图在没有注册进程的情况下调用'linkmon:judge/2'，则`！会抛出一个'bad argument'错误运算符，确保依赖命名进程的进程在没有命名进程的情况下无法运行。

::: note
**Note:** If you remember earlier texts, atoms can be used in a limited (though high) number. You shouldn't ever create dynamic atoms. This means naming processes should be reserved to important services unique to an instance of the VM and processes that should be there for the whole time your application runs.
**注：**如果你还记得以前的文字，原子的使用数量有限（虽然很高）。你不应该创造动态原子。这意味着命名进程应该保留给VM实例特有的重要服务，以及在应用程序运行的整个过程中都应该存在的进程。

If you need named processes but they are transient or there isn't any of them which can be unique to the VM, it may mean they need to be represented as a group instead. Linking and restarting them together if they crash might be the sane option, rather than trying to use dynamic names.
如果您需要命名的进程，但它们是暂时的，或者没有任何一个进程对VM来说是唯一的，这可能意味着它们需要被表示为一个组。如果它们崩溃，将它们链接并重新启动可能是明智的选择，而不是尝试使用动态名称。
:::

In the next chapter, we'll put the recent knowledge we gained on concurrent programming with Erlang to practice by writing a real application.
在下一章中，我们将通过编写一个真实的应用程序来实践我们在Erlang并发编程方面获得的最新知识。
