# Postscript: Time Goes On

## [On Time for Time]

Time is a very, very tricky thing. In the physical every day world, we at least have one certainty: time moves forward, and generally at a constant rate. If we start looking at fancypants physics (anything where relativity is involved, so not *that* fancypants), then time starts drifting and shifting around. A clock on a plane goes slower than a clock on the ground, and someone nearing a black hole ages at a different speed from someone orbiting the moon.
时间是一件非常非常棘手的事情。在日常的物质世界中，我们至少有一个确定性：时间总是以恒定的速度向前移动。如果我们开始研究fancypants物理学（任何涉及相对论的东西，所以不是那个fancypants），那么时间开始漂移和移动。飞机上的时钟比地面上的时钟走得慢，接近黑洞的人的衰老速度与绕月球运行的人不同。

![a pirate with a hook scared of time and clocks](../img/hook.png)

Unfortunately for programmers and computer people, there is no need for nifty phenomena like that to be involved for time to go weird; clocks on computers are just not that great. They spring forwards, spring backwards, stall or accelerate, get [leap seconds](https://en.wikipedia.org/wiki/Leap_second) will play around with time corrections but may crash at any time.
不幸的是，对于程序员和计算机人来说，没有必要为了时间的怪异而卷入这样漂亮的现象；电脑上的时钟并没有那么好。它们向前跳，向后跳，失速或加速，获得[闰秒](https://en。维基百科。org/wiki/Leap_second）将进行时间修正，但随时可能崩溃。

There is therefore no need to leave the room for computer time to dilate and ruin your understanding of the world. Even on a single computer, it is possible for time to move in frustrating ways. It's just not very reliable.
因此，没有必要为计算机时间留出空间来扩展和破坏你对世界的理解。即使在一台电脑上，时间也有可能以令人沮丧的方式流逝。只是不太可靠。

In the context of Erlang, we care a lot about time. We want low latencies, and we can specify timeouts and delays in milliseconds on almost every operation out there: sockets, message receiving, event scheduling, and so on. We also want fault tolerance and to be able to write reliable systems. The question is how can we make something solid out of such unreliable things? Erlang takes a somewhat unique approach, and since release 18, it has seen some very interesting evolution.
在Erlang的环境中，我们非常关心时间。我们需要低延迟，并且我们可以在几乎所有操作上指定超时和延迟（以毫秒为单位）：套接字、消息接收、事件调度等等。我们还希望容错，并能够编写可靠的系统。问题是，我们怎样才能把这些不可靠的东西变成现实？Erlang采用了一种有点独特的方法，自发布18以来，它经历了一些非常有趣的演变。

## [How Things Were]

Before release 18, Erlang time works in one of two major ways:

1.  The operating system's clock, represented as a tuple of the form `` (`os:timestamp()`)
2.  The virtual machine's clock, represented as a tuple of the form `` (`erlang:now()`, auto-imported as `now()`)
2.。虚拟机的时钟，以``形式的元组表示（`erlang:now（）`，自动导入为`now（）`）

The operating system clock can follow any pattern whatsoever:

![A curve with the x-axis being real time and the y-axis being OS time; the curve goes up and down randomly although generally trending up](../img/clock-os.png)
![x轴为实时，y轴为操作系统时间的曲线；曲线随机上升和下降，但通常呈上升趋势](。。/静态/img/时钟操作系统。（巴布亚新几内亚）

It can move however the OS feels like moving it.

The VM's clock can only move forward, and can never return the same value twice. This is a property named being *strictly monotonic*:
虚拟机的时钟只能向前移动，不能两次返回相同的值。这是一个名为“严格单调”的属性：

![A curve with the x-axis being real time and the y-axis being VM time; the curve goes up steadily though at irregular rates](../img/clock-vm.png)
!【x轴为实时，y轴为虚拟机时间的曲线；曲线以不规则的速率稳定上升】(。。/静态/img/时钟虚拟机。（巴布亚新几内亚）

In order for `now()` to respect such properties, it requires coordinated access by all Erlang processes. Whenever it is called twice in a row at close intervals or while time has gone backwards, the VM will increment microseconds to make sure the same value isn't returned twice. This coordination mechanism (acquiring locks and whatnot) can act as a bottleneck in a busy system.
为了让`now（）`尊重这些属性，它需要所有Erlang进程进行协调访问。每当它在一行中以接近的间隔被调用两次，或者当时间向后时，VM将增加微秒，以确保相同的值不会返回两次。这种协调机制（获取锁等）可能会成为繁忙系统中的瓶颈。

::: note
**Note:** monotonicity comes in two main flavors: strict and non-strict.

Strict monotonic counters or clocks are guaranteed to return always increasing values (or always decreasing values). The sequence `1, 2, 3, 4, 5` is strictly monotonic.
严格的单调计数器或时钟保证返回总是递增的值（或总是递减的值）。序列'1,2,3,4,5'是严格单调的。

Regular (non-strict) monotonic counters otherwise only require to return non-decreasing values (or non-increasing values). The sequence `1, 2, 2, 2, 3, 4` is monotonic, but not strictly monotonic.
常规（非严格）单调计数器，否则只需要返回非递减值（或非递增值）。序列'1，2，2，2，3，4'是单调的，但不是严格单调的。
:::

Now, having time that never goes back is a useful property, but there are many cases when that is not enough. One of them is a common one encountered by people programming Erlang on their home laptop. You're sitting at the computer, running Erlang tasks at frequent intervals. This works well and has never failed you. But one day, you hear the chime of an ice cream truck outdoors, and put your computer to sleep before running outdoors to grab something to eat. After 15 minutes, you come back, wake your laptop up, and everything starts exploding in your program. What happened?
现在，拥有永不回头的时间是一种有用的属性，但在很多情况下，这还不够。其中之一是人们在家用笔记本电脑上编程Erlang时经常遇到的问题。你坐在电脑前，频繁地运行Erlang任务。这很有效，从来没有让你失望过。但是有一天，你听到外面的冰激凌车发出的钟声，在跑出去拿东西吃之前，先把电脑关掉。15分钟后，你回来，叫醒你的笔记本电脑，你的程序中的一切开始爆炸。怎么搞的？

The answer depends on how time is accounted for. If it's counted in cycles (\"I have seen `N` instructions fly by on the CPU, that's 12 seconds!\"), you could be fine. If it's counted by looking at the clock on the wall and going \"gee golly, it's 6:15 now and it was 4:20 last time! that's 1h55 that went past!\", then going to sleep would hurt a lot for tasks that are expected to run every few seconds or so.
答案取决于如何计算时间。如果以周期计算（\“我看到'N'指令在CPU上飞过，那是12秒！\”），你就可以了。如果通过看墙上的时钟并说“天哪，现在是6点15分，上次是4点20分！这是过去的1h55！”来计算，那么对于预期每隔几秒钟运行一次的任务来说，睡觉会造成很大的伤害。

On the other hand, if you use cycles and keep them stable, you'll never really see the clock from the program synchronize up with the underlying operating systems. This means that either we can get an accurate `now()` value, or accurate intervals, but not both at once.
另一方面，如果你使用循环并保持它们稳定，你永远不会真正看到程序的时钟与底层操作系统同步。这意味着要么我们可以得到准确的'now（）'值，要么我们可以得到准确的间隔，但不能同时得到两者。

For this reason, the Erlang VM introduces *time correction*. Time correction makes it so the VM, for timers having to do with `now()`, `after` bits in `receive`, `erlang:start_timer/3` and `erlang:send_after/3` along with the `timer` module, will dampen sudden changes by adjusting the clock frequency to go slightly faster or slower.
因此，Erlang VM引入了*时间校正*。时间修正使得VM，对于必须使用'now（）'、'receive'中的'after'位、'erlang:start_timer/3'和'erlang:send_after/3'以及'timer'模块的计时器，将通过调整时钟频率来抑制突然的变化，使其稍微快一点或慢一点。

So instead of seeing either of these curves:

![Two curves with the x-axis being real time and the y-axis being uncorrected VM time; the first curve (labelled 'frequency-based') goes up, plateaus, then goes up again. The second curve (labelled 'clock-based') goes up, plateaus, jumps up directly, then resumes going up](../img/clock-uncorrected.png)
![两条曲线，x轴是实时的，y轴是未校正的VM时间；第一条曲线（标记为“基于频率”）上升，达到平稳，然后再次上升。第二条曲线（标记为“基于时钟的”）上升，平稳，直接上升，然后继续上升](。。/静态/img/时钟未修正。（巴布亚新几内亚）

We would see:

![Three curves with the x-axis being real time and the y-axis being VM time; the first curve (labelled 'frequency-based') goes up, plateaus, then goes up again. The second curve (labelled 'clock-based') goes up, plateaus, jumps up directly, then resumes going up. The third curve, labelled 'corrected time' closes the gap between both other curves after the plateau](../img/clock-corrected.png)
![三条曲线，x轴为实时，y轴为虚拟机时间；第一条曲线（标记为“基于频率”）上升，平稳，然后再次上升。第二条曲线（标记为“基于时钟的”）上升，平稳，直接上升，然后继续上升。第三条曲线，标记为“校正时间”，闭合了平台后两条其他曲线之间的间隙](。。/静态/img/时钟校正。（巴布亚新几内亚）

Time correction in versions prior to 18.0, if undesired, can be turned off by passing the `+c` argument to the Erlang VM.
18岁之前版本的时间修正。如果不需要，可以通过将“+c”参数传递给Erlang VM来关闭0。

## [How Things Are (18.0+)]

The model seen for versions prior to 18.0 was fairly good, but it ended up being annoying in a few specific ways:
18岁之前版本的车型。0相当不错，但它在以下几个方面令人恼火：

-   Time correction was a compromise between skewed clocks and inaccurate clock frequencies. We would trade some accelerated or slowed frequency in order to get closer to the proper OS time. To avoid breaking events, the clock can only be corrected very slowly, so we could have both inaccurate clocks *and* inaccurate intervals for very long periods of time
-时间校正是倾斜时钟和不准确时钟频率之间的折衷。为了更接近合适的操作系统时间，我们会交换一些加速或减慢的频率。为了避免中断事件，时钟只能被非常缓慢地校正，因此我们可能会在很长一段时间内出现不准确的时钟*和*不准确的间隔
-   People used `now()` when they wanted monotonic *and* strictly monotonic time (useful to order events)
-当人们想要单调的*和*严格单调的时间时，他们使用'now（）'（用于安排事件）
-   people used `now()` when they wanted unique values (for the lifetime of a given node)
-   Having the time as `` is annoying and a remnant of times when bigger integers were impractical for the VM to represent and converting to proper time units is a pain. There is no good reason to use this format when Erlang integers can be of any size.
-把时间设为``是很烦人的，当VM无法表示更大的整数并将其转换为适当的时间单位时，剩下的时间是很痛苦的。当Erlang整数可以是任意大小时，没有充分的理由使用这种格式。
-   Backwards leap in time would stall the Erlang clock (it would only progress microseconds at a time, between each call)
-时间上的向后跳跃会使Erlang时钟暂停（每次通话之间，它一次只能前进微秒）

In general, the problem is that there were two tools (`os:timestamp()` and `now()`) to fill all of the following tasks:
一般来说，问题是有两个工具（`os:timestamp（）`和`now（）`）来完成以下所有任务：

-   Find the system's time
-   Measure time elapsed between two events
-   Determine the order of events (by tagging each event with `now()`)
-   Create unique values

All of these are made clearer by exploding the time in Erlang into multiple components, starting in 18.0:
通过将Erlang中的时间分解为多个组件（从18开始），所有这些都变得更加清晰。0:

-   OS system time, also known as the POSIX time.
-   OS Monotonic time; some operating systems provide it, some don't. It tends to be fairly stable when available, and avoids leaps in time.
-单调时间；有些操作系统提供，有些则没有。当可用时，它往往相当稳定，并避免时间跳跃。
-   Erlang system time. It's the VM's take on POSIX time. The VM will try to align it with POSIX, but it may move around a bit depending on the chosen strategy (the strategies are described in [Time Warp](time.html#time-warp)).
-Erlang系统时间。这是虚拟机对POSIX时间的挑战。虚拟机将尝试将其与POSIX对齐，但它可能会根据选择的策略（策略在[Time Warp]（时间）中描述）移动一点。html#时间扭曲）。
-   Erlang monotonic time. This is Erlang's view of the OS monotonic time if available, or the VM's own monotonic version of the system time if not available. This is the adjusted clock that is used for events, timers, and so on. Its stability makes it ideal to count a time interval. Do note that this time is *monotonic*, but not *strictly monotonic*, meaning that the clock can't go backwards, but it can return the same value many times!
-Erlang单调时间。这是Erlang对操作系统单调时间（如果可用）的看法，或者VM对系统时间（如果不可用）的看法。这是用于事件、计时器等的已调整时钟。它的稳定性使它非常适合计算时间间隔。请注意，这个时间是“单调的”，但不是“严格单调的”，这意味着时钟不能倒转，但它可以多次返回相同的值！
-   Time offset; because the Erlang Monotonic time is a stable source of authority, the Erlang system time will be calculated by having a given offset relative to the Erlang monotonic time. The reason for this is that it will allow Erlang to adjust the system time without modifying the monotonic time frequency.
-时间偏移；由于Erlang单调时间是一个稳定的权威来源，因此Erlang系统时间将通过相对于Erlang单调时间的给定偏移量来计算。这样做的原因是，它将允许Erlang在不修改单调时间频率的情况下调整系统时间。

Or more visually:

![comparison of timelines for real time, os monotonic time, Erlang monotonic time, Erlang system time, and OS system time, with their respective synchronization points and offsets.](../img/clock-compare.png)
![实时、os单调时间、Erlang单调时间、Erlang系统时间和os系统时间的时间线比较，以及它们各自的同步点和偏移量。](。。/静态/img/时钟比较。（巴布亚新几内亚）

If the offset is a constant 0, then the VM's monotonic and system times will be the same. If the offset is modified positively or negatively, the Erlang system time may be made to match the OS system time while the Erlang monotonic time is left independent. In practice, it is possible for the monotonic clock to be some large negative number, and the system clock to be modified by the offset to represent the positive POSIX timestamp.
如果偏移量为常数0，则VM的单调时间和系统时间将相同。如果偏移量被正向或负向修改，则可以使Erlang系统时间与OS系统时间匹配，而Erlang单调时间保持独立。在实践中，单调时钟可能是一些大的负数，并且系统时钟可能被偏移量修改以表示正的POSIX时间戳。

With all these new components, another use case remains: unique values that *always* increment. The high cost of the `now()` function was due to this necessity that it never returns the same number twice. As mentioned earlier, the Erlang monotonic time is not *strictly* monotonic: it will possibly return the same number twice if it's called at the same time on two different cores, for example. By comparison, `now()` wouldn't. To compensate for this, a strictly monotonic number generator was added to the VM, so that time and unique integers could be handled separately.
有了所有这些新组件，另一个用例仍然存在：唯一的值*总是*递增。'now（）'函数的高成本是因为它必须永远不会两次返回相同的数字。如前所述，Erlang单调时间不是严格的单调时间：例如，如果在两个不同的核上同时调用，它可能会返回相同的数字两次。相比之下，`now（）`就不会了。为了补偿这一点，VM中添加了一个严格单调的数字生成器，以便可以分别处理时间和唯一整数。

The new components of the VM are exposed to the user with the following functions:

-   [`erlang:monotonic_time()`](http://erldocs.com/18.0/erts/erlang.html#monotonic_time/0) for the Erlang monotonic time. It may return very low negative numbers, but they'll never get more negative.
-[`erlang:monotic_time（）`](http://erldocs。com/18。0/erts/erlang。html#单调时间/0）表示Erlang单调时间。它可能会返回非常低的负数，但它们永远不会变得更负。
-   [`erlang:system_time()`](http://erldocs.com/18.0/erts/erlang.html#system_time/0), for the Erlang system time (after the offset has been applied)
-[`erlang:system_time（）`](http://erldocs。com/18。0/erts/erlang。html#system_time/0），用于Erlang系统时间（应用偏移量后）
-   [`erlang:timestamp()`](http://erldocs.com/18.0/erts/erlang.html#timestamp/0)` format for backwards compatibility
-[`erlang:timestamp（）`](http://erldocs。com/18。0/erts/erlang。html#timestamp/0）`向后兼容的格式
-   [`erlang:time_offset()`](http://erldocs.com/18.0/erts/erlang.html#time_offset/0) to figure out the difference between the Erlang monotonic and Erlang system clocks
-[`erlang:time_offset（）`](http://erldocs。com/18。0/erts/erlang。html#time_offset/0）来计算Erlang单调时钟和Erlang系统时钟之间的差异
-   [`erlang:unique_integer()`](http://erldocs.com/18.0/erts/erlang.html#unique_integer/0) defaults to `[]`, which means that while the integers are unique, they might be positive or negative, and greater or smaller than previous ones given.
-[`erlang:unique_integer（）`](http://erldocs。com/18。0/erts/erlang。html#unique_integer/0）默认为“[]”，这意味着虽然整数是唯一的，但它们可能是正的或负的，并且比之前给出的整数大或小。
-   [`erlang:system_info(os_system_time_source)`](http://erldocs.com/18.0/erts/erlang.html#system_info/1), which gives access to the tolerance, intervals, and values of the OS system time.
-[`erlang:system\u info（os\u system\u time\u source）`](http://erldocs。com/18。0/erts/erlang。html#system\u info/1），它提供对操作系统系统时间的容差、间隔和值的访问。
-   [`erlang:system_info(os_monotonic_time_source)`](http://erldocs.com/18.0/erts/erlang.html#system_info/1): if the OS has a monotonic clock, its tolerance, intervals, and values can be fetched there.
-[`erlang:system_info（os_单调的_时间_源）`](http://erldocs。com/18。0/erts/erlang。html#system_info/1）：如果操作系统有一个单调的时钟，它的容差、间隔和值可以在那里获取。

The `Unit` option in all the functions above can be either `seconds`, `milli_seconds`, `micro_seconds`, `nano_seconds`, or `native`. By default, the type of timestamp returned is in the `native` format. The unit is determined at run time, and a function to convert between time units may be used to convert between them:
上述所有函数中的'Unit'选项可以是'seconds'、'milli_seconds'、'micro_seconds'、'nano_seconds'或'native'`。默认情况下，返回的时间戳类型为“本机”格式。单位在运行时确定，可以使用在时间单位之间转换的函数在时间单位之间进行转换：

```eshell
1> erlang:convert_time_unit(1, seconds, native).
1000000000
```

Meaning that my linux VPS has a unit in nanoseconds. The actual resolution may be lower than that (it's possible that only milliseconds are accurate), but nonetheless, it natively works in nanoseconds.
这意味着我的linux VPS有一个以纳秒为单位的单位。实际分辨率可能低于此值（可能只有毫秒是准确的），但它本机的工作时间为纳秒。

The last tool in the arsenal is a new type of monitor, usable to detect when the time offset jumps. It can be called as `erlang:monitor(time_offset, clock_service)`. It returns a reference and when time drifts, the message received will be ``.
武器库中的最后一个工具是一种新型监视器，可用于检测时间偏移量何时跳变。它可以被称为“erlang:monitor（时间偏移，时钟服务）`。它返回一个引用，当时间漂移时，接收到的消息将被删除``。

So how does time get adjusted? Get ready for time warp!

## [Time Warp]

![A Dali-style melting clock](../img/clock-melt.png)

The old style Erlang stuff would just make clocks drifts faster and slower until they matched whatever the OS gave. This was okay to keep some semblance of real time when clocks jumped around, but also meant that over time, events and timeouts would occur faster and slower by a small percentage across multiple nodes. You also had a single switch for the VM, `+c`, which disabled time correction altogether.
老式的Erlang设备只会让时钟漂移得越来越快，越来越慢，直到它们匹配操作系统提供的任何功能。当时钟跳来跳去时，这可以保持一些实时性的外观，但也意味着随着时间的推移，事件和超时会在多个节点上以小百分比的速度越来越慢。您还有一个用于VM的开关，“+c”，它完全禁用了时间校正。

Erlang 18.0 introduces a distinction between how things are done and makes it a lot more powerful and complex. Whereas versions prior to 18.0 only had time drift, meaning the clocks would accelerate or slow down, 18.0 introduced both time correction and a thing called *time warp*.
二郎18。0引入了做事方式之间的区别，并使其更加强大和复杂。而18岁之前的版本。0只有时间漂移，这意味着时钟会加速或减慢，18。0引入了时间校正和一种叫做*时间扭曲的东西*。

Basically, *time warp*, configured with `+C`, is about choosing how the *offset* (and therefore the *Erlang system time*) jumps around to stay aligned with the OS. Time warp is a time jump. Then there's *time correction*, configured with `+c`, which is how the *Erlang monotonic time* behaves when the OS monotonic clock jumps.
基本上，配置了“+C”的*time warp*，是关于选择*offset*（因此，*Erlang系统时间*）如何跳转以与操作系统保持一致。时间扭曲是一种时间跳跃。然后是用“+c”配置的*时间校正*，这就是操作系统单调时钟跳转时*Erlang单调时间*的行为。

There's only two strategies for time correction, but there's three for time warp. The problem is that the time warp strategy chosen impacts the time correction impact, and therefore we end up with a stunning *6* possible behaviours. To make sense out of this the following table might help:
时间修正只有两种策略，但时间扭曲有三种。问题是，选择的时间扭曲策略会影响时间修正效果，因此我们最终会出现惊人的*6*可能的行为。为了理解这一点，下表可能会有所帮助：

+-----------------------------------+-----------------------------------+
| `+C no_time_warp`                 |   ------------ ------             |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   |   `+c true`    Work               |
|                                   | s exactly as it did before 18.0.  |
|                                   | Time does not warp (does not jump |
|                                   | ), but clock frequency is adjuste |
|                                   | d to compensate. This is the defa |
|                                   | ult, for backwards compatibility. |
|                                   |   `+c false`   If the OS          |
|                                   |  system time jumps backwards, the |
|                                   |  Erlang Monotonic clock stalls un |
|                                   | til the OS system time jumps back |
|                                   |  forward, which can take a while. |
|                                   |   ------------ ------             |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
+-----------------------------------+-----------------------------------+
| `+C multi_time_warp`              |   ------------ --------------     |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   |   `+c true`    The                |
|                                   |  Erlang system time is adjusted b |
|                                   | ackwards and forwards via the off |
|                                   | set to match the OS system time.  |
|                                   | The monotonic clock can remain as |
|                                   |  stable and accurate as possible. |
|                                   |   `+c false`   The Erlang s       |
|                                   | ystem time is adjusted backwards  |
|                                   | and forwards via the offset, but  |
|                                   | because there's no time correcti |
|                                   | on, the monotonic clock may pause |
|                                   |  briefly (without freezing long). |
|                                   |   ------------ --------------     |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
|                                   | --------------------------------- |
+-----------------------------------+-----------------------------------+
| `+C single_time_warp`             | This is a special hybrid mode to  |
|                                   | be used on embedded hardware when |
|                                   | you know Erlang boots before the  |
|                                   | OS clock is synchronized. It      |
|                                   | works in two phases:              |
|                                   |                                   |
|                                   | 1.  a.  (with `+c true`) When the |
|                                   |         system boots, the         |
|                                   |         monotonic clock is kept   |
|                                   |         as stable as possible,    |
|                                   |         but no system time        |
|                                   |         adjustments are made      |
|                                   |     b.  (with `+c false`) Same as |
|                                   |         `no_time_warp`            |
|                                   |                                   |
|                                   | 2.  The user calls                |
|                                   |     `erlang:sys                   |
|                                   | tem_flag(time_offset, finalize)`, |
|                                   |     the Erlang system time warps  |
|                                   |     once to match the OS system   |
|                                   |     time, and then the clocks     |
|                                   |     become equivalent to those    |
|                                   |     under `no_time_warp`.         |
+-----------------------------------+-----------------------------------+

Whew. In short, the best course of action is to make sure your code can deal with time warping, and go into multi time warp mode. If your code isn't safe, stick to no time warp.
呼。简而言之，最好的做法是确保代码能够处理时间扭曲，并进入多时间扭曲模式。如果您的代码不安全，请坚持“无时间扭曲”。

## [How to Survive Time Warps]

-   To find system time: `erlang:system_time/0-1`
-   To measure time differences: call `erlang:monotonic_time/0-1` twice and subtract them
-   To define an absolute order between events on a node: `erlang:unique_integer([monotonic])`
-   Measure time *and* make sure an absolute order is defined: ``
-   Create a unique name: `erlang:unique_integer([positive])`. Couple it with a node name if you want the value to be unique in a cluster, or try using [UUIDv1](https://github.com/okeuday/uuid)s.
-创建一个唯一的名称：`erlang:unique_integer（[positive]）`。如果希望值在集群中是唯一的，请将其与节点名称耦合，或者尝试使用[UUIDv1](https://github。com/okeuday/uuid）s。

By following these concepts, your code should be fine to use in multi time warp mode with time correction enabled, and benefit from its better accuracy and lower overhead.
通过遵循这些概念，您的代码应该可以在启用时间校正的多时间扭曲模式下使用，并受益于其更高的准确性和更低的开销。

With all this information in hand, you should now be able to drift and warp through time!
