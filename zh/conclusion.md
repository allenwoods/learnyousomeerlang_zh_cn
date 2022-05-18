# Conclusion

## [A Few Words]

I see you chose to read the conclusion after all. Good for you. Before I get to point you to a bunch of interesting topics that you could explore, were you to pick Erlang as a development language you want to learn more about, I'd like to take a few lines to say writing Learn You Some Erlang has been one hell of a ride. It took me three years of hard work while studying and working full time, and juggling every day's life needs (if I had children, they'd have died of neglect a while ago now).
我知道你最终还是选择了阅读结论。真为你高兴。在我给大家介绍一系列有趣的话题之前，如果你选择Erlang作为一种开发语言，你想了解更多，我想用几句话告诉大家，编写学习Erlang是一件非常困难的事情。我花了三年的时间在学习和全职工作的同时努力工作，并兼顾每天的生活需求（如果我有孩子，他们早就死于忽视了）。

This site, coupled with some luck and some more work, allowed me to get jobs, both as an Erlang trainer, course material writer, and developer. It allowed me to travel around the world and meet a crapload of interesting people. It drained a lot of energy, cost me a decent chunk of money and time to run, but it paid back tenfold in most ways imaginable.
这个网站，再加上一些运气和更多的工作，让我找到了工作，既当了Erlang培训师，又当了课程材料编写员和开发人员。它让我得以环游世界，结识了许多有趣的人。它消耗了很多精力，花了我不少钱和时间跑步，但在大多数可以想象的方面，它的回报是我的十倍。

I have to give a lot of thanks to the Erlang community in general. They helped me learn stuff, reviewed pages and pages of material for free, fixed my typoes, helped me get better at writing English and writing in General. There's been dozen of people helping in many ways. The biggest contributors in terms of time, advice, and general resources are all in this site's [FAQ](faq.html). If you've ever written me an e-mail telling me you'd buy me a beer, buy it to one of these guys instead; they deserve it as their participation was way more thankless than mine.
总的来说，我要感谢Erlang社区。他们帮助我学习东西，免费阅读一页又一页的材料，修正我的打字错误，帮助我更好地写英语和写作。已经有十几个人在很多方面提供了帮助。在时间、建议和一般资源方面，最大的贡献者都在本网站的[FAQ]（FAQ）中。（html）。如果你曾经给我写过一封电子邮件，告诉我你要给我买一杯啤酒，那就把它买给这些家伙中的一个；他们值得这样做，因为他们的参与比我的更吃力不讨好。

The Erlang community as a whole has been very welcoming to the work I've been doing with LYSE, helped make it known to readers (it's even on the official Erlang documentation and website!). Without the concerted efforts of everyone around Erlang, this site would probably have died after four or five chapters, left to be yet another useless website clogging the Internet's tubes. So, hey, thanks.
作为一个整体，Erlang社区非常欢迎我用LYSE所做的工作，帮助读者了解它（甚至在Erlang官方文档和网站上！）。如果没有Erlang周围所有人的共同努力，这个网站可能会在四、五章之后消亡，成为另一个堵塞互联网管道的无用网站。所以，嘿，谢谢。

## [Other Topics]

There's only so many topics I could cover without going over the top. This site, if it were to be turned in dead tree form, would probably yield around 600 pages now. It's taken three years to bring it there, and I'm tired and glad it's over (what am I gonna do with all that free time, now?), but there are still plenty of topics I would have *loved* to cover. Here's a quick list:
我能涵盖的话题太多了，不必过火。如果这个网站变成枯树形，现在可能会有600页左右的页面。我花了三年时间才把它带到那里，我很累，也很高兴它结束了（现在我该怎么利用这些空闲时间呢？），但我仍然有很多我喜欢的话题。下面是一个快速列表：

### Tracing BIFs and DBG

The Erlang VM is traceable inside and out. Got a bug or some stack trace you can't make sense of? Turn on a few [trace flags](http://www.erlang.org/doc/man/erlang.html#trace-3) module's tracing functions. They work only on OTP behaviourised processes, but they're often good enough to get going.
Erlang虚拟机可以从内部和外部进行跟踪。有一个bug或一些你无法理解的堆栈跟踪吗？打开几个[跟踪标志](http://www。二郎。org/doc/man/erlang。html#trace-3）模块的跟踪功能。它们只适用于OTP行为化流程，但它们通常足够好，可以开始工作。

### Profiling

Erlang comes with a good bunch of different profiling tools to analyze your programs and find all kinds of bottlenecks. The [fprof](http://www.erlang.org/doc/apps/tools/fprof_chapter.html) for code coverage. Most of them are built using the tracing BIFs of the language, funnily enough.
Erlang提供了一系列不同的分析工具，可以分析程序并找到各种瓶颈。[fprof](http://www。二郎。org/doc/apps/tools/fprof_章。html）用于代码覆盖率。有趣的是，它们中的大多数都是使用该语言的跟踪BIF构建的。

### More Introspection

Top-like tools exist for Erlang, such as [pman](http://www.erlang.org/doc/apps/pman/pman_chapter.html), but I do recommend DBG instead of that one. To explore entire supervision trees for your nodes, appmon is your app.
Erlang也有类似Top的工具，比如[pman](http://www。二郎。org/doc/apps/pman/pman_章。html），但我建议使用DBG而不是那个。要探索节点的整个监控树，appmon是您的应用程序。

### Documentation

[EDoc](http://www.erlang.org/doc/apps/edoc/chapter.html) is a tool that lets you turn your Erlang modules into HTML documentation. It supports annotations and ways to declare specific pages that allow you to build small websites to document your code. It's similar to Javadoc, if you've heard of it.
[EDoc](http://www。二郎。org/doc/apps/edoc/chapter。html）是一个可以将Erlang模块转换为html文档的工具。它支持注释和声明特定页面的方法，允许您构建小型网站来记录代码。如果你听说过的话，它类似于Javadoc。

### GUIs

The [Wx](http://www.erlang.org/doc/apps/wx/chapter.html) application is the new standard for multiplatform GUI writing with Erlang. I'm terrible at GUI stuff, so it's probably better for everyone I actually didn't cover that one.
[Wx](http://www。二郎。org/doc/apps/wx/chapter。html）应用程序是使用Erlang编写多平台GUI的新标准。我在GUI方面做得很糟糕，所以对每个人来说可能都更好，因为我实际上没有涉及到这一点。

### Other Useful Libraries

There are plenty of nice libraries coming by default with Erlang not mentioned here. Cryptography tools, web servers, web clients, all kinds of protocol implementations, and so on. You can get a general list of them at [http://www.erlang.org/doc/applications.html](http://www.erlang.org/doc/applications.html).
这里没有提到Erlang，默认情况下有很多不错的库。加密工具、web服务器、web客户端、各种协议实现等等。你可以在[http://www。二郎。org/doc/applications。[html](http://www。二郎。org/doc/applications。（html）。

### Community libraries

There is a crapload of them. I didn't want to cover them because they can tend to change and I didn't want to favor one over the other, but here's a quick list: [Rebar3](https://github.com/erlang/rebar3) for some generic message-based pool, and a whole lot more stuff. Community libraries could easily get their own book.
有一大堆。我不想报道它们，因为它们可能会发生变化，我不想偏袒其中一个，但这里有一个快速列表：[Rebar3](https://github。com/erlang/rebar3）来获取一些通用的基于消息的池，以及更多的东西。社区图书馆很容易就能买到自己的书。

## [I heard LYSE is also a book?]

You heard right. Thanks to No Starch Press, Learn You Some Erlang is available both as a [dead tree book and an ebook](http://nostarch.com/erlang)! At a large 600 black and white pages, including images (in color for ebook copies), you can now have the largest Erlang-themed paperweight and bookcase decoration printed to date (as far as I know). This should ease the sharp pain of reading hundreds of pages on a computer screen.
你没听错。感谢无淀粉印刷机，了解一些Erlang既可以作为[枯树书，也可以作为电子书](http://nostarch。com/erlang）！有600多张黑白页面，包括图片（电子书的彩色），你现在可以打印出迄今为止最大的Erlang主题镇纸和书架装饰（据我所知）。这将减轻在电脑屏幕上阅读数百页的痛苦。

## [Your ideas are intriguing to me and I wish to subscribe to your newsletter]

I have a blog at [ferd.ca](http://ferd.ca) where I discuss all kinds of stuff (or at least I want to), but inevitably come back to Erlang topics, due to using it all the time.
我在[ferd]有一个博客。ca](http://ferd。ca）在这里，我讨论了各种各样的东西（或者至少我想），但不可避免地会回到Erlang主题，因为我一直在使用它。

## [Is that it?]

Yes, it is. Have a nice day!
