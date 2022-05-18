# 概述

## [关于本书](#about-this-tutorial)

这是《Learn You Some Erlang for Great Good》（以下简称LYSE）的开始！阅读本教程可能是你学习Erlang的第一步，所以让我们来讨论一下。

![Erlang logo](../img/erlang.png)

首先，我在读了米兰·利波瓦·查阿（Miran Lipovača）的 [《Learn You a Haskell for great Good! (LYAH)》](http://learnyouahaskell.com/ "Free Book!") 之后，开始产生了写这本书的想法。我认为他在提高语言吸引力和学习体验的友好度方面做得很好。因为我们早就认识了，我问他如果我模仿LYAH写一个Erlang版本怎么样。他喜欢这个主意，而且对Erlang也有点感兴趣。

所以我就开始写了。当然，我的动机还有其他来源：我主要觉得入门Erlang这门语言很难（网络上的文档很少，你需要买书），而且我认为社区会从类似LYAH的指南中受益。此外，我见过一些人有时会基于笼统的概括，就认为Erlang的优点太多或不够。当然，有人肯定会认为Erlang只不过是炒作而已。不过即使我想说服他们，他们大概从一开始就不太可能读到这篇文章吧。

因此，本书希望自己能成为学习Erlang的一种方式，主要面向的对象是那些掌握已经掌握命令式语言（如C/C++、Java、Python、Ruby等）编程基础知识，但可能还不太熟悉函数式编程（Haskell、Scala、Erlang、Clojure、OCaml……）的人。我想以诚实的方式来写这本书，在推销Erlang的同时，承认它的缺点和优点。

## [什么是Erlang?](#what-is-erlang)

首先，Erlang是一种函数式编程语言。如果你曾经使用命令式语言，像`i++`这样的语句对你来说可能很正常；在函数式编程中，它们是不允许的。事实上，更改任何变量的值都是严格禁止的！一开始这听起来可能很奇怪，但如果你还记得你的数学课，那实际上就是你是如何学习的：

```shell
y = 2
x = y + 3
x = 2 + 3
x = 5
```

如果在上述语句的后面，我们又定义了下面的内容：

```shell
x = 5 + 1
x = x
∴ 5 = 6
```

你会很困惑的。函数式编程认识到了这一点：如果我说`x`是5，那么我就不能从逻辑上说它也是6！这是不诚实的。这也是为什么具有相同参数的函数应始终返回相同的结果：

```shell
x = add_two_to(3) = 5
∴ x = 5
```

对同样的参数，函数总是返回相同结果，这种性质我们称为称为**引用透明性**。这就是我们可以用5代替`add_two_to（3）`的原因，因为`3+2`的结果永远是5。这意味着我们可以将几十个函数粘在一起，以解决更复杂的问题，同时确保不会出现任何问题。逻辑清晰，不是吗？但有一个问题：

```shell
x = today() = 2009/10/22
#  -- 一天后 --
x = today() = 2009/10/23
x = x
∴ 2009/10/22 = 2009/10/23
```

哦不！我美丽的方程式！他们突然全都错了！为什么我的函数每天都返回不同的结果？

显然，在某些情况下，打破引用透明性是有用的。Erlang在函数式编程方面有一种非常实用的方法：遵守其最纯粹的原则（引用透明性、避免可变数据等），但当现实世界出现问题时，就要脱离这些原则。

![An envelope](../img/envelope.png)

现在，我们将Erlang定义为一种函数式编程语言，但它也非常强调并发性和高可靠性。为了能够同时执行几十个任务，Erlang使用[actor模型](http://en.wikipedia.org/wiki/Actor_model "A more technical definition")，每个Actor在虚拟机中是一个单独的进程。简而言之，如果你是Erlang世界里的一个actor，你将是一个孤独的人，坐在一间没有窗户的黑暗房间里，在你的邮箱旁等待一条消息。一旦你收到一条信息，你会以一种特定的方式对其做出反应：你在收到账单时付账，你会用一封“谢谢”的信来回复生日卡，而你会忽略那些你听不懂的信。

Erlang的actor模型可以想象为一个每个人都独自坐在自己的房间里，可以执行一些不同的任务的世界。每个人都通过写信来进行严格的沟通，仅此而已。虽然这听起来像是一种无聊的生活（也可能是邮政服务的一个新时代），但这意味着你可以要求许多人为你完成非常具体的任务，他们中没有一个人会做错事或犯错误，从而影响他人的工作；他们甚至可能不知道你以外的人的存在（这很好）。

为了跳出这种类比，Erlang强制您编写actor（进程），这些actor除了相互传递消息，不会与其他actor共享任何代码和信息。每一次交流都是明确的、可追踪的和安全的。

当我们定义Erlang时，我们是在语言级别上定义的，但从更广泛的意义上讲，这并不是它的全部：Erlang也是一个整体的开发环境。代码被编译成字节码并在虚拟机中运行。因此，Erlang就像Java和有多动症的小朋友一样，可以在任何地方运行（run）。标准发行版包括了开发工具（编译器、调试器、分析器、测试框架）、开放电信平台（OTP）框架、web服务器、解析器生成器和mnesia数据库（一个键值存储系统，能够在多台服务器上复制自身，支持嵌套事务，并允许您存储任何类型的Erlang数据）和许多其他工具。

VM和库还允许您在不中断任何程序的情况下更新运行系统的代码，在许多计算机上轻松分发代码，并以简单但强大的方式管理错误和故障。

![A crashed plane](../img/letitcrash.png "Don't do this")

稍后我们将了解如何使用这些工具中的大多数并实现安全性，但现在，我将告诉您Erlang处理此类问题的一般性策略：**让它崩溃（Let it crash）**。当然，不是像一架有几十名乘客的飞机那样crash，而更像是一个有安全网保护的走钢丝的人。虽然您应该避免犯错误，但在大多数情况下，您不需要检查每种类型或错误情况。

Erlang's ability to recover from errors, organize code with actors and making it scale with distribution and concurrency all sound awesome, which brings us to the next section\...
Erlang从错误中恢复的能力、使用actor组织代码并提供分布式拓展和并发性的能力听起来都很棒，这就把我们带到了下一节……

## [不要削足适履](#kool-aid)[^1]

书中可能有许多小的黄橙色部分，名字和这一节类似（当你看到它们时，你会认出它们 [^2]）。由于热情洋溢的谈话，Erlang目前正在获得很多人气，这可能会让人们相信它比实际情况更重要。如果你是这些过度热情的学习者之一，这些提醒将帮助你脚踏实地。

第一种情况与Erlang由于其轻量级进程而具有的大规模扩展能力有关。Erlang进程确实很轻：你可以同时拥有数十万个进程，但你可以并不意味着你必须这样使用它。例如，创建一个包括子弹在内的一切都是actor的射击游戏是疯狂的。在这样的游戏中，你唯一能射到的就是你自己的脚。将信息从一个actor发送到另一个actor的成本虽小，如果你划分的任务太多，*你会让事情变得更慢*！

当我们深入学习到需要担心这些问题的时候，我会更详细地讨论它们，但请记住，随机地将一个问题并行化并不足以让它运行地更快行。不要悲伤；有时，使用数百个过程也是可能且有用的！只是不会总是有用。

据说Erlang能以与计算机核数成正比的方式扩展，但这通常不是真的：

![Bad Graph: Speed vs Cores: It just scales!](../img/scaling.png)

虽然这是可能的，但大多数问题的运行方式并不允许同时运行所有内容。

还有一点需要记住：虽然Erlang在某些方面做得很好，但从技术上讲，你仍然可以从其他语言中获得相同的结果，反之亦然；根据需要评估每个问题，并根据所解决的问题选择合适的工具。Erlang不是什么灵丹妙药，在图像和信号处理、操作系统设备驱动程序等方面尤其糟糕。Erlang擅长的事服务器使用的大型软件（例如，队列、map-reduce）、与其他语言结合进行提升、实现更高级别的协议等。而这中间区域如何处理将取决于您。你不必将Erlang的使用局限在在服务器软件中，有人也用它实现了一些意想不到的事情。其中一个例子是UNICT团队开发的机器人IANO，它使用Erlang实现人工智能，并在2009年的[欧洲机器人大赛](http://eurobot.dmi.unict.it/)上获得银牌。另一个例子是Wings 3D，它是一个开源的3D建模工具（但不是渲染器），用Erlang编写，因此是跨平台的。

## [你需要什么?](#what-you-need)

您只需要开始一个文本编辑器和Erlang环境。您可以从[官方Erlang网站](http://erlang.org/download.html "official Erlang download page")获取源代码和Windows二进制文件。记得修改系统变量以便能够从命令行访问它。

在基于Debian的Linux发行版上，您应该能够通过执行`$apt get install erlang`来安装该软件包。在Fedora上（如果安装了`yum`），可以通过键入`#yum install erlang`来实现同样的功能。然而，这些存储库通常保存着Erlang包的过时版本；使用过时的版本可能会与本教程中的内容有所不同，并且会在某些应用程序中获得较差的性能。因此，我建议从源代码处编译。请查阅软件包中的自述文件，并通过谷歌获得您需要的所有安装细节，他们的工作将比我做得更好。

在FreeBSD上，您可以使用许多选项。如果您使用的是portmaster，那么可以使用`portmaster lang/erlang`。对于标准端口，它应该是`cd/usr/ports/lang/erlang；make install clean`。最后，如果要使用包，请运行`pkg_add-rv erlang`。

如果您在OSX上，可以使用`$brew install Erlang`（使用`[Homebrew](https://github.com/Homebrew/homebrew)`）安装Erlang。

此外，Erlang Solutions Ltd. 提供了[适用于所有主要操作系统的软件包](https://www.erlang-solutions.com/resources/download.html)，一般这些软件包都运行良好（选择一个"Standard"发行版）。

*注意：在撰写本文时，我使用的是Erlang版本R13B+，但为了获得最佳效果，您应该使用更新的版本。*

## [如何求助](#get-help)

有几个地方你可以得到帮助。如果您使用的是linux，您可以访问手册页以获取良好的技术文档。Erlang有一个列表模块（我们很快就会看到）：要获取关于列表的文档，只需输入`$erl-man lists`。

在Windows上，安装已经包括了HTML文档。您也可以随时从[官方erlang网站](http://erlang.org/doc/ "Official Erlang HTML documentation")下载。

良好的编码实践可以在[这里](http://www.erlang.se/doc/programming_rules.shtml)查看，如果你觉得有必要保持代码的整洁。本书中的代码也将尝试遵循这些准则。

Now, there are times where just getting the technical details isn't enough. When that happens, I tend to turn to two main sources: the official [mailing list](http://www.erlang.org/static/doc/mailinglist.html).
有些时候仅仅了解技术细节是不够的。当这种情况发生时，我倾向于求助于两个主要来源：一个是官方[邮件列表](http://www.erlang.org/static/doc/mailinglist.html)。如果你是那种喜欢烹饪书和预制食谱的人，[trapexit](http://trapexit.org/ "trapexit, an Erlang community site")是你要找的地方。它们还将邮件列表镜像为一个论坛和一个通用维基，你可能会发现很有用。

[^1]: 原文为Kool-kid，邪教服毒梗
[^2]: 大法官黄书梗