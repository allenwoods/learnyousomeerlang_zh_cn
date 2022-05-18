# Starting Out

## [The Shell]

In Erlang, you can test most of your stuff in an emulator; it will run your scripts when compiled and deployed, but it will also let you edit stuff live. To start the shell in Linux, open a terminal and then type in `$ erl`. If you've set up everything fine, you should see text like this:
在Erlang中，您可以在模拟器中测试大部分内容；它将在编译和部署时运行您的脚本，但它也允许您实时编辑内容。要在Linux中启动shell，请打开一个终端，然后键入“$erl”`。如果你把一切都设置好了，你会看到这样的文本：

```eshell
Erlang R13B01 (erts-5.7.2) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.7.2  (abort with ^G)
```

Congratulations, you're running the Erlang shell!

For Windows users, you can still run the `erl.exe` shell, but it's recommended you instead use `werl.exe`, which can be found in your start menu (`programs > Erlang`). Werl is a windows-only implementation of the Erlang shell, having its own window with scrollbars and supporting command-line editing (like copy-pasting, which got to be a pain with the standard `cmd.exe` shell in Windows). The erl shell is still required if you want to redirect standard input or output, or use pipelines.
对于Windows用户，您仍然可以运行`erl。exe`shell，但建议您改用`werl。exe`，可以在“开始”菜单中找到（`programs>Erlang`）。Werl是Erlang shell的一个仅限windows的实现，它有自己的带有滚动条的窗口，并支持命令行编辑（比如复制粘贴，这对于标准的`cmd来说是一个难题）。exe（Windows中的shell）。如果要重定向标准输入或输出，或使用管道，仍然需要erl shell。

We'll be able to enter and run code in the emulator, but first, let's see how we can get around in it.
我们可以在模拟器中输入和运行代码，但首先，让我们看看如何在模拟器中运行。

## [Shell Commands]

The Erlang shell has a built-in line editor based on a subset of Emacs, a popular text editor that's been in use since the 70s. If you know Emacs, you should be fine. For the others, you'll do fine anyway.
Erlang shell有一个基于Emacs子集的内置行编辑器，Emacs是一种流行的文本编辑器，自70年代开始使用。如果你知道Emacs，你应该没事。对于其他人，你无论如何都会做得很好。

![super turtle](../img/shell.png "In your face, Hare!")

First of all, if you type some text and then go `^A` (Ctrl+A), you should see your cursor moving to the beginning of the line. `^E` (Ctrl+E) gets you to the end. You can use arrow keys to go forward, backwards, show previous or next lines so you can repeat code.
首先，如果您键入一些文本，然后转到“^A”（Ctrl+A），您应该会看到光标移动到行首。`^E`（Ctrl+E）让你走到终点。可以使用箭头键向前、向后、显示上一行或下一行，以便重复代码。

If you type something like `li` and then press \"tab\", the shell will have completed the terms for you to `lists:`. Press tab again, and the shell will suggest you many functions to use after. This is Erlang completing the module `lists` and then suggesting functions from it. You may find the notation weird, but don't worry, you'll get familiar with it soon enough.
如果您键入类似“li”的内容，然后按“tab”，shell将为您完成“列表”的条款：`。再次按tab键，shell将向您推荐许多以后要使用的函数。这是Erlang完成模块'lists'，然后从中建议函数。你可能会觉得这个符号很奇怪，但别担心，你很快就会熟悉它的。

I think we've seen enough of shell functionality to be alright, except for one thing: we don't know how to leave! There's a fast way to find how. Just type in `help().` and you should get information on a bunch of commands you can use in the shell (do not forget the full stop (`.`) as it is necessary for the command to run). We'll use some of them at a later point, but the only line of concern to us in order to get out is\
我认为我们已经看到了足够多的shell功能，除了一件事：我们不知道如何离开！有一种快速的方法可以找到。只需输入“help”（帮助）。`你应该了解一些可以在shell中使用的命令（不要忘记句号）(`。`)因为命令必须运行）。我们将在稍后使用其中的一些，但我们唯一关心的问题是\
`q()        -- quit - shorthand for init:stop()`\
So this is one way to do it (in fact, two ways). But this won't help us if the shell freezes! If you were paying attention, when you started the shell, there was a comment about 'aborting with `^G`'. Let's do that, and then press `h` to get help!
所以这是一种方法（实际上是两种方法）。但如果外壳冻结，这对我们没有帮助！如果你注意到了，当你启动shell时，有一条关于“用“^G”中止”的评论。让我们这样做，然后按“h”键寻求帮助！

```eshell
User switch command
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q        - quit erlang
  ? | h             - this message
 -->
```

If you type in `i` then `c`, Erlang should stop the currently running code and bring you back to a responsive shell. `j` will give you a list of processes running (a star after a number indicates this is the job you are currently running), which you can then interrupt with `i` followed by the number. If you use `k`, you will kill the shell as it is instead of just interrupting it. Press `s` to start a new one.
如果输入'i'，然后输入'c'，Erlang应该停止当前正在运行的代码，并将您带回响应shell。`j`会给你一个正在运行的进程列表（数字后的星号表示这是你当前正在运行的作业），然后你可以用'i'和数字来打断它。如果你使用'k'，你会按原样杀死外壳，而不是打断它。按's'开始一个新的。

```eshell
 Eshell V5.7.2  (abort with ^G)
1> "OH NO THIS SHELL IS UNRESPONSIVE!!! *hits ctrl+G*"
User switch command
 --> k
 --> c
Unknown job
 --> s
 --> j
   2* 
 --> c 2
Eshell V5.7.2  (abort with ^G)
1> "YESS!"
```

If you read back the help text, you'll notice we can start remote shells. I won't get into details right now, but this should give you an idea of what the Erlang VM can do apart from running code. For now, let's get things started (for real).
如果你读回帮助文本，你会注意到我们可以启动远程shell。我现在不会详细介绍，但这应该会让您了解Erlang VM除了运行代码之外还能做些什么。现在，让我们开始（真正的）。
