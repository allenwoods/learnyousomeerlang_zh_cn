# 初探

## [终端模拟器](#the-shell)

在Erlang中，您可以在模拟器中测试大部分内容；它将在编译和部署时运行您的脚本，但它也允许您实时编辑内容。要在Linux中启动shell，请打开一个终端，然后键入`$erl`。如果你把一切都设置好了，你会看到这样的文本：

```shell
Erlang R13B01 (erts-5.7.2) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.7.2  (abort with ^G)
```

祝贺！你成功启动了Erlang shell

对于Windows用户，您仍然可以运行`erl.exe`shell，但建议您改用`werl.exe`，可以在“开始”菜单中找到（`programs>Erlang`）。Werl是Erlang shell的一个Windows专用实现，它有自己的带有滚动条的窗口，并支持命令行编辑（比如复制粘贴，这对于标准的Windows shell`cmd.exe`来说是一个难题）。如果要重定向标准输入或输出，或使用管道，仍然需要erl shell。

我们可以在模拟器中输入和运行代码，但首先，让我们看看如何在模拟器中执行基础操作。

## [Shell命令](#shell-commands)

Erlang shell有一个基于Emacs子集的内置行编辑器，Emacs是一种流行的文本编辑器，自70年代开始使用。无论你知不知道Emacs，你应该都可以正常的使用这个编辑器。

![super turtle](../img/shell.png "In your face, Hare!")

首先，如果您键入一些文本，然后转到`^A`（Ctrl+A），您应该会看到光标移动到行首。`^E`（Ctrl+E）让你走到终点。可以使用箭头键向前、向后、显示上一行或下一行，以便重复代码。

如果您键入类似`li`的内容，然后按`tab`，shell将为您完成`list:`。再次按tab键，shell将向您推荐许多以后要使用的函数。这是Erlang模块`lists`的自动补全函数功能。你可能会觉得这个符号很奇怪，但别担心，你很快就会熟悉它的。

我认为暂时知道这些shell功能就够了，除了一件事：我们不知道如何离开！有一种快速的方法可以找到。只需输入`help().`，你就可以了解一些可以在shell中使用的命令（不要忘记句号`.`，否则命令不会运行）。我们将在稍后使用其中的一些，但现在我们唯一关心的问题是如何退出编辑器：
`q()        -- quit - shorthand for init:stop()`

所以这是一种方法（实际上提示了两种方法，另一种是`init:stop()`）。但如果shell冻结了，这种方法显然不起作用！如果你注意到当你启动shell时，有一条关于'aborting with `^G`'的提示。让我们这样做，然后按`h`键寻求帮助！

```shell
User switch command
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 -->
```

如果输入`i`，然后输入`c`，Erlang会停止当前正在运行的代码，并将您带回响应shell。`j`会给你一个正在运行的进程列表（数字后的星号表示这是你当前正在运行的作业），然后你可以用`i`和数字来打断它。如果你使用`k`，你会立刻杀死外壳，而不是打断它。按`s`开始一个新的。

```shell
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

如果你读回帮助文本，你会注意到我们可以启动远程shell。我现在不会详细介绍，但这应该会让您了解Erlang VM除了运行代码之外还能做些什么。现在，让我们开始（真正的）Erlang初探吧。
