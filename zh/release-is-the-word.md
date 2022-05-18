# Release is the Word

## [Am I an Executable Yet?]

How far have we got. All this work, all these concepts, and we haven't shipped a single Erlang executable yet. You might agree with me that getting an Erlang system up and running requires a lot of effort, especially compared to many languages where you call the compiler and off you go.
我们还有多远。所有这些工作，所有这些概念，我们还没有发布一个Erlang可执行文件。您可能同意我的观点，即启动并运行Erlang系统需要付出很多努力，尤其是与许多调用编译器的语言相比。

![A slice of pizza](../img/pizza.png)

Of course this is entirely right. We can compile files, run applications, check for some dependencies, handle crashes and whatnot, but it's not very useful without a functioning Erlang system you can easily deploy or ship with it. What use is it to have great pizza when it can only be delivered cold? (people who enjoy cold pizza might feel excluded here. I am sorry.)
当然，这是完全正确的。我们可以编译文件、运行应用程序、检查一些依赖关系、处理崩溃等等，但如果没有一个运行正常的Erlang系统，它就没有多大用处，你可以轻松地部署或附带它。当披萨只能冷送时，有什么用呢？（喜欢冷披萨的人在这里可能会感到被排斥。我很抱歉。)

The OTP team didn't leave us on our own when it comes to making sure real systems come to life. OTP releases are part of a system made to help package applications with the minimal resources and dependencies.
OTP团队在确保真正的系统投入使用方面并没有让我们孤立无援。OTP版本是一个系统的一部分，该系统旨在帮助以最少的资源和依赖性打包应用程序。

## [Fixing The Leaky Pipes]

For our first release, we will reuse our `ppool` and `erlcount` applications from last chapters. However, before we do so, we'll need to change a few things here and there. If you're following along with the book and writing your own code, you might want to copy both of our apps into a new directory called `release/`, which I will assume you will have done for the rest of the chapter.
在我们的第一个版本中，我们将重用上一章中的“ppool”和“erlcount”应用程序。然而，在我们这么做之前，我们需要在这里和那里改变一些事情。如果你正在跟随这本书并编写自己的代码，你可能想将我们的两个应用程序复制到一个名为“release/”的新目录中，我想你在本章的其余部分都会这么做。

![A leaky pipe with brown liquid dripping into a metal bucket](../img/pipes.png)

The first thing that's really bothering me with erlcount is that once it's done running, the VM stays up, doing nothing. We might want most applications to stay running forever, but this time it's not the case. Keeping it running made sense because we might have wanted to play with a few things in the shell and needed to manually start applications, but this should no longer be necessary.
erlcount真正困扰我的第一件事是，一旦它完成运行，虚拟机就会保持运行，什么也不做。我们可能希望大多数应用程序永远保持运行，但这次情况并非如此。保持它运行是有意义的，因为我们可能想在shell中处理一些东西，需要手动启动应用程序，但这应该不再是必要的。

For this reason, we'll add a command that will shut the BEAM virtual machine down in an orderly manner. The best place to do it is within [erlcount_dispatch.erl](static/erlang/release/erlcount-1.0/src/erlcount_dispatch.erl.html)'s own terminate function, given it's called after we obtain the results. The perfect function to tear everything down is `init:stop/0`. This function is quite complex, but will take care of terminating our applications in order, will get rid of file descriptors, sockets, etc. for us. The new stop function should now look like this:
因此，我们将添加一个命令，该命令将有序地关闭BEAM虚拟机。最好的地方是在[erlcount_]调度范围内。erl]（静态/erlang/release/erlcount-1。0/src/erlcount\u调度。呃。html）自己的终止函数，因为它在我们获得结果后被调用。完美的函数是'init:stop/0`。这个函数相当复杂，但会按顺序终止我们的应用程序，会去掉文件描述符、套接字等。为了我们。新的停止功能现在应该如下所示：

```erl
terminate(_Reason, _State, _Data) ->
    init:stop().
```

And that's it for the code itself. We've got a bit more work to do, still. When we defined our app files during the two last chapters, we did so while using the absolute minimal amount of information necessary to get them running. A few more fields are required so that Erlang isn't completely mad at us.
这就是代码本身。我们还有一点工作要做。在最后两章中定义应用程序文件时，我们使用了运行它们所需的绝对最少的信息量。还需要几个字段，这样Erlang就不会完全生我们的气了。

First of all, the Erlang tools to build releases require us to be a little bit more precise in our application descriptions. You see, although tools for releases don't understand documentation, they still have this intuitive fear of code where the developers were too impolite to at least leave an idea of what the application does. For this reason, we'll need to add a `description` tuple to both our [ppool.app](static/erlang/release/ppool-1.0/ebin/ppool.app.html) files.
首先，用于构建发行版的Erlang工具要求我们在应用程序描述中更加精确。你看，尽管用于发布的工具不理解文档，但他们仍然对代码有这种直觉上的恐惧，因为开发人员太不礼貌了，至少不知道应用程序是做什么的。因此，我们需要在[ppool]和[ppool]中都添加一个'description'元组。应用程序]（静态/erlang/release/ppool-1。0/ebin/ppool。应用程序。html）文件。

For `ppool`, add the following one:

```erl

```

and for `erlcount`:

```erl

```

Now we'll be able to get a better idea of what's going on when we inspect our different systems.

The most attentive readers will also remember I've mentioned at some point that *all* applications depend on `stdlib` and `kernel`. However, our two app files do not mention any of these. Let's add both applications to each of our app files. This will require to add the following tuple to the [`ppool` app file](static/erlang/release/ppool-1.0/ebin/ppool.app.html):
最细心的读者也会记得我曾经提到过，*所有*应用程序都依赖于'stdlib'和'kernel'`。然而，我们的两个应用程序文件没有提到任何这些。让我们将这两个应用程序添加到每个应用程序文件中。这需要将以下元组添加到[`ppool`app文件]（static/erlang/release/ppool-1）中。0/ebin/ppool。应用程序。html）：

```erl

```

And add the two applications to the existing [erlcount](static/erlang/release/erlcount-1.0/ebin/erlcount.app.html)`.
并将这两个应用程序添加到现有的[erlcount]（static/erlang/release/erlcount-1）中。0/ebin/erlcount。应用程序。（html）`。

::: 
**Don't Drink Too Much Kool-Aid:**\
While this might have virtually no impact when we start releases manually (and even when we generate them with systools, which we'll see very soon), it is absolutely vital to add both libraries to the list.
虽然当我们手动启动版本时（甚至当我们使用SYSTOOL生成它们时，我们很快就会看到），这可能几乎没有影响，但将这两个库添加到列表中是绝对重要的。

People who generate releases with `reltool` (another tool we'll see in this chapter) will definitely need these applications in order for their release to run well, and even to be able to shut the VM down in a respectable manner. I'm not kidding, it's this necessary. I forgot to do it when writing this chapter and lost a night of work trying to find what the hell was wrong when it was just me not doing things right in the first place.
使用“reltool”（我们将在本章中看到的另一个工具）生成版本的人肯定需要这些应用程序才能使其版本运行良好，甚至能够以体面的方式关闭VM。我不是开玩笑，这是必要的。我在写这一章的时候忘了做这件事，我花了一个晚上的时间试图找出到底是哪里出了问题，而一开始只是我做得不对。

It could be argued that ideally, the release systems of Erlang could implicitly add these applications given pretty much all of them (except very special cases) will depend on them. Alas, they don't. We'll have to make do with this.
可以说，理想情况下，Erlang的发布系统可以隐式添加这些应用程序，因为几乎所有应用程序（除了非常特殊的情况）都将依赖于它们。唉，他们没有。我们只好将就一下了。
:::

We've got a termination in place and we have updated the app files and whatnot. The last step before we start working with releases is to *compile all your applications*. Successively run your Emakefiles (with `erl -make`) in each directory containing one. Otherwise, Erlang's tools won't do it for you and you'll end up with a release without code to run. Ouch.
我们已经有了终止协议，我们已经更新了应用程序文件等等。在我们开始使用发行版之前的最后一步是编译所有的应用程序*。在每个包含Emakefiles的目录中连续运行Emakefiles（带有“erl-make”）。否则，Erlang的工具将无法为您完成这项工作，最终您将获得一个没有代码可运行的版本。哎哟。

## [Releases With Systools]

The `systools` application is the simplest one to build Erlang releases. It's the *Easy-Bake Oven*® of Erlang releases. To get your delicious releases out of the `systools` oven, you first need a basic recipe and list of ingredients. If I were to manually describe the ingredients of a successful minimal Erlang release for our erlcount application, It'd look a bit like this:
“systools”应用程序是构建Erlang版本最简单的应用程序。这是Erlang发布的“简易烤箱”。要想从“systools”烤箱中释放出美味，你首先需要一份基本的食谱和配料清单。如果我要为我们的erlcount应用程序手动描述一个成功的最小Erlang版本的成分，它看起来有点像这样：

Ingredients for erlcount 1.0.0

:   -   An Erlang Run Time System (ERTS) of your choice.
    -   A standard library
    -   A kernel library
    -   The ppool application, which should not fail
    -   The erlcount application.

Did I mention that I'm a terrible cook? I'm not sure I can even cook pancakes, but at least I know how to build an OTP release. The ingredient list for an OTP release with `systools` looks like this file, named [erlcount-1.0.rel](static/erlang/release/erlcount-1.0.rel.html) and placed at the top-level of the `release/` directory:
我说过我是个糟糕的厨师吗？我甚至不确定自己是否会做煎饼，但至少我知道如何构建OTP版本。带有“systools”的OTP版本的成分列表类似于此文件，名为[erlcount-1]。0。rel]（静态/erlang/release/erlcount-1。0。雷尔。html）并放置在`release/`目录的顶层：

```erl
{release,
 ,
 ,
 [,
  ,
  ,
  .
```

This just tells you all the same content as my manual recipe, although we can specify how we want the applications to be started (`temporary`, `transient`, `permanent`). We can also specify versions so we can mix and match different libraries from different Erlang versions depending on our needs. To get all the version numbers in there, you can just do the following sequence of calls:
这只是告诉你所有与我的手动配方相同的内容，尽管我们可以指定如何启动应用程序（“临时”、“暂时”、“永久”）。我们还可以指定版本，以便根据需要混合和匹配来自不同Erlang版本的不同库。要获取其中的所有版本号，只需执行以下调用序列：

```eshell
$ erl
Erlang R14B03 (erts-5.8.4) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.4  (abort with ^G)
1> application:which_applications().
[,
 ]
```

So for that one, I was running R14B03. You can see the ERTS version in there right after the release number (the version is 5.8.4). Then by calling `application:which_applications()` on a running system, I can see the two versions I need from `kernel` (2.14.4) and `stdlib` (1.17.4). The numbers will vary from Erlang version to version. However, being explicit about the versions you need is helpful because it means that if you have many different Erlang installs, you might still only want an older version of `stdlib` that won't badly influence whatever you were doing.
所以对于那个，我运行的是R14B03。你可以在发布号后面看到ERTS版本（版本是5）。8.。4)。然后，通过在运行的系统上调用'application:which_applications（）'，我可以从'kernel'（2）中看到我需要的两个版本。14。4） 和“stdlib”（1）。17。4)。不同版本的Erlang的数字会有所不同。然而，明确说明您需要的版本是有帮助的，因为这意味着如果您有许多不同的Erlang安装，您可能仍然只需要一个旧版本的“stdlib”，它不会严重影响您所做的任何事情。

![A chocolate cupcake with pink creamy topping in a purplish paper, with a face, beard, legs and high heel shoes (pink)](../img/cupcake.png "oh you smell so good")
![一个巧克力蛋糕，上面有紫色的纸，上面有粉色奶油状的面，有脸、胡子、腿和高跟鞋（粉色）](。。/静态/图像/纸杯蛋糕。png“哦，你闻起来真香”）

You'll also note that I chose to name the *release* `erlcount` and make it version 1.0.0. This is unrelated to the `ppool` and `erlcount` *applications*, which are both also running the version 1.0.0, as specified in their app file.
您还将注意到，我选择将*release*`erlcount`命名为第1版。0。0。这与同样运行版本1的“ppool”和“erlcount”*应用程序*无关。0。0，在其应用程序文件中指定。

So now we have all our applications compiled, our list of ingredients and the wonderful concept of a metaphorical *Easy-Bake Oven*®. What we need is the actual recipe.
现在我们已经编译了所有的应用程序，我们的配料清单，以及一个隐喻性的“简易烤箱”的奇妙概念。我们需要的是真正的配方。

There are a few concepts that will enter the stage right about now. A recipe will tell you a few things: in what order to add ingredients, how to mix them, how to cook them, etc. The part about the order used to add them is covered by our list of dependencies in each app file. The `systools` application will be clever enough to look at the app files and figure out what needs to run before what.
现在有几个概念将进入这个阶段。食谱会告诉你一些事情：按什么顺序添加原料，如何混合，如何烹饪等等。每个应用程序文件中的依赖项列表涵盖了添加它们的顺序部分。“systools”应用程序将足够聪明，可以查看应用程序文件，并找出需要在什么之前运行什么。

Erlang's virtual machine can start itself with a basic configuration taken from something called a *boot file*. In fact, when you start your own `erl` application from the shell, it implicitly calls the Erlang Run Time System with a default boot file. That boot file will give basic instructions such as 'load the standard library', 'load the kernel application', 'run a given function' and so on. That boot file is a binary file created from something called a [boot script](http://www.erlang.org/doc/man/script.html), which contains tuples that will represent these instructions. We'll see how to write such a boot script.
Erlang的虚拟机可以从一个名为*引导文件的基本配置中启动*。事实上，当您从shell启动自己的“erl”应用程序时，它会使用默认启动文件隐式调用Erlang运行时系统。该启动文件将给出基本指令，如“加载标准库”、“加载内核应用程序”、“运行给定函数”等。该引导文件是一个二进制文件，由一个名为[boot script]的东西创建(http://www。二郎。org/doc/man/script。html），其中包含表示这些指令的元组。我们将了解如何编写这样的启动脚本。

First we start with:

```erl
,
 [
  ,
  ,
  .
  ,
  ...
```

Just kidding. Nobody really takes the time to do that and we won't either. The boot script is something easy to generate from the `.rel` file. Just start an Erlang VM from the `release/` directory and call the following line:
开玩笑。没有人真正花时间去做，我们也不会。引导脚本很容易从`。rel`file。只需从'release/'目录启动一个Erlang VM，并调用以下命令行：

```eshell
$ erl -env ERL_LIBS .
...
1> systools:make_script("erlcount-1.0", [local]).
ok
```

Now if you look in your directory, you will have a bunch of new files, including `erlcount-1.0.script` and `erlcount-1.0.boot` files. Here, the `local` option means that we want the release to be possible to run from anywhere, and not just the current install. There are a [bunch more options](http://www.erlang.org/doc/man/systools.html) to be seen, but because systools isn't as powerful as `reltool` (in the next sections), we won't look into them with too much depth.
现在，如果你查看目录，你会有一堆新文件，包括'erlcount-1'。0。脚本`和`计数-1。0。启动文件。在这里，“local”选项意味着我们希望这个版本可以在任何地方运行，而不仅仅是当前的安装。还有[更多选择](http://www。二郎。org/doc/man/systools。html），但由于systools不如“reltool”（在接下来的部分中）强大，我们不会深入研究它们。

In any case, we have the boot script, but not enough to distribute our code yet. Get back to your Erlang shell and run the following command:
无论如何，我们有启动脚本，但还不足以分发代码。返回Erlang shell并运行以下命令：

```eshell
2> systools:make_tar("erlcount-1.0", []).
ok
```

Or, on Windows 7:

```eshell
2> systools:make_tar("erlcount-1.0", []).
ok
```

Here, systools will look for your release files and the Erlang Run Time System (because of the `erts` option). If you omit the `erts` option, the release won't be self-executable and will depend on the presence of Erlang already being installed on a system.
在这里，systools将查找您的发布文件和Erlang运行时系统（因为“erts”选项）。如果省略“erts”选项，该版本将不会自动执行，并且将取决于系统上是否安装了Erlang。

Running the function call above will have created an archive file named `erlcount-1.0.tar.gz`. Unarchive the files inside and you should see a directory like this:
运行上述函数调用将创建一个名为“erlcount-1”的存档文件。0。焦油。广州`。取消归档里面的文件，你会看到这样一个目录：

``` expand
erts-5.8.4/
lib/
releases/
```

The `erts-5.8.4/` directory will contain the run time system. The `lib/` directory holds all the applications we need and `releases` has the boot files and whatnot.
erts-5。8.。4/`目录将包含运行时系统。“lib/”目录包含我们需要的所有应用程序，“releases”包含启动文件等等。

Move into the directory where you extracted these files. From there, we can build a command line call for `erl`. First of all, we specify where to find the `erl` executable and the boot file (without the `.boot` extension). In Linux, this gives us:
移动到解压缩这些文件的目录。我们可以从那里呼叫命令行`。首先，我们指定在哪里找到“erl”可执行文件和引导文件（不带`。启动（扩展）。在Linux中，这为我们提供了：

```eshell
$ ./erts-5.8.4/bin/erl -boot releases/1.0.0/start
```

The command is the same for me on Windows 7, using Windows PowerShell.

::: 
**Don't Drink Too Much Kool-Aid:**\
There is no guarantee that a release will work on any system ever. If you're using pure Erlang code, then that code will be portable. The issue is that the ERTS you ship with it might itself not work: you will either need to create many binary packages for many different platforms for large-scale definition, or just ship the BEAM files without the associated ERTS and ask people to run them with an Erlang system they have on their own computer.
目前还不能保证发行版能在任何系统上运行。如果您使用的是纯Erlang代码，那么该代码将是可移植的。问题是，随附的ERT本身可能不起作用：您要么需要为许多不同的平台创建许多二进制软件包，以便进行大规模定义，要么只需在没有相关ERT的情况下发送BEAM文件，并要求人们使用他们自己计算机上的Erlang系统运行它们。
:::

You can optionally use absolute paths if you want the command to work from anywhere on your computer. Don't run it right now, though. It's going to be useless because there is no source file to analyse in the current directory. If you used absolute paths, you can go to the directory you want to analyse and call the file from there. If you used relative paths (as I did) and if you recall our `erlcount` application, we made it possible to configure what directory the code will be scanning. Let's add `-erlcount directory "'<path to the directory>'"` to the command. Then because we want this not to look like Erlang, let's add the `-noshell` argument. This gives me something like this on my own computer:
如果希望命令在计算机上的任何位置工作，可以选择使用绝对路径。不过，现在不要运行它。它将毫无用处，因为当前目录中没有要分析的源文件。如果使用绝对路径，可以转到要分析的目录并从那里调用文件。如果您使用了相对路径（正如我所做的），并且回忆起我们的“erlcount”应用程序，我们就可以配置代码将扫描的目录。让我们将“-erlcount directory”<path to the directory>'”添加到命令中。因为我们不想让它看起来像Erlang，所以让我们添加“-noshell”参数。这在我自己的电脑上给了我类似的东西：

```eshell
$ ./erts-5.8.4/bin/erl -boot releases/1.0.0/start -erlcount directory '"/home/ferd/code/otp_src_R14B03/"' -noshell
$。/erts-5。8.。4/bin/erl-启动释放/1。0。0/start-erlcount目录'/home/ferd/code/otp_src_R14B03/“'-noshell
Regex if\s.+-> has 3846 results
Regex case\s.+\sof has 55894 results
```

Using absolute file paths, I get something like this:

```eshell
$ /home/ferd/code/learn-you-some-erlang/release/rel/erts-5.8.4/bin/erl -boot /home/ferd/code/learn-you-some-erlang/release/rel/releases/1.0.0/start -noshell
$/home/ferd/code/了解一些erlang/release/rel/erts-5。8.。4/bin/erl-boot/home/ferd/code/了解一些erlang/release/rel/releases/1。0。0/开始-noshell
```

Wherever I run it from, that's the directory that's going to be scanned. Wrap this up in a shell script or a batch file and you should be good to go.
无论我从哪里运行它，这都是要扫描的目录。将其包装在shell脚本或批处理文件中，就可以开始了。

## [Releases With Reltool]

There are a bunch of annoying things with systools. We have very little control about how things are done, and frankly, running things as they are there is a bit annoying. Manually specifying the the path to the boot file and whatnot is kind of painful. Moreover, the files are a bit large. The whole release takes over 20mb on disk, and it would be a lot worse if we were to package more applications. It is possible to do better with `reltool` as we get a lot more power, although the tradeoff is increased complexity.
systools有很多令人讨厌的东西。我们几乎无法控制事情是如何完成的，坦率地说，按现状运行有点烦人。手动指定引导文件的路径等等都有点痛苦。而且，文件有点大。整个版本占用了超过20mb的磁盘空间，如果我们要打包更多的应用程序，情况会糟糕得多。使用“reltool”可以做得更好，因为我们可以获得更多的功能，尽管代价是增加了复杂性。

Reltool works from a [config file that looks like this](static/erlang/release/erlcount-1.0.config.html):
Reltool从一个如下所示的[config文件]（static/erlang/release/erlcount-1）开始工作。0。配置。html）：

```erl
{sys, [
    ,
    {rel, "erlcount", "1.0.0",
     [kernel,
      stdlib,
      ,
      
     ]},
    ,
    ,
    ,
    ,
                  ,
                  ,
    ,
                     ,
                     ,
                     
]}.
```

Behold the user friendliness of Erlang! To be quite honest, there's no easy way to introduce ourselves to Reltool. You need a bunch of these options at once or nothing will work. It might sound confusing, but there's a logic behind it.
看看Erlang的用户友好性吧！说实话，向你介绍自己并不容易。你需要同时有一大堆这样的选项，否则什么都不管用。这听起来可能令人困惑，但背后有一个逻辑。

First of all, Reltool will take different levels of information. The first level will contain release-wide information. The second level will be application-specific, before allowing fine-grained control at a module-specific level:
首先，Reltool将获取不同级别的信息。第一级将包含发布范围的信息。第二个级别是特定于应用程序的，然后允许在特定于模块的级别进行细粒度控制：

![The levels of reltools: the release levels contains environment, applications and properties of the releases. The level under that, Applications, contains what to include, compression, debug_info, app files, etc. The last (and lowest) level, the Modules, contains what to include and debug_info](../img/reltool-levels.png)
![reltools的级别：发布级别包含环境、应用程序和发布的属性。该级别下的应用程序包含要包含的内容、压缩、调试信息、应用程序文件等。最后一级（也是最低一级）模块包含要包含的内容和调试信息(。。/静态/img/REL工具级别。（巴布亚新几内亚）

For each of these levels, as in the previous graph, different options will be available. Rather than taking the encyclopedic approach with all the options possible, we'll rather visit a few essential options and then a few possible configurations depending on what you might be looking for in your app.
对于这些级别中的每一个，如上图所示，将提供不同的选项。与其采用百科全书式的方法，提供所有可能的选项，不如先访问一些基本选项，然后再访问一些可能的配置，具体取决于你可能在应用程序中寻找的内容。

The first option is one that helps us get rid of the somewhat annoying need to be sitting in a given directory or to set the correct `-env` arguments to the VM. The option is `lib_dirs` and it takes a list of directories where applications are sitting. So really, instead of adding `-env ERL_LIBS <list of directories>`, you put in `` and you get the same result.
第一个选项可以帮助我们摆脱坐在给定目录中或为VM设置正确的`-env`参数的烦人需求。该选项是“lib_dirs”，它包含应用程序所在的目录列表。所以，实际上，不是添加“-env ERL_LIBS<list of directories>”，而是添加“”，得到了相同的结果。

Another vital option for the Reltool configuration files is `rel`. This tuple is very similar to the `.rel` file we had written for `systools`. In the demo file above, we had:
Reltool配置文件的另一个重要选项是“rel”`。这个元组与`。rel`我们为'systools'编写的文件`。在上面的演示文件中，我们有：

```erl
{rel, "erlcount", "1.0.0",
 [kernel,
  stdlib,
  ,
  
 ]},
```

And that's what we'll need to tell us what apps need to be started correctly. After that tuple, we want to add a tuple of the form:
这就是我们需要告诉我们哪些应用程序需要正确启动的原因。在该元组之后，我们要添加以下形式的元组：

```erl

```

This will tell Reltool that whenever someone runs the `erl` binary included in the release, we want the apps from the `erlcount` release to be started. With just these 3 options (`lib_dirs`, `rel` and `boot_rel`), we can get a valid release.
这将告诉Reltool，每当有人运行版本中包含的`erl`二进制文件时，我们都希望启动`erlcount`版本中的应用程序。只要有这三个选项（`lib_dirs`、`rel`和`boot_rel`），我们就可以得到一个有效的版本。

To do so, we'll put these tuples into a format Reltool can parse:

```erl
{sys, [
    ,
    {rel, "erlcount", "1.0.0",
     [kernel,
      stdlib,
      ,
      
     ]},
    
]}.
```

Yeah, we just wrap them into a `` tuple. In my case, I saved this in a file named `erlcount-1.0.config` in the `release/` directory. You might put it anywhere you want (except `/dev/null`, even though it's got exceptional write speeds!)
是的，我们只是把它们包装成一个“元组”。在我的例子中，我将其保存在一个名为“erlcount-1”的文件中。0。config`release/`目录中的`。你可以把它放在任何你想放的地方（除了“/dev/null”，尽管它的写入速度非常快！）

Then we'll need to open an Erlang shell:

```eshell
1>  = file:consult("erlcount-1.0.config").
,
           {rel,"erlcount","1.0.0",
                [kernel,stdlib,,
           
2>  = reltool:get_target_spec(Conf).
{ok,[{create_dir,"releases",
   ...
3> reltool:eval_target_spec(Spec, code:root_dir(), "rel").
ok
```

The first step here is to read the config and bind it to the `Conf`.

The third command takes the spec and tells Reltool 'I want you to take my release specification, using whatever path where my Erlang installs are, and shove it into the \"rel\" directory'. That's it. Look into the `rel` directory and you should have a bunch of subdirectories there.
第三个命令获取规范，并告诉Reltool“我希望您使用我的Erlang安装所在的任何路径获取我的发布规范，并将其放入\“rel\”目录”。就这样。查看一下'rel'目录，你应该有很多子目录。

For now we don't care and can just call:

```eshell
$ ./bin/erl -noshell
Regex if\s.+-> has 0 results
Regex case\s.+\sof has 0 results
```

Ah, a bit simpler to run. You can put these files pretty much anywhere, as long as they keep the same file tree and run them from wherever you want.
啊，运行起来简单一点。你可以把这些文件放在几乎任何地方，只要它们保持相同的文件树，并在你想要的任何地方运行它们。

![a squid's tentacle being cut off so it could free itself from a pair of handcuffs](../img/cuffs.png "I'm not exactly sure how this worked for a squid?")
![一只乌贼的触须被切断，这样它就可以摆脱一副手铐](。。/静电/img/袖口。png“我不确定这对鱿鱼是怎么起作用的？”）

Have you noticed something different? I hope you have. We didn't need to specify any version numbers. Reltool is a bit more clever than Systools there. If you do not specify a version, it will automatically look for the newest one possible in the paths you have (either in the directory returned by `code:root_dir()` or what you put in the `lib_dirs` tuple).
你有没有注意到一些不同的东西？我希望你有。我们不需要指定任何版本号。Reltool比Systools聪明一点。如果您没有指定版本，它将自动在您拥有的路径中查找可能的最新版本（或者在`code:root_dir（）`返回的目录中，或者在`lib_dirs`元组中输入的目录中）。

But what if I'm not hip and cool and trendy and all about the latest apps, but rather a retro lover? I'm still wearing my disco pants in here and I want to use older ERTS versions and older library versions, you see (I've never stayed more alive than I was in 1977!)
但是，如果我不是一个时髦、酷、时尚的人，不是一个最新的应用程序，而是一个复古爱好者呢？我想在我的图书馆里看到比1977年更老的迪斯科版本

Thankfully, Reltool can handle releases that need to work with older versions of Erlang. Respecting your elders is an important concept for Erlang tools.
谢天谢地，Reltool可以处理需要使用旧版本Erlang的版本。尊重长辈是Erlang工具的一个重要概念。

If you have older versions of Erlang installed, you can add an `` entry to the config file:

```erl
{sys, [
    ,
    ,
    {rel, "erlcount", "1.0.0",
     [kernel,
      stdlib,
      ,
      
     ]},
    
]}.
```

Now, you want to clear out the `rel/` directory to get rid of the newer release. Then you run the rather ugly sequence of calls again:
现在，您需要清除'rel/'目录以删除较新的版本。然后你再次运行一系列相当丑陋的调用：

```eshell
4> f(),
4>  = file:consult("erlcount-1.0.config"),
4>  = reltool:get_target_spec(Conf),
4> reltool:eval_target_spec(Spec, code:root_dir(), "rel").
ok
```

A quick reminder here, `f()` is used to unbind the variables in the shell. Now if I go to the `rel` directory and call `$ ./bin/erl`, I get the following output:
这里有一个快速提示，`f（）`用于解除shell中变量的绑定。现在如果我去'rel'目录打电话`$。/bin/erl`，我得到以下输出：

```eshell
Erlang R14B02 (erts-5.8.3) [source] ...

Eshell V5.8.3  (abort with ^G)
1> Regex if\s.+-> has 0 results
Regex case\s.+\sof has 0 results
```

Awesome. This runs on version 5.8.3 even though I've got newer ones available. Ah, ha, ha, ha, Stayin' alive.
令人惊叹的。这在版本5上运行。8.。3尽管我有新的。啊，哈，哈，哈，活着。

::: note
**Note:** if you look at the `rel/` directory, you'll see things are kind of similar to what they were with Systools, but one of the difference will be in the `lib/` directory. That one will now contain a bunch of directories and `.ez` files. The directories will contain the `include/` files required when you want to do development and the `priv/` directories when there are files that need to be kept there. The `.ez` files are just zipped beam files. The Erlang VM will unpack them for you come runtime, it's just to make things lighter.
**注意：*如果你看一下'rel/'目录，你会发现它们与Systools有点类似，但其中一个区别是'lib/'目录。现在，它将包含一组目录和`。ez'文件。这些目录将包含进行开发时所需的'include/`文件，以及需要保存文件时所需的'priv/`目录。那个`。ez'文件只是压缩的光束文件。Erlang虚拟机将在运行时为您解包，这只是为了让事情变得更轻松。
:::

But wait, what about my other modules?

Ah now we move away from the release-wide settings and have to enter the settings that have to do with applications. There are still a lot of release-wide options to see, but we're on such a roll that we can't be asked to stop right now. We'll revisit them in a while. For applications, we can specify versions by adding more tuples:
啊，现在我们离开了整个版本的设置，必须输入与应用程序有关的设置。还有很多发布范围内的选项可供选择，但我们的进展如此之快，以至于我们现在不能被要求停止。我们过一会儿再去看看。对于应用程序，我们可以通过添加更多元组来指定版本：

```erl

```

And put in one per app that needs it.

Now we have way more options for everything. We can specify if we want the release to include debug info, strip it away, try to make more compact app files or trust us with our definitions, stuff to include or exclude, how strict to be when it comes to including applications and modules on which your own applications might depend, etc. Moreover, these options can usually be defined both release-wide and application-wide so you can specify defaults and then values to override.
现在我们有了更多的选择。我们可以指定是否希望版本包含调试信息、删除它、尝试制作更紧凑的应用程序文件，或者信任我们的定义、要包含或排除的内容、在包含应用程序和模块时的严格程度，这些应用程序和模块可能依赖于您自己的应用程序，等等。此外，这些选项通常可以在版本范围和应用程序范围内定义，因此您可以指定默认值，然后指定要覆盖的值。

Here's a quick rundown. If you find it complex, just skip over it and you'll see a few cookbook recipes to follow:
下面是一个简短的概述。如果你觉得它很复杂，跳过它，你会看到一些食谱如下：

### Release-only options


:   What directories to look inside for libraries.


:   Added in R15B02, this option lets you specify OTP applications as part of your release, without including whatever comes from the standard Erlang/OTP path in the final release. This lets you create releases that are essentially libraries bootable from an existing virtual machine installed in a given system. When using this option you must now start the virtual machine as `$ erl -boot_var RELTOOL_EXT_LIB <path to release directory>/lib -boot <path to the boot file>`. This will allow the release to use the current Erlang/OTP install, but with your own libraries for your custom release.
：添加在R15B02中，此选项允许您在发布中指定OTP应用程序，而无需在最终发布中包含来自标准Erlang/OTP路径的任何内容。这允许您创建基本上可以从安装在给定系统中的现有虚拟机启动的版本。使用此选项时，现在必须以“$erl-boot\u var RELTOOL\u EXT\u LIB<path to release directory>/LIB-boot<path to the boot file>`。这将允许该版本使用当前的Erlang/OTP安装，但要使用自定义版本的库。


:   Will let you specify application-wide options, usually more specific than the release-wide options.
：将允许您指定应用程序范围的选项，通常比版本范围的选项更具体。


:   Default release to boot with the `erl` executable. This means we won't need to specify the boot file when calling `erl`.
：使用“erl”可执行文件启动的默认版本。这意味着我们在调用'erl'时不需要指定启动文件`。


:   The applications to be included in the release.


:   It is possible to run the release from everywhere or only from a hard coded path in your system. By default it's set to `true` and I tend to leave it that way unless there is a good reason to do otherwise. You'll know when you need it.
：可以从任何地方或仅从系统中的硬编码路径运行版本。默认情况下，它被设置为'true'，我倾向于保持这种状态，除非有充分的理由这样做。你会知道什么时候需要它。


:   This option will act as a way to specify default `*_filters` (described below) based on your type of release. By default, `development` is used. That one will include more files from each app and ERTS blindly. The `standalone` profile will be more restrictive, and the `embedded` profile even more so, dropping more default ERTS applications and binaries.
：此选项将根据您的发布类型指定默认的“*_过滤器”（如下所述）。默认情况下，使用“development”。这将盲目地包含来自每个应用程序和ERT的更多文件。“独立”配置文件将更加严格，而“嵌入式”配置文件将更加严格，删除更多默认的ERTS应用程序和二进制文件。

### Release and Application-wide Options

Note that for all of these, setting the option on the level of an application will simply override the value you gave at a system level.
请注意，对于所有这些，在应用程序级别设置选项只会覆盖您在系统级别给出的值。

\

:   Checks whether a file matches the include filters without matching the exclude filters before including it. You might drop or include specific files that way.
：在包含文件之前，检查文件是否与包含筛选器匹配，而不与排除筛选器匹配。您可以通过这种方式删除或包含特定文件。

\

:   Similar to `incl_sys_filters` and `excl_sys_filters`, but for application-specific files

\

:   Specified what top-level directories have to be included or excluded into `.ez` archive files (more on this soon).
：指定必须包含或排除的顶级目录`。ez'存档文件（很快会有更多相关信息）。


:   Decides how to include applications not necessarily specified in the `rel` tuple. Picking `include` means that Reltool will include pretty much everything it can find. Picking `derived` means that Reltool will only include applications that it detects can be used by any of the applications in your `rel` tuple. This is the default value. Picking `exclude` will mean that you will include no apps at all by default. You usually set this on a release-level when you want minimal includes, and then override it on an application-by-application basis for the stuff you feel like adding.
：决定如何包含不一定在'rel'元组中指定的应用程序。选择“include”意味着Reltool将包含它能找到的几乎所有内容。选择'derived'意味着Reltool将只包含它检测到的可由'rel'元组中的任何应用程序使用的应用程序。这是默认值。选择“排除”将意味着默认情况下您将不包括任何应用程序。当你想要最小的包含时，你通常在一个发布级别上设置它，然后在一个应用程序一个应用程序的基础上覆盖它，以获得你想要添加的内容。


:   This controls the module inclusion policy. Picking `none` means no modules will be kept. That's not very useful. The `derived` option means that Reltool will try to figure out what modules are used by other modules which are already included and add them in that case. Setting the option to `app` will mean that Reltool keeps all the modules mentioned in the app file and those that were derived. Setting it to `ebin` will keep those in the `ebin/` directory and the derived ones. Using the option `all` will be a mix of using `ebin` and `app`. That's the default value.
：这控制模块包含策略。选择“无”意味着不会保留任何模块。这不是很有用。“派生”选项意味着Reltool将尝试找出哪些模块被已经包含的其他模块使用，并在这种情况下添加它们。将选项设置为“app”将意味着Reltool保留app文件中提到的所有模块以及派生的模块。将其设置为“ebin”将保留“ebin/”目录中的内容和派生内容。使用'all'选项将混合使用'ebin'和'app'`。这是默认值。


:   This option manages how the app files are going to be managed for you when you include an application. Picking `keep` will guarantee that the app file used in the release is the same one you wrote for your application. That's the default option. If you choose `strip`, Reltool will try to generate a new app file that removes the modules you don't want in there (those that were excluded by filters and other options). Choosing `all` will keep the original file, but will also add specifically included modules in there. The nice thing with `all` is that it can generate app files for you if none are available.
：此选项管理在包含应用程序时如何为您管理应用程序文件。选择“keep”将保证发布中使用的应用程序文件与您为应用程序编写的文件相同。这是默认选项。如果您选择'strip'，Reltool将尝试生成一个新的应用程序文件，删除您不想要的模块（被过滤器和其他选项排除的模块）。选择“all”将保留原始文件，但也会在其中添加专门包含的模块。“all”的好处是，如果没有可用的应用程序文件，它可以为您生成应用程序文件。

### Module-specific Options


:   This lets you override the `mod_cond` option defined at the release level and application level.

### All-levels Options

These options work on all levels. The lower the level, the more precedence it takes.


:   Assuming your files were compiled with `debug_info` on (which I suggest you do), this option lets you decide whether to keep it or drop it. The `debug_info` is useful when you want to decompile files, debug them, etc. but will take some space.
：假设你的文件是用“debug_info”编译的（我建议你这样做），这个选项让你决定是保留还是删除它。“debug_info”在您想要反编译文件、调试文件等时非常有用。但会占用一些空间。

### THAT'S DENSE

Oh yes it is a lot of information. I didn't even cover all the possible options, but that's still a decent reference. If you want the whole thing, check out the [official doc](http://www.erlang.org/doc/man/reltool.html).
哦，是的，这是很多信息。我甚至没有涵盖所有可能的选项，但这仍然是一个不错的参考。如果你想了解整件事，请查看[官方文件](http://www。二郎。org/doc/man/reltool。（html）。

![A complex Rube Goldberg machine to represent the OTP Release process](../img/rube.png)

## [Recipes]

For now we'll have a few general tips and tricks of things to do depending on what you want to obtain.
现在，我们将根据您想要获得的内容，提供一些一般性的提示和技巧。

### Development versions

Getting something for development has to be relatively easy. Often the defaults are good enough. Stick to getting the basic items we'd seen before in place and it should be enough:
为发展做点什么必须相对容易。通常，默认值已经足够好了。坚持把我们以前见过的基本物品准备好，这就足够了：

```erl
{sys, [
    ,
    ,
    
]}.
```

Reltool will take care about importing enough to be fine. In some cases, you might want to have everything from a regular VM. You might be distributing an entire VM for a team, with some libraries included. In that case, what you want to do is something more like this:
Reltool会注意导入足够多的内容。在某些情况下，您可能希望拥有普通VM的所有功能。您可能正在为一个团队分发一个完整的VM，其中包括一些库。在这种情况下，你想做的事情更像这样：

```erl
{sys, [
    ,
    ,
    ,
    
]}.
```

By setting `incl_cond` to `include`, all applications found in the current ERTS install and the `lib_dirs` will be part of your release.
通过将“incl_cond”设置为“include”，当前ERTS安装和“lib_dirs”中找到的所有应用程序都将成为您发布的一部分。

::: note
**Note:** when no `boot_rel` is specified, you have to have a release named `start_clean` for reltool to be happy. That one will be picked by default when you start the associated `erl` file.
**注：*当没有指定“boot_rel”时，你必须有一个名为“start_clean”的版本，reltool才会高兴。默认情况下，启动关联的“erl”文件时会选择该文件。
:::

If we want to exclude a specific application, let's say `megaco` because I never looked into it, we can instead get a file like this:
如果我们想排除一个特定的应用程序，比如说“megaco”，因为我从未研究过它，我们可以得到这样一个文件：

```erl
{sys, [
    ,
    ,
    ,
    ,
    
]}.
```

Here we can specify one or more applications (each having its own `app` tuple), and each of them overrides the `incl_cond` setting put at the release level. So in this case, we will include everything except megaco.
在这里，我们可以指定一个或多个应用程序（每个应用程序都有自己的'app'元组），并且每个应用程序都会覆盖发布级别的'incl_cond'设置。所以在本例中，我们将包括除megaco之外的所有内容。

### Only importing or exporting part of a library

In our release, one annoying thing that happened was that apps like `ppool` and others, even though they didn't need them, also kept their test files in the release. You can see them by going into `rel/lib/` and unzipping `ppool-1.0.0.ez` (you might need to change the extension first).
在我们的版本中，一件令人恼火的事情发生了，像“ppool”和其他应用程序，即使它们不需要它们，也在版本中保留了它们的测试文件。通过进入'rel/lib/'并解压缩'ppool-1，可以看到它们。0。0。ez`（可能需要先更改扩展名）。

To get rid of these files, the easiest way to do it is specify exclusion filters such as:

```erl
{sys, [
    ,
    ,
    
]}.
```

When you want to only import specific files of an application, let's say our `erlcount_lib` for its functionality, but nothing else from there, things get a bit more complex:
如果只想导入应用程序的特定文件，比如我们的“erlcount_lib”的功能，而不想导入其他文件，那么事情就变得更复杂了：

```erl
{sys, [
    ,
    ,
    , % exclude would also work, but not include
    ,
                     
]}.
```

In this case, we switched from `` to more restrictive `incl_cond`s. This is because if you go large and rake everything is, then the only way to include a single lib is to exclude all the others with an `excl_app_filters`. However, when our selection is more restrictive (in this case we're `derived` and wouldn't include `erlcount` because it's not part of the `rel` tuple), we can specifically tell the release to include the `erlcount` app with only files that match the regular expression having to do with `erlcount_lib`. This prompts the question as to how to make the most restrictive application possible, right?
在本例中，我们从``切换到了更严格的``包括条件`。这是因为如果你把所有的东西都放大，那么包含一个库的唯一方法就是用一个“excl_app_过滤器”排除所有其他库`。然而，当我们的选择更加严格时（在本例中，我们是“派生”的，不包括“erlcount”，因为它不是“rel”元组的一部分），我们可以明确地告诉发行版只包含与“erlcount_lib”正则表达式相匹配的文件`。这就引发了一个问题，即如何使最严格的应用成为可能，对吗？

### Smaller Apps For Programmers With Big Hearts

This is where Reltool becomes a good bit more complex, with a [rather verbose configuration file](static/erlang/release/erlcount-sm.config.html):
这就是Reltool变得更加复杂的地方，它有一个[相当详细的配置文件]（static/erlang/release/erlcount-sm）。配置。html）：

```erl
{sys, [
    ,
    ,
            ,
    ,
    ,
    ,
    , 
    ,
    ,
    ,
    ,
    ,
    ,
    ,
    
]}.
```

Oh, a lot more stuff going on. We can see that in the case of `erts`, we ask for Reltool to keep only what's necessary in there. Having `mod_cond` to `derived` and `app_file` to `strip` will ask Reltool to check and only keep what's used for something else. That's why `` is also used on the release level.
哦，还有很多事情要做。我们可以看到，在“erts”的情况下，我们要求Reltool只保留必要的内容。将'mod_cond'改为'derived'并将'app_file'改为'strip'将要求Reltool检查并仅保留用于其他用途的内容。这就是为什么在发布级别上也使用``的原因。

![a crate with a sign that sayz '.ez'](../img/ez.png)

The profile is set to `embedded`. If you looked at the `.ez` archives in the previous cases, they contained the source files, test directories, etc. When switching over to `embedded` only include files, binaries and the `priv/` directories are kept. I'm also removing `debug_info` from all files, even if they were compiled with it. This means we're going to lose some debugging ability, but it will reduce the size of files.
配置文件设置为“嵌入式”`。如果你看看`。在以前的案例中，它们包含源文件、测试目录等。当切换到“embedded”时，只包含文件、二进制文件和“priv/”目录。我还将从所有文件中删除'debug_info'，即使它们是用它编译的。这意味着我们将失去一些调试能力，但它会减少文件的大小。

I'm still stripping away test files, and setting things so that no application is included until explicitly told to be (`` so that the minimal files of some applications are what is kept.
我仍在剥离测试文件，并进行设置，以便在明确要求之前不包含任何应用程序（``以便保留一些应用程序的最小文件）。

What's the difference in the end? Some of our more general releases would weigh in at over 35MB. The one described above is reduced to less than 20MB. We're shaving a good part of it. The size is still fairly large though. That's because of ERTS, which itself takes 18.5MB. If you want to, you can dig deeper and really micro manage how ERTS is built to get something smaller. You can alternatively pick some binary files in the ERTS that you know won't be used by your application: executables for scripts, remote running of Erlang, binaries from test frameworks, different running commands (Erlang with or without SMP, etc.)
最后有什么不同？我们的一些更通用的版本将超过35MB。上面描述的一个被减少到小于20MB。我们剃掉了很大一部分。不过尺寸还是相当大。这是因为ERTS本身需要18分钟。5MB。如果你想的话，你可以更深入、更微观地管理ERTS是如何构建的，以获得更小的产品。您也可以在ERT中选择一些您知道应用程序不会使用的二进制文件：脚本的可执行文件、远程运行Erlang、来自测试框架的二进制文件、不同的运行命令（带有或不带有SMP的Erlang，等等）。)

The lightest release will be the one that assumes that the other user has Erlang installed already---when you pick this option, you need to add the `rel/` directory's content as part of your `ERL_LIBS` environment variable and call the boot file yourself (a bit like with systools), but it'll work. Programmers might want to wrap this up in scripts to get things going.
最轻的版本是假设另一个用户已经安装了Erlang的版本——当你选择这个选项时，你需要添加'rel/`directory'的内容作为'ERL_LIBS'环境变量的一部分，并自己调用启动文件（有点像systools），但它可以工作。程序员可能想用脚本来完成这项工作。

::: note
**Note:** these days, Erlang programmers seem to really love the idea of having all these releases handled for you by a tool called *rebar3*. Rebar3 will act as a wrapper over the Erlang compiler and handle releases. There is no loss in understanding how Reltool works---Rebar3 uses a higher level abstraction for releases, and understanding reltool makes it easy to understand how any other tool works.
**注意：*现在，Erlang程序员似乎真的很喜欢用一个名为*rebar3的工具来处理所有这些发布*。Rebar3将充当Erlang编译器的包装器，并处理发布。理解Reltool的工作原理并没有什么损失——Rebar3对发布使用了更高级别的抽象，而理解Reltool使理解任何其他工具的工作原理变得容易。
:::

## [Released From Releases]

Well, that's it for the two major ways to handle releases. It's a complex topic, but a standard way to handle things. Applications might be enough for many readers and there's nothing bad in sticking to them for a good while, but now and then releases might be useful if you want your Operations and Maintenance guy to like you a bit better given you know (or at least have some idea on) how to deploy Erlang applications when you need to.
这就是处理发布的两种主要方法。这是一个复杂的话题，但却是处理事情的标准方式。对于很多读者来说，应用程序可能已经足够了，长期使用这些应用程序并没有什么坏处，但是如果你想让你的操作和维护人员更喜欢你，那么时不时的发布可能会很有用，因为你知道（或者至少对如何在需要时部署Erlang应用程序有一些了解）。

Of course, what could make your Operations guy happier than no down time? The next challenge will be to do software upgrades while a release is running.
当然，有什么能比没有休息时间更让你的运营人员开心呢？下一个挑战是在发行版运行时进行软件升级。

![Parody of the poster of the Grease movie, where 'Grease' is replaced by 'Release', Olivia Newton-Jogn by Joe Armstrong and John Travolta by Bjarne Dacker](../img/release.png "Grease is the time, is the place is the motion Grease is the way we are feeling")
!【模仿《油脂》电影的海报，其中《油脂》被《释放》取代，乔·阿姆斯特朗的奥利维亚·牛顿·乔恩和比亚恩·达克的约翰·特拉沃尔塔】(。。/静态/图像/释放。png“润滑脂是时间，是地点是运动润滑脂是我们的感觉”）
