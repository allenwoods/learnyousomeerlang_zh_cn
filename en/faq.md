# FAQ

Isn't LYSE just Learn You a Haskell but with Erlang? Can't you get your own ideas?

Yes, that's in fact what it is. LYAH for Erlang. I e-know Miran Lipovaca and I even asked him the permission to do that site. Read the first chapter, it's explained in there! To answer the second question, it appears that no, I can't get my own ideas. Even this FAQ is more or less copied on his.

Did you draw the pictures yourself?

Yep. Props to my girlfriend for making the site's design, though.

Do you recommend any other Erlang reading material?

Of course. There's some free content on the web, like the official [Getting started](http://www.erlang.org/starting.html) guides. They give a good overview, even though the information can be a bit outdated. You can also google for blogs helping you out, but the signal to noise ratio is relatively low for beginners' tutorial (in my opinion).

For stuff you can pay for, take a look at Joe Armstrong's [Programming Erlang: Software for a Concurrent World](http://pragprog.com/titles/jaerlang/programming-erlang). Joe Armstrong's one of the creators of Erlang and gives a pretty good description of the language and why it was thought the way it is. Then you should read [Erlang Programming](http://oreilly.com/catalog/9780596518189/) from Francesco Cesarini and Simon Thompson. It's almost a continuation of Joe Armstrong's book in the sense that it will tell you more about Erlang's environment and how to make complete safe, reliable and scalable applications. For more advanced Erlang material (especially OTP), [Erlang and OTP in Action](http://manning.com/logan/) is a good source of information.

Can I get this in the form of a real book?

Thanks to No Starch Press, Learn You Some Erlang is available both as a [dead tree book and an ebook](http://nostarch.com/erlang)! At a large 600 black and white pages, including images (in color for ebook copies), you can now have the largest Erlang-themed paperweight and bookcase decoration printed to date (as far as I know). This should ease the sharp pain of reading hundreds of pages on a computer screen.

About the author

My name is Frederic Trottier-Hebert (oh please call me Fred), I'm a French-Canadian, living in Quebec. If you want to contact me directly, send me an e-mail to mononcqc+lyse at ferd.ca. I have a blog at [ferd.ca](http://ferd.ca/), and currently spend my time working for BLOOM Digital Platforms.

I love to talk about web standards, tell me about web standards!

Alright. This question has (obviously) never been asked but I felt like having a place to talk about the web standards I used for this site as my day job concerns web programming a lot.

This site has been tested in Opera 10, FF3.0, FF3.5, IE7, IE8, Chrome, Safari, Elinks, Lynx, Opera mobile, Opera's accessibility layout and a Nokia N810. I've also made sure the site looks okay when printed and that it degrades well without JavaScript or images. I still haven't tested it for audio readers, but I plan on doing that sooner or later. I haven't tested IE6 and I don't plan to do so. If all the testing I've done for what's above is still not good enough for IE6, the problem is probably IE6.

I hope the effort will mean everybody will have clean looking content, although if you want me to support some additional software or device, I'll try to do so (with the exception of IE6). Yes, I pretty much dislike IE6 and decided not to support it out of principle. Sorry for you guys stuck at work with IE6 only :(

Did you do all of this alone?

Yes and no. As said above, the concept comes from Miran, the site design from my girlfriend, and I've had the help of a lot of e-people who accepted to review my texts, fix my grammar and syntax mistakes, correct me on a few facts and suggested content for me to add. Here's a 'thank you' to some of them: [OJ Reeves](http://buffered.io), [Michael Richter](https://plus.google.com/104854939887541782231/), Dave Pawson, Robert Virding, Richard O'Keefe, Ulf Wiger, Lukas Larsson, [Dale Harvey](http://erldocs.com "yeah, he totally owns that site"), Richard Carlsson, [Nick Fitzgerald](http://fitzgeraldnick.com), [Brendon Hogger](https://github.com/brendonh/), [Geoff Cant](http://www.linkedin.com/in/geoffcant "Geoff is a pretty awesome guy and Erlang programmer and always helpful"), [Andrew Thompson](http://github.com/Vagabond), Bartosz Fabianowski, [Richard Jones](http://www.metabrew.com), Tuncer Ayaz, [William King](http://www.linkedin.com/pub/william-king/14/11a/9b7), [Mahesh Paolini-Subramanya](http://dieswaytoofast.blogspot.com), and a bunch more from IRC (ie.: [orbitz](http://functional-orbitz.blogspot.com), who didn't want to tell me his real name). Thanks guys! (if I have forgotten to include you (which means you're not in my inbox at the moment) and want your name added to the list, just tell me, it'll be my pleasure.)

I want to read Learn You Some Erlang offline!

That's not a question. In any case, I never had the time to do it, and when a few people sent me PDFs version, they were never fully maintained. In any case, there is a script somewhere on github to help you turn this site into a Kindle book (which I can't openly advertise due to a publishing deal!). For personal copies, I can recommend using [wget](http://www.gnu.org/software/wget/) to download your own HTML copy of the site. More precisely, use `wget --mirror -k -E http://learnyousomeerlang.com`. You should then have a local copy of the site that can be moved and whatnot.

Can you open source the material?

It is open-sourced. Believe it or not, except for one very basic and ugly draft copy of the site in a .txt file, the whole thing is written straight in HTML with the help of a few Vim macros. So right click in your browser, pick 'view source' and you've got the whole thing (except a few templating rules and variables for links).

What do you use to write LYSE?

As explained (partially) in the answer above, I begin with a bland .txt file. Everything is written flat---I don't want to be annoyed with markup. Once this is done, I copy/paste the site's directory in the repository, and start running a bunch of vim macros to mark everything up in proper XHTML, which I intended to make easily convertible to any other format if need was felt for it. Each chapter is then spellechecked, proof-read (by myself), and then proof-read by people in the Erlang community. Meanwhile, I add drawings, and after a short stabilization time and enough reviews, I push it online via SFTP.

Regarding the site's backend, I'm running everything from a nearlyfreespeech.net account (pay for what you use), with apache doing some dispatching for me. The site is basically a linked list implemented in PHP, with a few variables pre-defined to be used in ad-hoc templates (URLs and whatnot). That's about it. People often asked my 'Why PHP?' and the answer takes longer to debate with people online than it took me to write the site. It was simple, cheap, fast, and never caused me an issue with the site. I'm using PHP as a template, which is the best use case you can have for it. If I had to do it again, I'd probably use my own [blogging platform](https://bitbucket.org/ferd/blogerl), though.
