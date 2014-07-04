# Web Spreadsheet in [99 lines](https://github.com/aosabook/500lines/tree/master/spreadsheet)

In this chapter we introduce a Web spreadsheet, written in 99 lines of HTML, CSS and JS, the three languages natively supported by Web browsers.

## Introduction

When Tim Berners-Lee invented the Web in 1990, _Web pages_ were written in HTML by marking up text with angle-bracketed _tags_, assigning a logical structure to the content. Text marked up within `<a>…</a>` became _hyperlinks_ that would refer the user to other pages on the Web.

In the 1990s, browsers added various presentational tags to the HTML vocabulary, including some notoriously nonstandard tags such as `<blink>…</blink>` from Netscape Navigator and `<marquee>…</marquee>` from Internet Explorer, causing widespread problems in usability and browser compatibility.

In order to keep HTML to its original purpose—describing a document’s logical structure—browser makers eventually agreed to support two additional languages: CSS to describe presentational styles of a page, and JS to describe its dynamic interactions.

Since then, the three languages have become more concise and powerful through twenty years of co-evolution. Today, cross-platform _Web applications_ (such as Web Spreadsheets) are as ubiquitous and popular as platform-specific applications (such as VisiCalc, Lotus 1-2-3 and Excel) from the previous century.

How many features can a Web application offer in 99 lines? Let’s see it in action!

## Overview

The [spreadsheet](https://github.com/aosabook/500lines/tree/master/spreadsheet) directory contains our showcase for the latest evolution of the three languages: [HTML5](http://www.w3.org/TR/html5/) for structure, [CSS3](http://www.w3.org/TR/css3-ui/) for presentation, and the JS [ES6 “Harmony”](http://wiki.ecmascript.org/doku.php?id=harmony:specification_drafts) standard for interaction. We also use [Web Storage](http://www.whatwg.org/specs/web-apps/current-work/multipage/webstorage.html) for data persistence, and [Web Worker](http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html) for running JS code in the background. Since 2012, these web standards are supported by Firefox, Chrome, Internet Explorer 10+, as well as mobile browsers on iOS 5+ and Android 4+.

Now let’s open http://audreyt.github.io/500lines/spreadsheet/ in a browser:

![Initial Screen](./images/01-initial.png)

### Basic Concepts

The _spreadsheet_ as shown on the screen is a grid that spans in two dimensions, with _columns_ starting from `A`, and _rows_ starting from `1`. Each _cell_ has a unique _coordinate_ (such as `A1`) and its _content_ (`1874`), which belongs to one of four _types_:

* Text: `+` in `B1` and `⇒` in `D1`, aligned to the left.
* Number: `1874` in `A1` and `2046` in `C1`, aligned to the right.
* Formula:  `=A1+C1` in `E1`, which _calculates_ to the _value_ `3920`, displayed with a light blue background.
* Empty: All cells in row `2` are currently empty.

Click `3920` to set _focus_ on `E1`, revealing its formula in an _input box_:

![Input Box](./images/02-input.png)

Now let’s set focus on `A1` and _change_ its content to `1`, causing `E1` to _recalculate_ its value to `2047`:

![Changed Content](./images/03-changed.png)

Press `ENTER` to set focus to `A2` and change its content to `=Date()`, then press `TAB`, change the content of `B2` to `=alert()`, then press `TAB` again to set focus to `C2`:

![Formula Error](./images/04-error.png)

This shows that a formula may calculates to a number (`2047` in `E1`), a text (the current time in `A2`, aligned to the left), or an _error_ (red letters in`B2`, aligned to the center).

Next, let’s try entering `=for(;;){}`, the JS code for an infinite loop that never terminates. The spreadsheet will prevent us from entering that code, automatically _undo_ the edit after half a second has passed.

Now reload the page in the browser with `Ctrl-R` or `Cmd-R` to verify that the spreadsheet content is _persistent_, staying the same across browser sessions.

Finally, press the `↻` button on the top-left corner to _reset_ the spreadsheet to its original content.
