# Web Spreadsheet in [99 lines](https://github.com/aosabook/500lines/tree/master/spreadsheet)

In this chapter we introduce a web spreadsheet, written in 99 lines of HTML, CSS and JS, the three languages natively supported by web browsers.

## Introduction

When Tim Berners-Lee invented the Web in 1990, _web pages_ are written in HTML by marking up text with angle-bracketed _tags_, assigning a logical structure to the content. Text marked up within `<a>…</a>` are _hyperlinks_, referring the user to other pages on the web.

In the 1990s, browser makers added various presentational tags to the HTML vocabulary, including the most infamous non-standard tags such as `<blink>…</blink>` from Netscape Navigator and `<marquee>…</marquee>` from Internet Explorer, causing widespread problems in usability and browser compatibility.

In order to keep HTML to its original purpose — describing a document’s logical structure — browser makers eventually agreed to support two additional languages: CSS to describe presentational styles of a page, and JS to describe its dynamic interactions.

Since then, the three languages became more concise and powerful through twenty years of co-evolution. Today, cross-platform _web applications_ (such as Web Spreadsheets) are as ubiquitous and popular as platform-specific applications (such as VisiCalc, Lotus 1-2-3 and Excel) from the previous century.

How many features can a web application offer in 99 lines? Let’s see it in action!

## Overview

The [spreadsheet](https://github.com/aosabook/500lines/tree/master/spreadsheet) directory contains our showcase for the latest evolution of the three languages: [HTML5](http://www.w3.org/TR/html5/) for structure, [CSS3](http://www.w3.org/TR/css3-ui/) for presentation, and the [ES6 “Harmony”](http://wiki.ecmascript.org/doku.php?id=harmony:specification_drafts) standard of JS for interaction.

We also use [Web Storage](http://www.whatwg.org/specs/web-apps/current-work/multipage/webstorage.html) for data persistence, and [Web Worker](http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html) for running JS code in background. These two web standards are supported by major browsers since 2012, such as Firefox, Chrome, Internet Explorer (version 10+), iOS Safari (version 5+) and Android Browser (version 4+).

Now let’s open <http://audreyt.github.io/500lines/spreadsheet/> in a browser:

![Initial Screen](http://user-image.logdown.io/user/6443/blog/6432/static_page/8659/Bx8iguS5mmVbkh3cuBiA_%E8%9E%A2%E5%B9%95%E5%BF%AB%E7%85%A7%202014-06-08%20%E4%B8%8B%E5%8D%8812.03.58.png)
