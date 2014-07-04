# [Web Spreadsheet](http://audreyt.github.io/500lines/spreadsheet/) in [99 lines](https://github.com/audreyt/500lines/tree/master/spreadsheet)

In this chapter we introduce a Web spreadsheet, written in 99 lines of HTML, CSS and JS, the three languages natively supported by Web browsers.

## Introduction

When Tim Berners-Lee invented the Web in 1990, _Web pages_ were written in HTML by marking up text with angle-bracketed _tags_, assigning a logical structure to the content. Text marked up within `<a>…</a>` became _hyperlinks_ that would refer the user to other pages on the Web.

In the 1990s, browsers added various presentational tags to the HTML vocabulary, including some notoriously nonstandard tags such as `<blink>…</blink>` from Netscape Navigator and `<marquee>…</marquee>` from Internet Explorer, causing widespread problems in usability and browser compatibility.

In order to keep HTML to its original purpose—describing a document’s logical structure—browser makers eventually agreed to support two additional languages: CSS to describe presentational styles of a page, and JS to describe its dynamic interactions.

Since then, the three languages have become more concise and powerful through twenty years of co-evolution. Today, cross-platform _Web applications_ (such as Web Spreadsheets) are as ubiquitous and popular as platform-specific applications (such as VisiCalc, Lotus 1-2-3 and Excel) from the previous century.

How many features can a Web application offer in 99 lines? Let’s see it in action!

## Overview

The [spreadsheet](https://github.com/audreyt/500lines/tree/master/spreadsheet) directory contains our showcase for the latest evolution of the three languages: [HTML5](http://www.w3.org/TR/html5/) for structure, [CSS3](http://www.w3.org/TR/css3-ui/) for presentation, and the JS [ES6 “Harmony”](http://wiki.ecmascript.org/doku.php?id=harmony:specification_drafts) standard for interaction. We also use [Web Storage](http://www.whatwg.org/specs/web-apps/current-work/multipage/webstorage.html) for data persistence, and [Web Worker](http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html) for running JS code in the background. Since 2012, these web standards are supported by Firefox, Chrome, Internet Explorer 10+, as well as mobile browsers on iOS 5+ and Android 4+.

Now let’s open http://audreyt.github.io/500lines/spreadsheet/ in a browser:

![Initial Screen](./images/01-initial.png)

### Basic Concepts

The spreadsheet spans in two dimensions, with _columns_ starting from **A**, and _rows_ starting from **1**. Each _cell_ has a unique _coordinate_ (such as **A1**) and its _content_ (`1874`), which belongs to one of four _types_:

* Text: `+` in **B1** and `⇒` in **D1**, aligned to the left.
* Number: `1874` in **A1** and `2046` in **C1**, aligned to the right.
* Formula:  `=A1+C1` in **E1**, which _calculates_ to the _value_ `3920`, displayed with a light blue background.
* Empty: All cells in row `2` are currently empty.

Click `3920` to set _focus_ on **E1**, revealing its formula in an _input box_:

![Input Box](./images/02-input.png)

Now let’s set focus on **A1** and _change_ its content to `1`, causing **E1** to _recalculate_ its value to `2047`:

![Changed Content](./images/03-changed.png)

Press **ENTER** to set focus to **A2** and change its content to `=Date()`, then press **TAB**, change the content of **B2** to `=alert()`, then press **TAB** again to set focus to `C2`:

![Formula Error](./images/04-error.png)

This shows that a formula may calculate to a number (`2047` in **E1**), a text (the current time in **A2**, aligned to the left), or an _error_ (red letters in **B2**, aligned to the center).

Next, let’s try entering `=for(;;){}`, the JS code for an infinite loop that never terminates. The spreadsheet will prevent us, automatically _restoring_ the content of **C2** half a second after an attempted change.

Now reload the page in the browser with **Ctrl-R** or **Cmd-R** to verify that the spreadsheet content is _persistent_, staying the same across browser sessions. To _reset_ the spreadsheet to its original contents, press the `↻` button on the top-left corner.

### Progressive Enhancement

Before we dive into the 99 lines of code, it’s worthwhile to disable JS in the browser, reload the page, and note the differences:

* Instead of a large grid, only a 2x2 table remains on screen with a single content cell.
* Row and column labels are replaced by `{{ row }}` and `{{ col }}`.
* Pressing the `↻` button produces no effect.
* Press **TAB** or clicking into the first line of content still reveals an editable input box.

![With JavaScript Disabled](./images/05-nojs.png)

When we disable the dynamic interactions (JS), the content structure (HTML) and presentational styles (CSS) remain in effect. If a website is useful with both JS and CSS disabled, we say it adheres to the _progressive enhancement_ principle, making its content accessible to the largest audience possible.

Because our spreadsheet is a web application with no server-side code, we must rely on JS to provide the required logic. However, it does work correctly when CSS is not fully supported, such as with screen readers and text-mode browsers. If we enable JS in the browser, but disable CSS instead, the effects are:

* All background and foreground colors are gone.
* The input box and the cell value are both displayed, instead of just one at a time.
* Otherwise, the application still works the same as the full version.

![With CSS Disabled](./images/06-nocss.png)

## Code Walkthrough

Now let’s look at the four source code files, in the same order as the browser loads them:

* **index.html**: 18 lines
* **main.js**: 36 lines (excluding comments and blank lines)
* **worker.js**: 33 lines (excluding comments and blank lines)
* **styles.css**: 12 lines

### HTML

The first line of `index.html` declares that it’s written in HTML5 (`<!DOCTYPE html>`) with the UTF-8 encoding:

```html
<!DOCTYPE html><html><head><meta charset="UTF-8">
```

Without the `charset` declaration, the browser may display the reset button’s Unicode symbol `↻` as `â†»`, an example of _mojibake_ — garbled text caused by decoding issues.

The next four lines are JS declarations, usually placed within the `head` section:

```html
  <script src="main.js"></script>
  <script>if (!window.Spreadsheet) { location.href = "es5/index.html" }</script>
  <script src="worker.js"></script>
  <script src="lib/angular.js"></script>
```

The `script src="…"` tags load JS resources from the same path as the HTML page. For example,  if the current URL is `http://audreyt.github.io/500lines/spreadsheet/index.html`, then `lib/angular.js` refers to `http://audreyt.github.io/500lines/spreadsheet/lib/angular.js`.

The `if (!window.Spreadsheet)` conditional tests if `main.js` is loaded correctly; if not, it tells the browser to load the `es5/index.html` page instead. This _redirect-based graceful degradation_ technique ensures that, for pre-2015 browsers with no ES6 support, we can use the translated-to-ES5 versions of JS programs as a fallback.

The next two line loads the CSS resource, closes the `head` section, and begins the `body` section:

```html
  <link href=“styles.css” rel=“stylesheet”>
</head><body ng-app ng-cloak ng-controller="Spreadsheet">
```

### JS: Main Thread

### JS: Worker Thread

### CSS

