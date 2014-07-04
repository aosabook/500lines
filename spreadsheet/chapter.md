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

Now let’s go through the four source code files, in the same order as the browser loads them:

* **index.html**: 20 lines
* **main.js**: 36 lines (excluding comments and blank lines)
* **worker.js**: 32 lines (excluding comments and blank lines)
* **styles.css**: 11 lines

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

The `if (!window.Spreadsheet)` line tests if `main.js` is loaded correctly; if not, it tells the browser to navigate to `es5/index.html` instead. This _redirect-based graceful degradation_ technique ensures that, for pre-2015 browsers with no ES6 support, we can use the translated-to-ES5 versions of JS programs as a fallback.

The next two line loads the CSS resource, closes the `head` section, and begins the `body` section:

```html
  <link href="styles.css" rel="stylesheet">
</head><body ng-app ng-cloak ng-controller="Spreadsheet">
```

_(to be continued…)_

```html
  <table><tr>
    <th><button ng-click="reset(); calc()" title="Reset">↻</button></th>
```

```html
    <th ng-repeat="col in Cols">{{ col }}</th>
  </tr><tr ng-repeat="row in Rows">
    <th>{{ row }}</th>
```

```html
    <td ng-repeat="col in Cols" ng-class="{ formula: ('=' === sheet[col+row][0]) }">
```

```html
      <input id="{{ col+row }}" ng-model="sheet[col+row]" ng-change="calc()"
                                ng-keydown="keydown( $event, col, row )">
```

```html
      <div ng-class="{ error: errs[col+row], text: vals[col+row][0] }">
        {{ errs[col+row] || vals[col+row] }}&nbsp;</div>
```

```html
    </td>
  </tr></table>
</body></html>
```

### JS: Main Thread

```js
window.Spreadsheet = ($scope, $timeout)=>{
```

```js
  function* range(cur, end) { while (cur <= end) {
    yield cur;
    // If it's a number, increase it by one; otherwise move to next letter
    cur = (isNaN( cur ) ? String.fromCodePoint( cur.codePointAt()+1 ) : cur+1);
  } }
```

```js
  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [ for (col of range( 'A', 'H' )) col ];
  $scope.Rows = [ for (row of range( 1, 20 )) row ];
```

```js
  // UP (38) and DOWN/ENTER (40/13) keys move focus to the row above (-1) or below (+1).
  $scope.keydown = ({which}, col, row)=>{ switch (which) {
    case 38: case 40: case 13: $timeout( ()=>{
      const direction = (which == 38) ? -1 : +1;
      const cell = document.querySelector( `#${ col }${ row + direction }` );
      if (cell) { cell.focus(); }
    } )
  } };
```

```js
  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '+', C1: 2046, D1: '⇒', E1: '=A1+C1' } }
```

```js
  // Define the initializer, and immediately call it
  ($scope.init = ()=>{
    // Restore the previous .sheet; reset to default if it's the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'worker.js' );
  })();
```

```js
  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  [$scope.errs, $scope.vals] = [ {}, {} ];
```

```js
  // Define the calculation handler, and immediately call it
  ($scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet );
```

```js
    const promise = $timeout( ()=>{
      // If the worker has not returned in 0.5 seconds, terminate it
      $scope.worker.terminate();
      // Back up to the previous state and make a new worker
      $scope.init();
      // Redo the calculation using the last-known state
      $scope.calc();
    }, 500 );
```

```js
    // When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = ({data})=>{ $timeout( ()=>{
      [$scope.errs, $scope.vals] = data;
      localStorage.setItem( '', json );
      $timeout.cancel( promise );
    } ) }
```

```js
    // Post the current sheet content for the worker to process
    $scope.worker.postMessage( $scope.sheet );
  })();
}
```

### JS: Worker Thread

```js
if (this.importScripts) {
```

```js
  // Fast eval without the "this" object; available as execScript in IE
  const globalEval = self.execScript || ( (js)=>eval.call( null, js ) );

  // Forward the incoming messages to calc(), and post its result back
  self.onmessage = (event)=>{ self.postMessage( calc( event ) ) };
```

```js
  let sheet, cache, errs, vals;
  function calc({data}) {
    [sheet, cache, errs, vals] = [ data, {}, {}, {} ];
```

```js
    for (const coord in sheet) {
      // Four variable names pointing to the same coordinate: A1, a1, $A1, $a1
      for (const name of [ for (p of [ '', '$' ]) for (c of [ coord, coord.toLowerCase() ]) p+c ]) {
```

```js
        // Worker is reused across calculations, so only define each variable once
        if (( Object.getOwnPropertyDescriptor( self, name ) || {} ).get) { continue; }

        // Define self['A1'], which is the same thing as the global variable A1
        Object.defineProperty( self, name, { get() {
          if (coord in cache) { return cache[coord]; }
          cache[coord] = NaN;
```

```js
          // Convert numeric-looking strings into numbers so =A1+C1 works when both are numbers
          let val = +sheet[coord];
          if (sheet[coord] != val.toString()) { val = sheet[coord]; }
```

```js
          // Evaluate formula cells that begin with =
          try { cache[coord] = ( ('=' === val[0]) ? globalEval( val.slice( 1 ) ) : val ); }
```

```js
          catch (e) {
            const match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec( e );
            if (match && !( match[0] in self )) {
              // The formula refers to a uninitialized cell; set it to 0 and retry
              self[match[0]] = 0;
              delete cache[coord];
              return self[coord];
            }
            // Otherwise, stringify the caught exception in the errs object
            errs[coord] = e.toString();
          }
          return cache[coord];
        } } )
      }
    }
```

```js
    // For each coordinate in the sheet, call the property getter defined above
    for (const coord in sheet) { vals[coord] = self[coord]; }
    return [ errs, vals ];
  }
}
```

### CSS

```css
input { position: absolute; border: 0; padding: 0;
        width: 120px; height: 1.3em; font-size: 100%;
        color: transparent; background: transparent; }
```

```css
input:focus { color: #111; background: #efe; }
input:focus + div { white-space: nowrap; }
```

```css
table { border-collapse: collapse; }
th { background: #ddd; }
td, th { border: 1px solid #ccc; }
```

```css
td.formula { background: #eef; }
td div { text-align: right; width: 120px; overflow: hidden; text-overflow: ellipsis; }
td div.text { text-align: left; }
td div.error { text-align: center; color: #800; font-size: 90%; border: solid 1px #800 }
```

_(to be continued…)_
