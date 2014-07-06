# Spreadsheet

    Author: Audrey Tang
    Project: Web Spreadsheet in 99 Lines
    Languages: JavaScript, HTML, CSS
    Dependencies: AngularJS, Web Workers, Traceur Compiler

## Online Demo

<http://audreyt.github.io/500lines/spreadsheet/>

## Local Demo

Simply open `index.html` with Firefox, Safari or IE11+, and enter some content.

Values starting with `=` are parsed as formula written in JavaScript, for example `=A1*C1`.

If you'd like to use Chrome, type `make run` or `node extra/static-here.js` and connect to [localhost:8888](http://127.0.0.1:8888/) to view the demo.

## Building the Code

Source code (`main.js` and `worker.js`) are written in ECMAScript 6 (aka _ES.next_, aka _Harmony_), specifically the subset marked with [TC39 Consensus](https://developer.mozilla.org/en-US/docs/Web/JavaScript/ECMAScript_6_support_in_Mozilla) as of February 2014.

For backward compatibility with ECMAScript 5 browsers, we use [Traceur](https://github.com/google/traceur-compiler) to compile source files into the `es5/` directory.

To build from source, first install [NodeJS](http://www.nodejs.org/) 0.10 or later, and run `make` (only tested on Linux/OSX at the moment).
