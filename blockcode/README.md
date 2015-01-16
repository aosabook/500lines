# Blockcode - a simple visual programming toolkit

Block Code is an attempt to create a block-based programming tool in under 500 lines of code, including HTML, CSS, Javascript. The code itself is intended to be a live-coding environment for turtle graphics.

Block-based languages have a long history, with some of the prominent ones being Lego Mindstorms, Alice3D, StarLogo, and especially Scratch. There are several tools for block-based programming on the web as well, such as Blockly, AppInventor, Tynker, and [many more](http://en.wikipedia.org/wiki/Visual_programming_language).

This particular code is loosely based on the open-source project [Waterbear](http://waterbearlang.com/), which is not a language but a tool for wrapping existing languages with a block-based syntax. Advantages of such a wrapper include: Eliminating syntax errors, visual display of available components, easier to read/debug (sometimes), blocks are more localizable than programming languages, blocks can be used by pre-literate or pre-typing children.

## Stepping through the code

I've tried to follow some conventions and best practices throughout this project. Each JavaScript file is wrapped in a function to avoid leaking variables into the global environment. If it needs to expose variables to other files it will define a single global per file, based on the filename, with the exposed functions in it. This will be near the end of the file, followed by any event handlers set by that file, so you can always glance a the end of a file to see what events it handles.

### blocks.js

#### `createBlock(name, value, contents)`

This can be used to create blocks for the menu, or for restoring blocks saved in files or localStorage.

####`blockContents(block)`

Simply retrieve the child blocks of a container block. Always returns a list if called on a container block, always returns null on a simple block

#### `blockValue(block)`

Return the numerical value of the input on a block, if the block has an input field of type number, or string for other input type, null if there is no input element for the block.

#### `blockScript(block)`

Returns the script of a block as a structure suitable for stringifying with JSON. Used for saving blocks in a form they can easily be restored from.

#### `runBlocks(blocks)`

Handler to run an array of blocks by sending each block the "run" event.

### drag.js

Defines a bunch of variables at the top of the file. When we're dragging, we'll need to reference these from different stages of the dragging callback dance

#### `findPosition(evt)`

Find which block we should insert the dragged block before.

#### `drop(evt)`

The `findPosition` function is called on drop vs. drag because of bug in Firefox drag event (no clientX, etc). This should also improve drag performance, but doesn't give opportunity to provide user feedback during drag.

If dragging from script to menu, delete dragTarget.

If dragging from script to script, move dragTarget.

If dragging from menu to script, copy dragTarget.

If dragging from menu to menu, do nothing.

#### `dragEnd(evt)`

Based on the repetitive nature of this code, it looks like a good place for a helper function.

### menu.js

#### `menu`, `script`

We use these a lot, keep references around.

#### `scriptRegistry`

This is where we will store the scripts of blocks in the menu. We use a very simple name -> script mapping, so it does not support either multiple menu blocks with the same name, or renaming blocks.

#### `scriptDirty`

Keep track of whether the script has been modified since the last time it was run, so we don't keep trying to run it constantly.

#### `runSoon()`

Flag the system that we should run the assembled script during the next frame handler

#### `menuItem(name, fn, value, contents)`

A menu block is just a normal block that is associated with a function, and it lives in the menu column.

#### `run()`

Run all the script blocks, let the specific language handle any tasks it needs before and after the script is run.

#### `runEach(evt)`

As each block is run, set a class on it, then find and execute its associated function. If we slow things down, you should be able to watch the code execute as each block highlights to show when it is running.

#### `repeat(block)`

One of the default menu blocks reused in each language.

### file.js

#### `saveLocal()`

Handler to save the current script in localStorage on page refresh.

#### `scriptToJson()`

Self-explanatory, a utility for converting scripts to JSON format.

#### `jsonToScript(json)`

The inverse of `scriptToJson()`.

#### `restoreLocal()`

Handler to restore the current script on page refresh.

#### `clearScript()`

Handler to clear the current script.

#### `saveFile()`

Handler to save to a local file

#### `readFile(file)`

Handler to load from a local file.

#### `loadFile()`

Part of the handshake involved in asych file loading.

### turtle.js

This is the implmentation of the turtle block language. It exposes no globals of its own., but it does define local variables for managing the canvas itself and save some handier references to frequently-used parts of Math.

#### `reset()`

Resets all the state variables (we could embed these in an object if we wanted to support more than one turtle).

#### `deg2rad(deg)`

Utility so we can work with degrees in the UI, but draw in radians.

#### `drawTurtle()`

Draw the turtle itself. Default turtle is a triangle, but you could override this to get a more "turtle-looking" turtle.

### util.js

This code may not belong in the count of 500 lines since it is all support and polyfills for things that should be native to the browser.

#### `elem(name, attrs, children)`

Shorthand function to create elements easily, while also setting their attributes and adding child elements

#### `matches(elem, selector)`

Remove namespace for matches. This is a lot of code just to get functionality that is already built-in.

#### `closest(elem, selector)`

Emulate one of the handiest methods in all of jQuery that isn't already built in to the browser yet

#### `requestAnimationFrame(fn)`

Another polyfill for built-in functionality, just to get rid of namespaces in older browsers, or to emulate it for browsers that don't have requestAnimationFrame yet.

#### `trigger(name, target)`

Shorthand for sending a custom event to an element.

### index.html

Including the turtle.js file is the only thing specific to the turtle language, the rest can be re-used for other block languages.













