# Blockcode - a simple visual programming toolkit

> Comments from Debo:

> You mention several examples of block-based languages in your intro paragraph, but I think it might be helpful to have a few sentences that just outright explain what a block-based language is, and how it differs from a 'traditional' programming language. The links are helpful in digital formats, but we will also be printing this book and it's harder for those readers to follow hyperlinks :)

> [dethe]: Move all links to endnotes

> You describe the js modules/files that you've decomposed the program into, but not how you arrived at that segmentation / design. The goal of this book is to give our readers examples of how to design software, so some context there would be great.

> Generally, the chapter outline so far feels like it progresses too quickly. We go from "here is a high-level description of block-based programming language" to "here is a listing of all the functions in my implementation".

Block Code is an attempt to create a block-based programming tool in under 500 lines of code, including HTML, CSS, Javascript. The code itself is intended to be a live-coding environment for turtle graphics.

A block-based language differs from programming language where you type in code as words because you drag blocks that represent code into place. Learning a programming language can be difficult because they are extremely sensitive to even the slightest of typos. Most programming languages are case-sensitive, have obscure syntax, and will refuse to run if you get so much as a semicolon in the wrong place, or worse, leave one out. Most of the programming languages in use today are also based on English and the language itself cannot be localized. In contrast, a well-done block language can eliminate syntax errors completely: you can still create a program which does the wrong thing, but you cannot create one with the wrong syntax, the blocks just won't fit that way. Block languages are more more discoverable: you can see all the constructs and libraries of the language right in the list of blocks. And blocks can be localized into any language without changing the programmatic meaning of the language.

Block-based languages have a long history, with some of the prominent ones being Lego Mindstorms [Mindstorms], Alice3D [Alice], StarLogo [StarLogo], and especially Scratch [Scratch]. There are several tools for block-based programming on the web as well, such as Blockly [Blockly], AppInventor [AppInventor], Tynker [Tynker], and many more [Visual Programming].

This particular code is loosely based on the open-source project Waterbear [Waterbear], which is not a language but a tool for wrapping existing languages with a block-based syntax. Advantages of such a wrapper include the ones noted above: Eliminating syntax errors, visual display of available components, blocks are more localizable than programming languages (you can translate the text on blocks more readily than translating a programming language). Additionally visual code can sometimes be easier to read/debug and blocks can be used by pre-typing children. We could even go further and put icons on the blocks, either in conjunction with the text names or instead of them, to allow pre-literate children to write programs, but we don't go that far in this example.

The choice of turtle graphics for this language also goes back to the Logo language, which was created specifically for teaching programming to children. Several of the block-based languages above include turtle graphics, and it is a small enough domain to be able to capture in a tightly constrained project such as this. Later we will see how easy it can be to extend or replace the turtle graphics code with code for other types of programming.

## Stepping through the code

I've tried to follow some conventions and best practices throughout this project. Each JavaScript file is wrapped in a function to avoid leaking variables into the global environment. If it needs to expose variables to other files it will define a single global per file, based on the filename, with the exposed functions in it. This will be near the end of the file, followed by any event handlers set by that file, so you can always glance a the end of a file to see what events it handles.

Aside from `blocks.css` which provides styling (and some help with functionality we'll also explore) and `index.html` to tie everything together, there are six JavaScript files: `blocks.js` defines the block objects and how they work, `drag.js` implements drag-and-drop using HTML5 native drag-and-drop, `file.js` handles loading and saving block scripts (as JSON) as well as loading the examples, `turtle.js` implements our little turtle graphics language and the blocks for it, and `util.js` removes namespaces from some useful browser methods and implements a couple of shortcuts to save us typing (this file has a similar purpose in the project that jQuery has in other projects, but in < 50 lines of code).

The file `menu.js` is a little bit weird: menu in this context is the list of blocks you can choose for your script and this file sets that up and adds a looping block that is generally useful (and thus not part of the turtle language itself) as well as some code for actually running the scripts. So this is kind of an odds-and-ends file, for things that may not have fit anywhere else.

### blocks.js

Each block consists of a few HTML elements, styled with CSS, with some JavaScript event handlers for drag-and-drop and modifying the input argument. It's all standard web stuff, and this file just helps to create and manage these grouping of elements as single objects. When a type of block is added to the block menu, it is also associated with a JavaScript function to run to implement the language, and so each block in the script has to be able to find its associated function and to call it when the script runs.

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

We're using HTML5 drag and drop, which requires some specific JavaScript event handlers to be defined, and those are defined here. For more information on using HTML5 drag and drop, see Eric Bidleman's article here: http://www.html5rocks.com/en/tutorials/dnd/basics/. While it is nice to have built-in support for drag and drop, it does have some oddities when using it, and some pretty major limitations (like not being implemented in any mobile browser at the time of this writing).

We define a bunch of variables at the top of the file. When we're dragging, we'll need to reference these from different stages of the dragging callback dance.

#### `findPosition(evt)`

Find which block we should insert the dragged block before.

#### `drop(evt)`

The `findPosition` function is called on drop vs. drag because of bug in Firefox drag event (no clientX, etc). This should also improve drag performance, but doesn't give opportunity to provide user feedback during drag. Ideally we'd like to highlight potential drop targets while the user is dragging, to show where a block can legally be placed and to provide some feedback that it will be dropped where they expect.

Depending on where the drag starts from and ends, it will have different effects:

* If dragging from script to menu, delete dragTarget (remove block from script).
* If dragging from script to script, move dragTarget (move an existing script block).
* If dragging from menu to script, copy dragTarget (insert new block in script).
* If dragging from menu to menu, do nothing.

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

## References

[Mindstorms]

[Alice]

[StarLogo]

[Scratch]

[Blockly]

[AppInventor]

[Tynker]

[Visual Programming] http://en.wikipedia.org/wiki/Visual_programming_language

[Waterbear] http://waterbearlang.com/









