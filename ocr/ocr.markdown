title: Optical Character Recognition (OCR)
author: Marina Samuel

## Introduction

What if your computer could wash your dishes, do your laundry, cook you dinner,
and clean your home? I think I can safely say that most people would be happy
to get a helping hand! But what would it take for a computer to be able to
perform these tasks, in exactly the same way that humans can? 

The famous computer scientist Alan Turing proposed the Turing Test as a way to
identify whether a machine could have intelligence indistinguishable from that
of a human being. The test involves a human posing questions to two hidden
entities, one human, and the other a machine, and trying to identify which is
which. If the interrogator is unable to identify the machine, then the machine
is considered to have human-level intelligence. 

While there is a lot of controversy surrounding whether the Turing Test is a
valid assessment of intelligence, and whether we can build such intelligent
machines, there is no doubt that machines with some degree of intelligence
already exist. There is currently software that helps robots navigate an office
and perform small tasks, or help those suffering with Alzheimer's. More common
examples of Artificial Intelligence (A.I.) are the way that Google estimates
what you’re looking for when you search for some keywords, or the way that
Facebook decides what to put in your news feed.

One well known application of A.I. is Optical Character Recognition (OCR). An
OCR system is a piece of software that can take images of handwritten
characters as input and interpret them into machine readable text.  While you
may not think twice when depositing a handwritten cheque into a bank machine
that confirms the deposit value, there is some interesting work going on in the
background. This chapter will examine a working example of a simple OCR system
that recognizes numerical digits using an Artificial Neural Network (ANN). But
first, let’s establish a bit more context.


## What is Artificial Intelligence?
While Turing’s definition of intelligence sounds reasonable, at the end of the
day what constitutes intelligence is fundamentally a philosophical debate.
Computer scientists have, however, categorized certain types of systems and
algorithms into branches of AI. Each branch is used to solve certain sets of
problems. These branches include the following examples, as well as [many
others](http://www-formal.stanford.edu/jmc/whatisai/node2.html):

- Logical and probabilistic deduction and inference based on some predefined
  knowledge of a world. e.g. [Fuzzy
  inference](http://www.cs.princeton.edu/courses/archive/fall07/cos436/HIDDEN/Knapp/fuzzy004.htm)
  can help a thermostat decide when to turn on the air conditioning when it
  detects that the temperature is hot and the atmosphere is humid
- Heuristic search. e.g. Searching can be used to find the best possible next
  move in a game of chess by searching all possible moves and choosing the one
  that most improves your position
- Machine learning (ML) with feedback models. e.g. Pattern-recognition problems
  like OCR.

In general, ML involves using large data sets to train a system to identify
patterns. The training data sets may be labelled, meaning the system’s expected
outputs are specified for given inputs, or unlabelled meaning expected outputs
are not specified. Algorithms that train systems with unlabelled data are
called unsupervised algorithms and those that train with labelled data are
called supervised. Although many ML algorithms and techniques exist for
creating OCR systems, ANNs are one simple approach.

## Artificial Neural Networks
### What Are ANNs?

An ANN is a structure consisting of interconnected nodes that communicate with
one another. The structure and its functionality are inspired by neural
networks found in a biological brain. [Hebbian
Theory](http://www.nbb.cornell.edu/neurobio/linster/BioNB420/hebb.pdf) explains
how these networks can learn to identify patterns by physically altering their
structure and link strengths. Similarly, a typical ANN (shown in FIXME) has
connections between nodes that have a weight which is updated as the network
learns. The nodes labelled "+1" are called _biases_. The leftmost blue column
of nodes are _input nodes_, the middle column contains _hidden nodes_, and the
rightmost column contains _output nodes_. There may be many columns of hidden
nodes, known as _hidden layers_.

The values inside all of the circular nodes in FIXME represent the output of
the node. If we call the output of the $n$th node from the top in layer $L$ as
a $n(L)$ and the connection between the $i$th node in layer $L$ and the $j$th
node in layer $L+1$ as $w^{(L)}_ji$, then the output of node $a^{(2)}_2$ is:

$$
a^{(2)}_2 = f(w^{(1)}_{21}x_1 + w^{(1)}_{22}x_2 + b^{(1)}_{2})
$$

 where $f(.)$ is known as the _activation function_ and $b$ is the _bias_. An
 activation function is the decision-maker for what type of output a node has.
 A bias is an additional node with a fixed output of 1 that may be added to an
 ANN to improve its accuracy. We’ll see more details on both of these in
 section FIXME (4.4.) 

This type of network topology is called a feedforward neural network because
there are no cycles in the network. ANNs with nodes whose outputs feed into
their inputs are called recurrent neural networks. There are many algorithms
that can be applied to train feedforward ANNs; one commonly used algorithm is
called Backpropagation. The OCR system we will implement in this chapter will
use backpropagation.

### How Do We Use ANNs?
Like most other ML approaches, the first step for using Backpropagation is to
decide how to transform or reduce our problem into one that can be solved by an
ANN. In other words, how can we manipulate our input data so we can feed it
into the ANN? For the case of our OCR system, we can use the positions of the
pixels for a given digit as input. It is worth noting that, often times,
choosing the input data format is not this simple. If we were analyzing large
images to identify shapes in them, for instance, we may need to pre-process the
image to identify contours within it. These contours would be the input.

Once we’ve decided on our input data format, what’s next? Since backpropagation
is a supervised algorithm, it will need to be trained with labelled data, as
mentioned in FIXME section 2. Thus, when passing the pixel positions as training
input, we must also pass the associated digit. This means that we must find or
gather a large data set of drawn digits and associated values.

The next step is to partition the data set into a training set and validation
set. The training data is used to run the backpropagation algorithm to set the
weights of the ANN. The validation data is used to make predictions using the
trained network and compute its accuracy. If we were comparing the performance
of backpropagation vs. another algorithm on our data, we would [split the
data](http://www-group.slac.stanford.edu/sluo/Lectures/stat_lecture_files/sluo2006lec7.pdf)
into 50% for training, 25% for comparing performance of the 2 algorithms
(validation set) and the final 25% for testing accuracy of the chosen algorithm
(test set). Since we’re not comparing algorithms, we can group one of the
25% sets as part of the training set and use 75% of the data to train the
network and 25% for validating that it was trained well.

The purpose of identifying the accuracy of the ANN is two-fold. First, it is to
avoid the problem of _overfitting_. Overfitting occurs when the network has a
much higher accuracy on predicting the training set than the validation set.
Overfitting tells us that the chosen training data does not generalize well
enough and needs to be refined. Secondly, testing the accuracy of several
different numbers of hidden layers and hidden nodes helps in designing the most
optimal ANN size. An optimal ANN size will have enough hidden nodes and layers
to make accurate predictions but also as few nodes/connections as possible to
reduce computational overhead that may slow down training and predictions. Once
the optimal size has been decided and the network has been trained, it’s ready
to make predictions!

## Design Decisions in a Simple OCR System
In the last few paragraphs we’ve gone over some of the basics of feedforward
ANNs and how to use them. Now it’s time to talk about how we can build an OCR
system. 

First off, we must decide what we want our system to be able to do. To keep
things simple, let’s allow users to draw a single digit and be able to train
the OCR system with that drawn digit or to request that the system predict what
the drawn digit is. While an OCR system could run locally on a single machine,
having a client-server setup gives much more flexibility. It makes
crowd-sourced training of an ANN possible and allows powerful servers to handle
intensive computations. 

Our OCR system will consist of 5 main components, divided into 5 files. There
will be:

- a client (`ocr.js`)
- a server (`server.py`)
- a simple user interface (`ocr.html`)
- an ANN trained via backpropagation (`ocr.py`) 
- an ANN design script (`neural_network_design.py`) 

The user interface will be simple: a canvas to draw digits on and buttons to
either train the ANN or request a prediction.  The client will gather the drawn
digit, translate it into an array, and pass it to the server to be processed
either as a training sample or as a prediction request. The server will simply
route the training or prediction request by making API calls to the ANN module.
The ANN module will train the network with an existing data set on its first
initialization. It will then save the ANN weights to a file and re-load them on
subsequent startups. This module is where the core of training and prediction
logic happens. Finally, the design script is for experimenting with different
hidden node counts and deciding what works best. Together, these pieces give us
a very simplistic, but functional OCR system.

Now that we've thought about how the system will work at a high level, it's
time to put the concepts into code!

### A Simple Interface (`ocr.html`)
As mentioned earlier, the first step is to gather data for training the
network. We could upload a sequence of hand-written digits to the server, but
that would be awkward. Instead, we could have users actually handwrite the
digits on the page using an HTML canvas. We could then give them a couple of
options to either train or test the network, where training the network also
involves specifying what digit was drawn. This way it is possible to easily
outsource the data collection by pointing people to a website to receive their
input. Here’s some HTML to get us started. 

```html
<html>
<head>
	<script src="ocr.js"></script>
	<link rel="stylesheet" type="text/css" href="ocr.css">
</head>
<body onload="ocrDemo.onLoadFunction()">
	<div id="main-container" style="text-align: center;">
		<h1>OCR Demo</h1>
		<canvas id="canvas" width="200" height="200"></canvas>
		<form name="input">
			<p>Digit: <input id="digit" type="text"> </p>
			<input type="button" value="Train" onclick="ocrDemo.train()">
			<input type="button" value="Test" onclick="ocrDemo.test()">
			<input type="button" value="Reset" onclick="ocrDemo.resetCanvas();"/>
		</form> 
	</div>
</body>
</html>
```

### An OCR Client (`ocr.js`)
Since a single pixel on an HTML canvas might be hard to see, we can represent a
single pixel for the ANN input as a square of 10x10 real pixels. Thus the real
canvas is 200x200 pixels and it is represented by a 20x20 canvas from the
perspective of the ANN. The variables below will help us keep track of these
measurements.


```javascript
var ocrDemo = {
    CANVAS_WIDTH: 200,
    TRANSLATED_WIDTH: 20,
    PIXEL_WIDTH: 10, // TRANSLATED_WIDTH = CANVAS_WIDTH / PIXEL_WIDTH
```

We can then outline the pixels in the new representation so they are easier to
see. Here we have a blue grid generated by `drawGrid()`.

```javascript
    drawGrid: function(ctx) {
        for (var x = this.PIXEL_WIDTH, y = this.PIXEL_WIDTH; 
                 x < this.CANVAS_WIDTH; x += this.PIXEL_WIDTH, 
                 y += this.PIXEL_WIDTH) {
            ctx.strokeStyle = this.BLUE;
            ctx.beginPath();
            ctx.moveTo(x, 0);
            ctx.lineTo(x, this.CANVAS_WIDTH);
            ctx.stroke();

            ctx.beginPath();
            ctx.moveTo(0, y);
            ctx.lineTo(this.CANVAS_WIDTH, y);
            ctx.stroke();
        }
    },
```

We also need to store the data drawn on the grid in a form that can be sent to
the server. For simplicity, we can have an array called `data` which labels an
uncoloured, black pixel as `0` and a coloured white pixel as `1`. We also need
some mouse listeners on the canvas so we know when to call `fillSquare()` to
colour a pixel white while a user is drawing a digit. These listeners should
keep track of whether we are in a drawing state and then call `fillSquare()` to
do some simple math and decide which pixels need to be filled in. 

```javascript
    onMouseMove: function(e, ctx, canvas) {
        if (!canvas.isDrawing) {
            return;
        }
        this.fillSquare(ctx, 
            e.clientX - canvas.offsetLeft, e.clientY - canvas.offsetTop);
    },

    onMouseDown: function(e, ctx, canvas) {
        canvas.isDrawing = true;
        this.fillSquare(ctx, 
            e.clientX - canvas.offsetLeft, e.clientY - canvas.offsetTop);
    },

    onMouseUp: function(e) {
        canvas.isDrawing = false;
    },

    fillSquare: function(ctx, x, y) {
        var xPixel = Math.floor(x / this.PIXEL_WIDTH);
        var yPixel = Math.floor(y / this.PIXEL_WIDTH);
        this.data[((xPixel - 1)  * this.TRANSLATED_WIDTH + yPixel) - 1] = 1;

        ctx.fillStyle = '#ffffff';
        ctx.fillRect(xPixel * this.PIXEL_WIDTH, yPixel * this.PIXEL_WIDTH, 
            this.PIXEL_WIDTH, this.PIXEL_WIDTH);
    },
```

Now we’re getting closer to the juicy stuff! We need a function that prepares
training data to be sent to the server. Here we have a relatively straight
forward `train()` function that does some error checking on the data to be sent,
adds it to `trainArray` and sends it off by calling `sendData()`. An interesting
design worth noting here is the use of `trainingRequestCount`, `trainArray`,
and `BATCH_SIZE`. 
