/**
  * This module creates a 200x200 pixel canvas for a user to draw
  * digits. The digits can either be used to to train the neural network
  * or to test the network's current prediction for that digit.
  *
  * To simplify computation, the 200x200px canvas is translated as a 20x20px
  * canvas to be processed as an input array of 1s (white) and 0s (black) on
  * on the server side. Each new translated pixel's size is 10x10px
  */
const CANVAS_WIDTH = 200
const TRANSLATED_WIDTH = 20
const PIXEL_WIDTH = 10 // TRANSLATED_WIDTH = CANVAS_WIDTH / PIXEL_WIDTH
const BATCH_SIZE = 1

// Server Variables
const PORT = "8000"
const HOST = "http://localhost"

// Colors
const BLACK = "#000000"
const BLUE = "#0000ff"

var ctx, canvas;
var data = [];
var trainArray = [];
var trainingRequestCount = 0;

function onLoadFunction() {
    canvas = document.getElementById('canvas');
    ctx = canvas.getContext('2d');
    resetCanvas();

    ctx.fillSquare = fillSquare;
    canvas.onmousemove = onMouseMove;
    canvas.onmousedown = onMouseDown;
    canvas.onmouseup = onMouseUp;
}

function resetCanvas() {
    data = [];
    ctx.fillStyle = BLACK;
    ctx.fillRect(0, 0, CANVAS_WIDTH, CANVAS_WIDTH);
    var matrixSize = 400;
    while (matrixSize--) data.push(0);
    drawGrid();
}


function drawGrid() {
    for (var x = PIXEL_WIDTH, y = PIXEL_WIDTH; x < CANVAS_WIDTH; x += PIXEL_WIDTH, y += PIXEL_WIDTH) {
        ctx.strokeStyle = BLUE;
        ctx.beginPath();
        ctx.moveTo(x, 0);
        ctx.lineTo(x, CANVAS_WIDTH);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(0, y);
        ctx.lineTo(CANVAS_WIDTH, y);
        ctx.stroke();
    }
}

function onMouseMove(e) {
    if (!canvas.isDrawing) {
        return;
    }
    ctx.fillSquare(e.clientX - this.offsetLeft, e.clientY - this.offsetTop);
}

function onMouseDown(e) {
    canvas.isDrawing = true;
    ctx.fillSquare(e.clientX - this.offsetLeft, e.clientY - this.offsetTop);
}

function onMouseUp(e) {
    canvas.isDrawing = false;
}

function fillSquare(x, y) {
    var xPixel = Math.floor(x / PIXEL_WIDTH);
    var yPixel = Math.floor(y / PIXEL_WIDTH);
    data[((xPixel - 1)  * TRANSLATED_WIDTH + yPixel) - 1] = 1;

    this.fillStyle = '#ffffff';
    this.fillRect(xPixel * PIXEL_WIDTH, yPixel * PIXEL_WIDTH, PIXEL_WIDTH, PIXEL_WIDTH);
}

function train() {
    var digitVal = document.getElementById("digit").value;
    if (!digitVal) {
        alert("Please type a digit value in order to train the network");
        return;
    }
    trainArray.push({"y0": data, "label": parseInt(digitVal)});
    trainingRequestCount++;

    // Time to send a training batch to the server.
    if (trainingRequestCount == BATCH_SIZE) {
        alert("Sending training data to server...");
        var json = {
            trainArray: trainArray,
            train: true
        };

        sendData(json);
        trainingRequestCount = 0;
        trainArray = [];
    }
}

function test() {
    var json = {
        image: data,
        train: false
    };
    sendData(json);
}

function receiveResponse() {
    var responseJSON = JSON.parse(this.responseText);
    if (this.responseText && responseJSON.type == "test") {
        alert("The neural network predicts you wrote a \'" + responseJSON.result + '\'');
    }
}

function onError(e) {
    alert("Error occurred while connecting to server: " + e.target.statusText);
}

function sendData(json) {
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.open('POST', HOST + ":" + PORT, false);
    xmlHttp.onload = receiveResponse;
    xmlHttp.onerror = onError;
    var msg = JSON.stringify(json);
    xmlHttp.setRequestHeader('Content-length', msg.length);
    xmlHttp.setRequestHeader("Connection", "close");
    xmlHttp.send(msg);
}