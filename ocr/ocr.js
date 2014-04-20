const BATCH_SIZE = 1

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
    ctx.fillStyle = '#000000';
    ctx.fillRect(0, 0, 200, 200);
    var matrixSize = 400;
    while (matrixSize--) data.push(0);
    drawGrid();
}


function drawGrid() {
    for (var x = 10, y = 10; x < 200; x += 10, y += 10) {
        ctx.strokeStyle = '#0000ff';
        ctx.beginPath();
        ctx.moveTo(x, 0);
        ctx.lineTo(x, 200);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(0, y);
        ctx.lineTo(200, y);
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
    var xPixel = Math.floor(x / 10);
    var yPixel = Math.floor(y / 10);
    data[((xPixel - 1)  * 20 + yPixel) - 1] = 1;

    this.fillStyle = '#ffffff';
    this.fillRect(xPixel * 10, yPixel * 10, 10, 10);
}

function train() {
    var digitVal = document.getElementById("digit").value;
    if (!digitVal) {
        alert("Please type a digit value in order to train the network");
        return;
    }
    trainArray.push({y0: data, label: parseInt(digitVal)});
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
    xmlHttp.open('POST', "http://localhost:80", false);
    xmlHttp.onload = receiveResponse;
    xmlHttp.onerror = onError;
    var msg = JSON.stringify(json);
    xmlHttp.setRequestHeader('Content-length', msg.length);
    xmlHttp.setRequestHeader("Connection", "close");
    xmlHttp.send(msg);
}