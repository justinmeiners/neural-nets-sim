
var INPUT_EXCITE = 0;
var INPUT_INHIBIT = 1;

function Cell() {
    this.inputs = [];
    this.inputTypes = [];
    this.output = null;
    this.threshold = 1;
}

function Fiber(i, from, to) {
    this.index = i;
    this.from = from;
    this.to = to;
}

function Branch() {
    this.input = null;
    this.outputs = [];
}

// find an open slot in an array
function insertionIndex(list) {
    var i;
    var n = this.cells.length;

    // find an open slot
    for (i = 0; i < this.cells.length; ++i) {
        if (!this.cells[i]) {
            n = i;
            break;
        }
    }
    return n;
}

function Net() {
    this.cells = [];
    this.fibers = [];
    this.branches = [];
}

Net.prototype.addCell = function() {
    var cell = new Cell();
    this.cells.push(cell);
}

Net.prototype.addFiber = function() {
    var n = insertionIndex(this.fibers);
    var fiber = new Fiber(n);
    this.fibers[n] = fiber;
}

Net.prototype.addBranch = function() {
    var branch = new Branch();
    this.branches.push(branch);
}


function visitFibers(fiber, f) {
    var b;
    var i;
    // apply function
    f(fiber);

    // if its connected to a branch
    // we need to visit the other fibers
    if (fiber.to instanceof Branch) {
        b = fiber.to;
        for (i = 0; i < b.outputs.length; ++i) {
            visitFibers(b.outputs[i], f);
        }
    }
}

function sendSignals(net, state) {
    var i;
    var signals = [];

    var fiberVisitor = function(fiber) {
        signals[fiber.index] = true;
    };

    // send signals from cells to fibers
    for (i = 0; i < net.cells.length; ++i) {
        var firing = state[i];
        if (firing) {
            var cell = net.cells[i];
            visitFibers(cell.output, fiberVisitor);
        }
    }

    return signals;
}

function applySignals(net, state, signals) {
    var i, j;
    var newState = [];
    var cell;
    var inputFiber;
    var inputType;
    var activated;
    var inhibited;

    // mark which cells are firing
    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];

        // count up activated
        // and check for inhibit 
        activated = 0;
        inhibited = false;

        for (j = 0; j < cell.inputs.length; ++j) {
            inputFiber = cell.inputs[j];
            inputType = cell.inputTypes[j];

            if (signals[inputFiber.index]) {
                if (inputType === INPUT_EXCITE) {
                    ++activated;
                } else {
                    inhibited = true;
                    break;
                }
            }
        }
        // ignore inhibited cells
        if (!inhibited && activated >= cell.threshold) {
            // fire this cell
            newState[i] = true;
        }
    }
}


function propogate(net, state) {
    var fiberSignals = sendSignals(net, state);
    return applySignals(net, state, fiberSignals);
}

// VIEW
// ------------------------

function Vec(x, y) {
    this.x = x;
    this.y = y;
}

Vec.prototype.lenSqr = function() {
    return this.x * this.x + this.y * this.y;
}

Vec.prototype.len = function() {
    return Math.sqrt(this.lenSqr());
}

Vec.prototype.inCircle = function(o, r) {
    return Vec.distSqr(this, o) < r * r;
}

Vec.prototype.add = function(b) {
    this.x += b.x;
    this.y += b.y;
}

Vec.add = function(a, b) {
    return new Vec(a.x + b.x, a.y + b.y);
}

Vec.sub = function(a, b) {
    return new Vec(a.x - b.x, a.y - b.y);
}

Vec.distSqr = function(a, b) {
    return Vec.sub(a, b).lenSqr();
}

function CellView(cell) {
    this.cell = cell;
    this.pos = new Vec(0, 0);
    this.size = 15.0;
}





// WINDOW AND CONTEXT
// -------------------------


var gCanvas = document.getElementById('main-canvas');
var gCtx = gCanvas.getContext('2d', { alpha: false });
var gWidth = 640;
var gHeight = 480;
var gDragging;
var gDragInitial = new Vec(0, 0);
var gDragStart;

function getMousePos(canvas, e) {
    var rect = canvas.getBoundingClientRect();
    return new Vec(e.clientX - rect.left, e.clientY - rect.top);
}


function mouseDownHandler(e) {
    var mousePos = getMousePos(gCanvas, e);
    var hit = gCells.find(function (cell) {
        return mousePos.inCircle(cell.pos, cell.size);
    });
    
    if (hit) {
        gDragInitial = mousePos;
        gDragStart = hit.pos;
        gDragging = hit; 
    }
}

function mouseMoveHandler(e) {
    var mousePos = getMousePos(gCanvas, e);
    var delta = Vec.sub(mousePos, gDragInitial);
    if (gDragging) {
        gDragging.pos = Vec.add(gDragStart, delta);
    }

    simLoop();
}

function mouseUpHandler(e) {
    gDragging = null;
}

gCanvas.onmousedown = mouseDownHandler;
gCanvas.onmousemove = mouseMoveHandler;
gCanvas.onmouseup = mouseUpHandler;

var testCell = new CellView();

testCell.pos.x = 40;
testCell.pos.y = 50;

var gCells = [testCell];




function simClearCanvas() {
    gCtx.fillStyle = '#D0E7F9';
    gCtx.beginPath();
    gCtx.rect(0, 0, gWidth, gHeight);
    gCtx.closePath();
    return gCtx.fill();
};

function simDraw() {
    drawCells(gCells);

    /*
    gNeurons.map(function (n) {
        return drawNeuron(n, 20);
    });

    gCtx.fillStyle = '#000000';
    return gNeurons.map(function (n) {
        return drawNeuronText(n, 20);
    });
    */
};

function simLoop() {
    simClearCanvas();
    return simDraw();
};

function drawCells(cells) {
    var i;
    var cell;
    // draw the front of the cells
    gCtx.strokeStyle = '#000000';
    gCtx.fillStyle = '#FFFFFF';

    for (i = 0; i < cells.length; ++i)  { 
        cell = cells[i];

        gCtx.beginPath();
        gCtx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 0.5, Math.PI * 1.5, false);

        gCtx.fill();
        gCtx.stroke();
    }


    // draw the back of the cells
    gCtx.fillStyle = '#444444';

    for (i = 0; i < cells.length; ++i)  { 
        cell = cells[i];

        gCtx.beginPath();
        gCtx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 0.5, Math.PI * 1.5, true);

        gCtx.fill();
        gCtx.stroke();
    }
    
    // draw the text 
    gCtx.fillStyle = '#000000';
    gCtx.font = '14pt monospace';
    gCtx.textAlign = "center";
    gCtx.textBaseline = "middle"; 

    for (i = 0 ; i < cells.length; ++i) {
        cell = cells[i];

        gCtx.fillText(String(3), cell.pos.x - cell.size * 0.5, cell.pos.y);
    }
}

simLoop();




