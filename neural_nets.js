
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
    var n = list.length;

    // find an open slot
    for (i = 0; i < list.length; ++i) {
        if (!list[i]) {
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
    return cell;
}

Net.prototype.addFiber = function(from, to) {
    var n = insertionIndex(this.fibers);
    var fiber = new Fiber(n, from, to);
    this.fibers[n] = fiber;
    return fiber;
}

Net.prototype.addBranch = function() {
    var branch = new Branch();
    this.branches.push(branch);
    return branch;
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
                if (inputType === INPUT_INHIBIT) {
                    inhibited = true;
                    break; 
                } else {
                    ++activated;
                }
            }
        }
        // ignore inhibited cells
        if (!inhibited && activated >= cell.threshold) {
            // fire this cell
            newState[i] = true;
        }
    }

    return newState;
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

function FiberView(fiber) {
    this.fiber = fiber;
    this.from = null;
    this.to = null;
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

    simDraw();
}

function mouseUpHandler(e) {
    gDragging = null;
}

gCanvas.onmousedown = mouseDownHandler;
gCanvas.onmousemove = mouseMoveHandler;
gCanvas.onmouseup = mouseUpHandler;

var net = new Net();

var testCell = new CellView(net.addCell());
testCell.pos.x = 40;
testCell.pos.y = 50;
testCell.cell.threshold = 0;

var testCell2 = new CellView(net.addCell());
testCell2.pos.x = 90;
testCell2.pos.y = 50;

var testCell3 = new CellView(net.addCell());
testCell3.pos.x = 150;
testCell3.pos.y = 50;

var testFiber = new FiberView(net.addFiber(testCell.cell, testCell2.cell));
testFiber.from = testCell;
testFiber.to = testCell2;

var testFiber2 = new FiberView(net.addFiber(testCell2.cell, testCell3.cell));
testFiber2.from = testCell2;
testFiber2.to = testCell3;

var testFiber3 = new FiberView(net.addFiber(testCell3.cell, testCell.cell));
testFiber3.from = testCell3;
testFiber3.to = testCell;


testCell.cell.output = testFiber.fiber;
testCell2.cell.inputs.push(testFiber.fiber);

testCell2.cell.output = testFiber2.fiber;
testCell3.cell.inputs.push(testFiber2.fiber);

testCell3.cell.output = testFiber3.fiber;
testCell.cell.inputs.push(testFiber3.fiber);
testCell.cell.inputTypes.push(INPUT_INHIBIT);


var gCells = [testCell, testCell2, testCell3];
var gFibers = [testFiber, testFiber2, testFiber3];

var gState = [];

setInterval(function() {
    gState = propogate(net, gState);
    simDraw();
}, 500);


function simClearCanvas() {
    gCtx.fillStyle = '#D0E7F9';
    gCtx.beginPath();
    gCtx.rect(0, 0, gWidth, gHeight);
    gCtx.closePath();
    return gCtx.fill();
};

function simDraw() {
    simClearCanvas();

    drawCells(gCells);
    drawFibers(gFibers);

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

function drawCells(cells) {
    var i;
    var cell;

    // draw the back of the cells
    gCtx.strokeStyle = '#000000';
    gCtx.fillStyle = '#FFFFFF';

    for (i = 0; i < cells.length; ++i)  { 
        cell = cells[i];

        gCtx.beginPath();
        gCtx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 0.5, Math.PI * 1.5, false);

        gCtx.fill();
        gCtx.stroke();
    }


    // draw the front of the cells

    for (i = 0; i < cells.length; ++i)  { 
        cell = cells[i];

        if (gState[i]) {
            gCtx.fillStyle = '#FF0000';
        } else {
            gCtx.fillStyle = '#444444';
        }

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

    for (i = 0; i < cells.length; ++i) {
        cell = cells[i];

        gCtx.fillText(String(cell.cell.threshold), cell.pos.x - cell.size * 0.5, cell.pos.y);
    }
}

function drawFibers(fibers) {
    var i;
    var fiber;
    gCtx.strokeStyle = '#000000';

    for (i = 0; i < fibers.length; ++i) {
        fiber = fibers[i];

        gCtx.beginPath();
        gCtx.moveTo(fiber.from.pos.x + fiber.from.size, fiber.from.pos.y);
        gCtx.bezierCurveTo(fiber.from.pos.x + fiber.from.size + 50, fiber.from.pos.y,
                      fiber.to.pos.x - fiber.to.size - 50, fiber.to.pos.y,
                      fiber.to.pos.x - fiber.to.size, fiber.to.pos.y);

        gCtx.stroke();
                      

    }


}


