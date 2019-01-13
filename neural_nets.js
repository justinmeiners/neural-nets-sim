function CellView() {
    this.pos = new Vec(0, 0);
    this.size = 15.0;
}

function BranchView() {
    this.pos = new Vec(0, 0);
    this.size = 5.0;
}



var INPUT_EXCITE = 0;
var INPUT_INHIBIT = 1;

function Cell() {
    this.inputs = [];
    this.inputTypes = [];
    this.output = null;
    this.threshold = 1;

    this.view = new CellView();
}

function Fiber(i, from, to) {
    this.index = i;
    this.from = from;
    this.to = to;
}

function Branch() {
    this.input = null;
    this.outputs = [];

    this.view = new BranchView();
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

// returns both the state and the signals
// the signals are not needed for the simulation
// but are helpful for visualization
/*
function propogate(net, state) {
    var fiberSignals = sendSignals(net, state);
    var newState = applySignals(net, state, fiberSignals);
    return [newState, fiberSignals];
}
*/

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

Vec.dist = function(a, b) {
    return Math.sqrt(Vec.distSqr(a, b));
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
    var hit = gNet.cells.find(function (cell) {
        return mousePos.inCircle(cell.view.pos, cell.view.size);
    });
    
    if (hit) {
        gDragInitial = mousePos;
        gDragStart = hit.view.pos;
        gDragging = hit; 
    }
}

function mouseMoveHandler(e) {
    var mousePos = getMousePos(gCanvas, e);
    var delta = Vec.sub(mousePos, gDragInitial);
    if (gDragging) {
        gDragging.view.pos = Vec.add(gDragStart, delta);
    }

    simDraw();
}

function mouseUpHandler(e) {
    gDragging = null;
}

gCanvas.onmousedown = mouseDownHandler;
gCanvas.onmousemove = mouseMoveHandler;
gCanvas.onmouseup = mouseUpHandler;

var gNet = new Net();

var c1 = gNet.addCell();
c1.view.pos.x = 40;
c1.view.pos.y = 50;
c1.threshold = 0;

var c2 = gNet.addCell();
c2.view.pos.x = 90;
c2.view.pos.y = 50;

var c3 = gNet.addCell();
c3.view.pos.x = 150;
c3.view.pos.y = 50;

var f1 = gNet.addFiber(c1, c2);
var f2 = gNet.addFiber(c2, c3);
var f3 = gNet.addFiber(c3, c1);


c1.output = f1
c2.inputs.push(f1);

c2.output = f2;
c3.inputs.push(f2);

c3.output = f3;
c1.inputs.push(f3);
c1.inputTypes.push(INPUT_INHIBIT);

var gState = [];
var gSignals = [];


setInterval(function() {
    var newState = applySignals(gNet, gState, gSignals);
    var nextSignals = sendSignals(gNet, newState, gSignals);

    gState = newState;
    gSignals = nextSignals;
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

    drawCells(gNet.cells);
    drawFibers(gCtx, gNet, gSignals);

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
        gCtx.arc(cell.view.pos.x, cell.view.pos.y, cell.view.size, Math.PI * 0.5, Math.PI * 1.5, false);

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
        gCtx.arc(cell.view.pos.x, cell.view.pos.y, cell.view.size, Math.PI * 0.5, Math.PI * 1.5, true);

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

        gCtx.fillText(String(cell.threshold), cell.view.pos.x - cell.view.size * 0.5, cell.view.pos.y);
    }
}

function drawFibers(ctx, net, signals) {
    // draw quiet fibers
    ctx.strokeStyle = '#000000';
    drawFiberPaths(ctx, net, signals, false);

    // draw quiet connectors
    ctx.fillStyle = '#FFFFFF';
    ctx.strokeStyle = '#000000';
    drawConnectorPaths(ctx, net, signals, false);

    // draw active fibers
    ctx.lineWidth = 2;
    ctx.strokeStyle = '#FF0000';
    drawFiberPaths(ctx, net, signals, true);

    ctx.lineWidth = 1;
    // draw active connectors
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#FF0000';
    drawConnectorPaths(ctx, net, signals, true);
}

function stackedOffset(spread, j, n) {
    return spread * (j - (n - 1) * 0.5);
}

function fiberPoints(fiber, j, n) {
    var spread = 5.0;  
    var yOffset = stackedOffset(spread, j, n);

    var s = new Vec(fiber.from.view.pos.x + fiber.from.view.size, fiber.from.view.pos.y);
    var e = new Vec(fiber.to.view.pos.x - fiber.to.view.size, fiber.to.view.pos.y + yOffset);

    return [s, e];
}

function drawFiberPaths(ctx, net, signals, active) {
    var i, j, n;
    var cell;
    var fiber;
    var points;
    var fiberActive;

    ctx.beginPath(); 
    for (i = 0; i < net.cells.length; ++i)  { 
        cell = net.cells[i];

        n = cell.inputs.length;
        for (j = 0; j < n; ++j) {
            fiber = cell.inputs[j];

            fiberActive = signals[fiber.index] ? true : false;
            if (fiberActive === active) {
                points = fiberPoints(fiber, j, n);

                var fudge = Vec.dist(points[0], points[1]) * 0.4;

                ctx.moveTo(points[0].x, points[0].y);
                ctx.bezierCurveTo(points[0].x + fudge, points[0].y,
                                   points[1].x - fudge, points[1].y,
                                   points[1].x, points[1].y);
            }
        }
    }
    ctx.stroke();
}

function drawConnectorPaths(ctx, net, signals, active) {
    var i, j, n;
    var cell;
    var fiber;
    var points;
    var fiberActive;

    for (i = 0; i < net.cells.length; ++i)  { 
        cell = net.cells[i];

        n = cell.inputs.length;
        for (j = 0; j < n; ++j) {
            fiber = cell.inputs[j];
            fiberActive = signals[fiber.index] ? true : false;

            if (cell.inputTypes[j] === INPUT_INHIBIT && fiberActive === active) {
                points = fiberPoints(fiber, j, n);

                ctx.beginPath(); 
                ctx.arc(points[1].x - 4.0, points[1].y, 4.0, 0.0, Math.PI * 2.0, false);
                ctx.fill();
                ctx.stroke();
            }
        }
    }
}


