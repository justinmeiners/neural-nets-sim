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

Vec.prototype.inRect = function(min, max) {
    return this.x >= min.x && this.y >= min.y &&
           this.x <= max.x && this.y <= max.y;  
};

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

function CellView() {
    this.pos = new Vec(0, 0);
}

CellView.prototype.size = 15.0;

CellView.prototype.hits = function(p) {
    return p.inCircle(this.pos, this.size);
}

CellView.prototype.hitsConnectors = function(p) {
    var offset = 10.0;
    return p.inCircle(this.pos, this.size + offset);
};

function BranchView() {
    this.pos = new Vec(0, 0);
    this.size = 5.0;
}


// SIMULATION
// ------------------------

var INPUT_EXCITE = 0;
var INPUT_INHIBIT = 1;

function Cell() {
    this.inputs = [];
    this.inputTypes = [];
    this.outputs = [];
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
    var i, j;
    var firing;
    var cell;
    var fiber;
    var signals = [];

    var fiberVisitor = function(fiber) {
        signals[fiber.index] = true;
    };

    // send signals from cells to fibers
    for (i = 0; i < net.cells.length; ++i) {
        firing = state[i];
        if (firing) {
            cell = net.cells[i];
            for (j = 0; j < cell.outputs.length; ++j) {
                fiber = cell.outputs[j];
                visitFibers(fiber, fiberVisitor);
            }
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



function MoveTool(e, hit) {
    this.dragInitial = new Vec(0, 0);
    this.dragStart;
    this.selection = [];

    var mousePos = getMousePos(gCanvas, e);

    this.dragInitial = mousePos;
    this.dragStart = hit.view.pos;
    this.selection = [hit];
}

MoveTool.prototype.mouseMove = function(e) {
    var i;
    var object;
    var mousePos = getMousePos(gCanvas, e);
    var delta = Vec.sub(mousePos, this.dragInitial);

    if (this.selection.length > 0) {
        for (i = 0; i < this.selection.length; ++i) {
            object = this.selection[i];
            object.view.pos = Vec.add(this.dragStart, delta);
        }
    }
};

MoveTool.prototype.mouseUp = function(e) {

};

function CreateTool(e) {
    var menu = document.getElementById('new-menu');

    this.menu = menu;
    this.menu.style.left = e.pageX + 'px';
    this.menu.style.top = e.pageY + 'px';
    this.menu.classList.add('active');

    this.menu.onclick = function(e) {
        var action;
        var added;
        var canvasLoc;
        if (e.target.matches('li')) {
            action = e.target.getAttribute('data-action');

            canvasLoc = getMousePos(gCanvas, e);

            if (action === 'new-cell') {
                added = gNet.addCell();
                added.view.pos = canvasLoc;
            } else if (action === 'new-branch') {
                added = gNet.addBranch();
                added.view.pos = canvasLoc;
            }

            menu.classList.remove('active');
        }
    };
}

CreateTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};


function FiberTool(e, cell) {
    this.from = cell;
    this.pos = getMousePos(gCanvas, e);
}

FiberTool.prototype.mouseMove = function(e) {
    this.pos = getMousePos(gCanvas, e);
};

FiberTool.prototype.mouseUp = function(e) {
    var mousePos = getMousePos(gCanvas, e);

    this.to = gNet.cells.find(function (cell) {
        return cell.view.hitsConnectors(mousePos);
    });

    if (this.to) {
        var f = gNet.addFiber(this.from, this.to);

        this.from.outputs.push(f)
        this.to.inputs.push(f);
    }
};




// WINDOW AND CONTEXT
// -------------------------

var STATE_SELECT = 0;
var STATE_DRAG = 1;

function Controller() {
    this.state = STATE_SELECT;

    this.time = 0;
    this.play = true;
    this.playBtn = document.getElementById('play-btn');
    this.playBtn.onclick = (function(e) {
        this.togglePlay();
    }).bind(this);

    this.stepBtn = document.getElementById('step-btn');
    this.stepBtn.onclick = function(e) {
        step();
    };

    this.resetBtn = document.getElementById('reset-btn');
    this.resetBtn.onclick = (function(e) {
        gState = [];
        this.time = 0; 
        this.timeDisplay.innerText = '0';
    }).bind(this);

    this.timeDisplay = document.getElementById('time'); 
}

Controller.prototype.togglePlay = function() {
    this.play = !this.play;

    if (this.play) {
        this.playBtn.innerText = 'Pause';
    } else {
        this.playBtn.innerText = 'Play';
    }
};

var gController = new Controller();

var gCanvas = document.getElementById('main-canvas');

var gCtx = gCanvas.getContext('2d', { alpha: false });

function keyDownHandler(e) {
    var num;
    if (e.key === 's') {
        // step hotkey
        step();
        e.preventDefault();
    } else if (e.key === ' ') {
        // play/pause hotkey  
        gController.togglePlay();
        e.preventDefault();
    } else if (e.key.localeCompare('0') >= 0 && e.key.localeCompare('9') <= 0) {
        // numbers for threshold
        num = parseInt(e.key);
        
        e.preventDefault();
    }
}

function getMousePos(canvas, e) {
    var rect = canvas.getBoundingClientRect();
    return new Vec(e.clientX - rect.left, e.clientY - rect.top);
}

function mouseDownHandler(e) {
    var mousePos = getMousePos(gCanvas, e);

    var hit = gNet.cells.find(function (cell) {
        return cell.view.hits(mousePos);
    });
    
    if (hit) {
        gController.tool = new MoveTool(e, hit);
    } else {

        var connectHit = gNet.cells.find(function (cell) {
            return cell.view.hitsConnectors(mousePos);
        });

        if (connectHit) {
            gController.tool = new FiberTool(e, connectHit);
        }
    }
}

function mouseMoveHandler(e) {
    if (gController.tool &&
        gController.tool.mouseMove) {
        gController.tool.mouseMove(e);
    }
}

function mouseUpHandler(e) {
    if (gController.tool &&
        gController.tool.mouseUp) {
        gController.tool.mouseUp(e);
    }

    delete gController.tool;
}

gCanvas.onmousedown = mouseDownHandler;
gCanvas.onmousemove = mouseMoveHandler;
gCanvas.onmouseup = mouseUpHandler;

window.addEventListener('keydown', keyDownHandler);

gCanvas.oncontextmenu = function(e) {
    e.preventDefault();

    var mousePos = getMousePos(gCanvas, e);

    var hit = gNet.cells.find(function (cell) {
        return cell.view.hits(mousePos);
    }); 

    if (!hit) {
        gController.tool = new CreateTool(e);
    }
};

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


c1.outputs.push(f1)
c2.inputs.push(f1);

c2.outputs.push(f2);
c3.inputs.push(f2);

c3.outputs.push(f3);
c1.inputs.push(f3);
c1.inputTypes.push(INPUT_INHIBIT);

var gState = [];
var gSignals = [];

setInterval(function() {
    simDraw();
}, 16);


setInterval(function() {
    if (gController.play) {
        step();
    }
}, 500);

function step() {
    var newState = applySignals(gNet, gState, gSignals);
    var nextSignals = sendSignals(gNet, newState, gSignals);

    gState = newState;
    gSignals = nextSignals;

    ++gController.time;
    gController.timeDisplay.innerText = String(gController.time);
}

// RENDERER
// --------------------------
//
function simDraw() {
    clearCanvas(gCtx, gCanvas);

    drawCells(gCtx, gNet, gState);
    drawBranches(gCtx, gNet);
    drawFibers(gCtx, gNet, gSignals);

    if (gController.tool) {
        if (gController.tool instanceof FiberTool) {
            drawPartialFiber(gCtx, gController.tool);
        }
    }
}


function clearCanvas(ctx, canvas) {
    ctx.fillStyle = '#D0E7F9';
    ctx.beginPath();
    ctx.rect(0, 0, canvas.width, canvas.height);
    ctx.closePath();
    return ctx.fill();
}

function drawCells(ctx, net, state) {
    var i;
    var cell;

    // draw the back of the cells
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#FFFFFF';

    // firing and quiet
    drawCellPaths(ctx, net, state, false, false);
    drawCellPaths(ctx, net, state, false, true);


    // draw the front of the cells
    // quiet
    ctx.fillStyle = '#444444';
    drawCellPaths(ctx, net, state, true, false);

    // firing
    ctx.fillStyle = '#FF0000';
    drawCellPaths(ctx, net, state, true, true);
   
    // draw the text 
    ctx.fillStyle = '#000000';
    ctx.font = '14pt monospace';
    ctx.textAlign = "center";
    ctx.textBaseline = "middle"; 

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];
        ctx.fillText(String(cell.threshold), cell.view.pos.x - cell.view.size * 0.5, cell.view.pos.y);
    }
}

function drawCellPaths(ctx, net, state, front, active) {
    var i;
    var cell;
    var cellFiring;

    for (i = 0; i < net.cells.length; ++i)  { 
        cell = net.cells[i];

        cellFiring = state[i] ? true : false;

        if (cellFiring === active) {
            ctx.beginPath();
            ctx.arc(cell.view.pos.x, cell.view.pos.y, cell.view.size, Math.PI * 0.5, Math.PI * 1.5, front);

            ctx.fill();
            ctx.stroke();
        }
   }
}

function drawBranches(ctx, net) {
    var i;
    var b;
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#000000';

    for (i = 0; i < net.branches.length; ++i) {
        b = net.branches[i];
        
        ctx.beginPath(); 
        ctx.arc(b.view.pos.x, b.view.pos.y, 5.0, 0.0, Math.PI * 2.0, false);
        ctx.fill();
        ctx.stroke();
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
    var spread = 7.0;  
    var yOffset = stackedOffset(spread, j, n);

    var s = new Vec(fiber.from.view.pos.x + fiber.from.view.size, fiber.from.view.pos.y);
    var e = new Vec(fiber.to.view.pos.x - fiber.to.view.size, fiber.to.view.pos.y + yOffset);

    return [s, e];
}

// for the fiber connecting tool
function drawPartialFiber(ctx, tool) {
    var p1 = new Vec(tool.from.view.pos.x + tool.from.view.size, tool.from.view.pos.y);
    var p2 = tool.pos;

    var fudge = Vec.dist(p1, p2) * 0.4;

    ctx.strokeStyle = '#000000';
    ctx.lineWidth = 2;
    ctx.setLineDash([2, 2]);

    ctx.beginPath(); 
    ctx.moveTo(p1.x, p1.y);
    ctx.bezierCurveTo(p1.x + fudge, p1.y,
                      p2.x - fudge, p2.y,
                      p2.x, p2.y);
    ctx.stroke();
    ctx.lineWidth = 1;
    ctx.setLineDash([]);
}

function drawFiberPaths(ctx, net, signals, active) {
    var i, j, n;
    var cell;
    var fiber;
    var points;
    var fiberActive;
    var fudge;

    ctx.beginPath(); 
    for (i = 0; i < net.cells.length; ++i)  { 
        cell = net.cells[i];

        n = cell.inputs.length;
        for (j = 0; j < n; ++j) {
            fiber = cell.inputs[j];

            fiberActive = signals[fiber.index] ? true : false;
            if (fiberActive === active) {
                points = fiberPoints(fiber, j, n);

                if (fiber.from === fiber.to) {
                    // cell connected to itself
                    fudge = cell.view.size * 2.0;
                    ctx.moveTo(points[0].x, points[0].y);
                    ctx.bezierCurveTo(points[0].x + fudge, points[0].y + fudge,
                                      points[1].x - fudge, points[1].y + fudge,
                                      points[1].x, points[1].y);

                } else {
                    // normal fiber
                    fudge = Vec.dist(points[0], points[1]) * 0.4;
                    ctx.moveTo(points[0].x, points[0].y);
                    ctx.bezierCurveTo(points[0].x + fudge, points[0].y,
                                       points[1].x - fudge, points[1].y,
                                       points[1].x, points[1].y);
                }
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

