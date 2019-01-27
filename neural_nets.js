// SIMULATION
// ------------------------

var INPUT_EXCITE = 0;
var INPUT_INHIBIT = 1;

function Cell(i) {
    this.index = i;
    this.inputs = [];
    this.inputTypes = [];
    this.outputs = [];
    this.threshold = 1;
}

function Fiber(i) {
    this.index = i;
    this.from = null;
    this.to = null;
}

function Branch(i) {
    this.index = i;
    this.input = null;
    this.outputs = [];
}

function Net() {
    this.cells = [];
    this.fibers = [];
    this.branches = [];
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

function CellView(i) {
    Cell.call(this, i);
    this.pos = new Vec(0, 0);
}

CellView.prototype = Object.create(Cell.prototype);
CellView.prototype.constructor = CellView;

CellView.prototype.size = 15.0;
CellView.prototype.connectorPadding = 10.0;

CellView.prototype.hits = function(p) {
    return p.inCircle(this.pos, this.size);
}

CellView.prototype.hitsConnectors = function(p) {
    return p.inCircle(this.pos, this.size + this.connectorPadding);
};


function BranchView() {
    Branch.call(this);
    this.pos = new Vec(0, 0);
}

BranchView.prototype = Object.create(Branch.prototype);
BranchView.prototype.constructor = BranchView;

BranchView.prototype.size = 5.0;

function NetView() {
    Net.call(this);
}

NetView.prototype = Object.create(Net.prototype);
NetView.prototype.constructor = NetView;

NetView.prototype.addCell = function() {
    var cell = new CellView(this.cells.length);
    this.cells.push(cell);
    return cell;
}

NetView.prototype.removeCell = function(toDelete) {
    // detach all fibers
    toDelete.inputs.forEach(this.removeFiber.bind(this));
    toDelete.outputs.forEach(this.removeFiber.bind(this));

    // algorithm Ryan thought up
    // 1. when you delete an item
    //    there is one unused index. 
    //    (at the slot you deleted.)
    // 2. set the last cells index
    //    to the available one.
    // 3. Shrink the array length by one
    var last = this.cells.pop();
    if (last !== toDelete) {
        this.cells[toDelete.index] = last;
        last.index = toDelete.index;
    }
    // now toDelete is no good
}

NetView.prototype.addFiber = function(from, to) {
    var fiber = new Fiber(this.fibers.length);
    fiber.from = from;
    fiber.to = to;
    this.fibers.push(fiber);
    return fiber;
}

NetView.prototype.removeFiber = function(toDelete) {
    var i;
    // remove from "from" outputs
    i = toDelete.from.outputs.indexOf(toDelete);
    toDelete.from.outputs.splice(i, 1);
    
    // remove from "to" inputs
    i = toDelete.to.inputs.indexOf(toDelete);
    toDelete.to.inputs.splice(i, 1);

    // see removeCell
    var last = this.fibers.pop();
    if (last !== toDelete) {
        this.fibers[toDelete.index] = last;
        last.index = toDelete.index;
    }
}

NetView.prototype.addBranch = function() {
    var branch = new BranchView();
    this.branches.push(branch);
    return branch;
}

NetView.prototype.save = function() {
    var d = [];

    function write(val) {
        d.push(val);
    }

    // add a placeholder for the data length
    write(0);

    write(this.cells.length);
    write(this.fibers.length);

    for (var i = 0; i < this.cells.length; ++i) {
        var cell = this.cells[i];

        write(cell.pos.x);
        write(cell.pos.y);
        write(cell.threshold);

        write(cell.inputs.length);
        for (var j = 0; j < cell.inputs.length; ++j) {
            write(cell.inputs[j].index);
            write(cell.inputTypes[j] == INPUT_INHIBIT ? INPUT_INHIBIT : INPUT_EXCITE);
        }

        write(cell.outputs.length);
        for (var j = 0; j < cell.outputs.length; ++j) {
            write(cell.outputs[j].index);
        }
    }

    for (var i = 0; i < this.fibers.length; ++i) {
        var fiber = this.fibers[i];

        write(fiber.from.index);
        write(fiber.to.index);
    }

    // prefix the data with the length so that load can detect malformed data
    d[0] = d.length;

    var arr16 = new Uint16Array(d);
    var arr8 = new Uint8Array(arr16.buffer);
    var str = String.fromCharCode.apply(null, arr8);

    return btoa(str);
}

NetView.prototype.load = function(base64) {
    var str;

    try {
        str = atob(base64);
    }
    catch (e) {
        // a base64 error occurred
        return false;
    }

    var arr8 = new Uint8Array(str.length);

    for (var i = 0; i < str.length; ++i) {
        arr8[i] = str.charCodeAt(i);
    }

    var d = new Uint16Array(arr8.buffer);
    var cursor = -1;

    function read() {
        return d[++cursor];
    }

    if (read() != d.length) {
        // the data was in the wrong format or has been clipped
        return false;
    }

    this.cells = new Array(read());
    this.fibers = new Array(read());

    for (var i = 0; i < this.cells.length; ++i) {
        this.cells[i] = new CellView(i);
    }

    for (var i = 0; i < this.fibers.length; ++i) {
        this.fibers[i] = new Fiber(i);
    }

    for (var i = 0; i < this.cells.length; ++i) {
        var cell = this.cells[i];

        cell.pos.x = read();
        cell.pos.y = read();
        cell.threshold = read();

        cell.inputs = new Array(read());
        for (var j = 0; j < cell.inputs.length; ++j) {
            cell.inputs[j] = this.fibers[read()];
            cell.inputTypes[j] = read();
        }

        cell.outputs = new Array(read());
        for (var j = 0; j < cell.outputs.length; ++j) {
            cell.outputs[j] = this.fibers[read()];
        }
    }

    for (var i = 0; i < this.fibers.length; ++i) {
        var fiber = this.fibers[i];

        fiber.from = this.cells[read()];
        fiber.to = this.cells[read()];
    }

    return true;
}

// TOOLS
// =====================

function MoveTool(e, hit) {
    this.dragInitial = new Vec(0, 0);
    this.dragStart;
    this.selection = [];

    var mousePos = getMousePos(gCanvas, e);

    this.dragInitial = mousePos;
    this.dragStart = hit.pos;
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
            object.pos = Vec.add(this.dragStart, delta);
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
                added.pos = canvasLoc;
            } else if (action === 'new-branch') {
                added = gNet.addBranch();
                added.pos = canvasLoc;
            }

            menu.classList.remove('active');
        }
    };
}

CreateTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};

function EditTool(e, obj) {
    var menu = document.getElementById('edit-menu');

    this.obj = obj;

    this.menu = menu;
    this.menu.style.left = e.pageX + 'px';
    this.menu.style.top = e.pageY + 'px';
    this.menu.classList.add('active');

    this.menu.onclick = function(e) {
        var action;
        if (e.target.matches('li')) {
            action = e.target.getAttribute('data-action');

            if (action === 'delete') {
                console.log(obj);
                gNet.removeCell(obj);
            }
        }
    };
}

EditTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};

function FiberTool(e, cell) {
    var mousePos = getMousePos(gCanvas, e);

    if (mousePos.x < cell.pos.x) {
        this.to = cell;
    } else {
        this.from = cell;
    }
}

FiberTool.prototype.mouseUp = function(e) {
    var mousePos = getMousePos(gCanvas, e);

    var hit = gNet.cells.find(function (cell) {
        return cell.hitsConnectors(mousePos);
    });

    if (this.from) {
        this.to = hit;
    } else {
        this.from = hit;
    }

    if (this.to && this.from) {
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
        return cell.hits(mousePos);
    });
    
    if (hit) {
        gController.tool = new MoveTool(e, hit);
    } else {

        var connectHit = gNet.cells.find(function (cell) {
            return cell.hitsConnectors(mousePos);
        });

        if (connectHit) {
            gController.tool = new FiberTool(e, connectHit);
        }
    }
}

function mouseMoveHandler(e) {
    gController.mousePos = getMousePos(gCanvas, e);

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
        return cell.hits(mousePos);
    }); 

    if (hit) {
        gController.tool = new EditTool(e, hit);
    } else {
        gController.tool = new CreateTool(e);
    }
};

var gNet = new NetView();

var c1 = gNet.addCell();
c1.pos.x = 40;
c1.pos.y = 50;
c1.threshold = 0;

var c2 = gNet.addCell();
c2.pos.x = 90;
c2.pos.y = 50;

var c3 = gNet.addCell();
c3.pos.x = 150;
c3.pos.y = 50;

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
    drawSim(gCtx, gCanvas, gController.mousePos);
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
function drawSim(ctx, canvas, mousePos) {
    clearCanvas(ctx, canvas);

    drawCells(ctx, gNet, gState);
    drawBranches(ctx, gNet);
    drawFibers(ctx, gNet, gSignals);

    if (gController.tool) {
        if (gController.tool instanceof FiberTool) {
            drawPartialFiber(ctx, gController.tool, mousePos);
        }
    }
 
    drawHoverRing(ctx, gNet, mousePos);
}


function clearCanvas(ctx, canvas) {
    ctx.fillStyle = '#D0E7F9';
    ctx.beginPath();
    ctx.rect(0, 0, canvas.width, canvas.height);
    ctx.closePath();
    return ctx.fill();
}

function drawHoverRing(ctx, net, mousePos) {
    var i;
    var cell;
    var radius;
    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];
        if (cell.hitsConnectors(mousePos)) {

            radius = cell.size + cell.connectorPadding;
            ctx.strokeStyle = '#003300';
            ctx.lineWidth = 1;
            ctx.setLineDash([4]);

            ctx.beginPath(); 
            ctx.arc(cell.pos.x, cell.pos.y, radius, 0.0, Math.PI * 2.0, false);

            ctx.stroke();
            ctx.lineWidth = 1;
            ctx.setLineDash([]);
        }
    } 
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
        ctx.fillText(String(cell.threshold), cell.pos.x - cell.size * 0.5, cell.pos.y);
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
            ctx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 0.5, Math.PI * 1.5, front);
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
        ctx.arc(b.pos.x, b.pos.y, 5.0, 0.0, Math.PI * 2.0, false);
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

    var s = new Vec(fiber.from.pos.x + fiber.from.size, fiber.from.pos.y);
    var e = new Vec(fiber.to.pos.x - fiber.to.size, fiber.to.pos.y + yOffset);

    return [s, e];
}

// for the fiber connecting tool
function drawPartialFiber(ctx, tool, mousePos) {
    var p = [];
    if (tool.from) {
        p[0] = new Vec(tool.from.pos.x + tool.from.size, tool.from.pos.y);
        p[1] = mousePos;  
    } else {
        p[0] = mousePos;
        p[1] = new Vec(tool.to.pos.x - tool.to.size, tool.to.pos.y);
    }

    var fudge = Vec.dist(p[0], p[1]) * 0.4;

    ctx.strokeStyle = '#000000';
    ctx.lineWidth = 2;
    ctx.setLineDash([2]);

    ctx.beginPath(); 
    ctx.moveTo(p[0].x, p[0].y);
    ctx.bezierCurveTo(p[0].x + fudge, p[0].y,
                      p[1].x - fudge, p[1].y,
                      p[1].x, p[1].y);
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
                    fudge = cell.size * 2.0;
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

