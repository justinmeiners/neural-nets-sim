// Created by Justin Meiners
// LICENSE GPL v3.0
// https://raw.githubusercontent.com/justinmeiners/neural-nets-sim/master/LICENSE

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
};

Vec.prototype.len = function() {
    return Math.sqrt(this.lenSqr());
};

Vec.prototype.inBounds = function(min, max) {
    return this.x >= min.x && this.y >= min.y &&
           this.x <= max.x && this.y <= max.y;
};

Vec.prototype.inCircle = function(o, r) {
    return Vec.distSqr(this, o) < r * r;
};

Vec.prototype.add = function(b) {
    this.x += b.x;
    this.y += b.y;
};

Vec.add = function(a, b) {
    return new Vec(a.x + b.x, a.y + b.y);
};

Vec.sub = function(a, b) {
    return new Vec(a.x - b.x, a.y - b.y);
};

Vec.scale = function(a, s) {
    return new Vec(a.x * s, a.y * s);
};

Vec.distSqr = function(a, b) {
    return Vec.sub(a, b).lenSqr();
};

Vec.dist = function(a, b) {
    return Math.sqrt(Vec.distSqr(a, b));
};

Vec.min = function(a, b) {  
    return new Vec(Math.min(a.x, b.x), Math.min(a.y, b.y));
};

Vec.max = function(a, b) {
    return new Vec(Math.max(a.x, b.x), Math.max(a.y, b.y));
}

Vec.dot = function(a, b) {
    return a.x * b.x + a.y * b.y;
}

Vec.bezier = function(t, p1, cp1, cp2, p2) {
    var inv_t = 1.0 - t;
    var a = Vec.scale(p1, inv_t * inv_t * inv_t);
    var b = Vec.scale(cp1, 3.0 * inv_t * inv_t * t);
    var c = Vec.scale(cp2, 3.0 * inv_t * t * t);
    var d = Vec.scale(p2, t * t * t);
    return Vec.add(a, Vec.add(b, Vec.add(c, d)));
}

var ANGLE_EAST = 0;
var ANGLE_WEST = 1;

function CellView(i) {
    Cell.call(this, i);
    this.pos = new Vec(0, 0);
    this.angle = ANGLE_EAST;
}

CellView.prototype = Object.create(Cell.prototype);
CellView.prototype.constructor = CellView;

CellView.prototype.size = 15.0;
CellView.prototype.connectorPadding = 10.0;

CellView.prototype.hits = function(p) {
    return p.inCircle(this.pos, this.size);
};

CellView.prototype.hitsConnectors = function(p) {
    return p.inCircle(this.pos, this.size + this.connectorPadding);
};


function FiberView(i) {
    Fiber.call(this, i);

    // don't set this.
    // Its driven by the inputTypes
    // on cell
    this.ouputIndex = -1;

    // cache of
    // beizer curve points
    // [p0, cp0, cp1, p1]
    this.bezierPoints = new Array(4);
}

FiberView.prototype = Object.create(Fiber.prototype);

// ATTEMPT 1: minimize distance between curve and point
// I did the math for this one. 
// Its relatively easy to get the derviative of
// D(t) = || B(t) - q ||^2
// however actually minimzing that
// requires solving a cubic polynomial
// and I don't want to mess around with Newton's method.

// ATTEMPT 2: Use isPointInStroke
// Maintaining the ctx state is awkward.
// It also doesn't seem to account for lineWidth
// correctly so its not very useable

// ATTEMPT 3: Chop the bezier curve into line segements
// and minimize the distance along each line.



FiberView.prototype.hits = function(q) {
    var p = this.bezierPoints;

    // early rejection
    // with a bounding box
    var min = p.reduce(Vec.min);
    var max = p.reduce(Vec.max);
    
    if (!q.inBounds(min, max)) {
        return false;
    }
    
    // number of segments
    var N = 50;

    // collision radius 
    // around path
    var r = 9.0;
    // cache this
    var rSqr = r * r;

    var x0 = p[0];
    var x1;
    var t;

    // l: vector in direction of line segmeent
    // d: delta from x0 to q
    // c: component of delta on l
    var l;
    var d;
    var c;

    for (var i = 1; i <= N; ++i) {
        t = i / N;

        // line segement from x0 to x1 along the path
        x1 = Vec.bezier(t, p[0], p[1], p[2], p[3]);

        l = Vec.sub(x1, x0);
        d = Vec.sub(q, x0);

        // project delta onto seg
        // two Inv sqrt :(
        c = Vec.scale(l, Vec.dot(l, d) / (l.len() * d.len())); 

        // subtract projection
        // check if below distance
        if (Vec.sub(d, c).lenSqr() < rSqr) {
            return true;
        }
        // save previous point
        x0 = x1;
    }

    return false;
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

    // since the NetView
    // is the visual component
    // we will store the state here
    // even though the underlying
    // structure is immutable over time
    this.state = [];
    this.signals = [];

    this.time = 0;
}

NetView.prototype = Object.create(Net.prototype);
NetView.prototype.constructor = NetView;

NetView.prototype.step = function() {
    var newState = applySignals(this, this.state, this.signals);
    var nextSignals = sendSignals(this, newState, this.signals);

    this.state = newState;
    this.signals = nextSignals;

    ++this.time;
};

NetView.prototype.restart = function() {
    this.state = [];
    this.signals = [];
    this.time = 0;
};


NetView.prototype.addCell = function() {
    var cell = new CellView(this.cells.length);
    this.cells.push(cell);
    return cell;
};

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
};

NetView.prototype.addFiber = function(from, to) {
    var fiber = new FiberView(this.fibers.length);
    fiber.from = from;
    fiber.to = to;
    this.fibers.push(fiber);
    return fiber;
};

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
};

NetView.prototype.addBranch = function() {
    var branch = new BranchView();
    this.branches.push(branch);
    return branch;
};

NetView.prototype.save = function() {
    var d = [];
    var i, j;

    function write(val) {
        d.push(val);
    }

    // add a placeholder for the data length
    write(0);

    write(this.cells.length);
    write(this.fibers.length);

    for (i = 0; i < this.cells.length; ++i) {
        var cell = this.cells[i];

        write(cell.pos.x);
        write(cell.pos.y);
        write(cell.threshold);

        write(cell.inputs.length);
        for (j = 0; j < cell.inputs.length; ++j) {
            write(cell.inputs[j].index);
            write(cell.inputTypes[j] == INPUT_INHIBIT ? INPUT_INHIBIT : INPUT_EXCITE);
        }

        write(cell.outputs.length);
        for (j = 0; j < cell.outputs.length; ++j) {
            write(cell.outputs[j].index);
        }
    }

    for (i = 0; i < this.fibers.length; ++i) {
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
};

NetView.prototype.load = function(base64) {
    var str;
    var i, j;

    try {
        str = atob(base64);
    } catch (e) {
        // a base64 error occurred
        return false;
    }

    var arr8 = new Uint8Array(str.length);

    for (i = 0; i < str.length; ++i) {
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

    for (i = 0; i < this.cells.length; ++i) {
        this.cells[i] = new CellView(i);
    }

    for (i = 0; i < this.fibers.length; ++i) {
        this.fibers[i] = new FiberView(i);
    }

    for (i = 0; i < this.cells.length; ++i) {
        var cell = this.cells[i];

        cell.pos.x = read();
        cell.pos.y = read();
        cell.threshold = read();

        cell.inputs = new Array(read());
        for (j = 0; j < cell.inputs.length; ++j) {
            cell.inputs[j] = this.fibers[read()];
            cell.inputTypes[j] = read();
        }

        cell.outputs = new Array(read());
        for (j = 0; j < cell.outputs.length; ++j) {
            cell.outputs[j] = this.fibers[read()];
        }
    }

    for (i = 0; i < this.fibers.length; ++i) {
        var fiber = this.fibers[i];

        fiber.from = this.cells[read()];
        fiber.to = this.cells[read()];
    }

    return true;
};

// TOOLS
// =====================

function SelectTool(sim, e) {
    this.dragInitial = sim.mousePos;
    this.sim = sim;
}

SelectTool.prototype.mouseUp = function(e) {
    var min = Vec.min(this.dragInitial, this.sim.mousePos);
    var max = Vec.max(this.dragInitial, this.sim.mousePos);

    var sel = this.sim.net.cells.filter(function(cell) {
        return cell.pos.inBounds(min, max);
    });

    this.sim.selection = sel;
};

function MoveTool(sim, e) {
    this.sim = sim;
    this.initialPos = this.sim.mousePos;
    this.previousPos = this.sim.mousePos;
}

MoveTool.prototype.mouseMove = function(e) {
    var i;
    var object;
    var delta = Vec.sub(this.sim.mousePos, this.previousPos);

    for (i = 0; i < this.sim.selection.length; ++i) {
        object = this.sim.selection[i];
        object.pos.add(delta);
    }

    this.previousPos = this.sim.mousePos;
};

MoveTool.prototype.mouseUp = function(e) {

};

function CreateTool(sim, e) {
    var menu = document.getElementById('new-menu');

    this.menu = menu;
    this.menu.style.left = e.pageX + 'px';
    this.menu.style.top = e.pageY + 'px';
    this.menu.classList.add('active');

    this.menu.onclick = function(e) {
        var action;
        var added;
        var canvasLoc = sim.mousePos;
        if (e.target.matches('li')) {
            action = e.target.getAttribute('data-action');

            if (action === 'new-cell') {
                added = sim.net.addCell();
                added.pos = canvasLoc;
            } else if (action === 'new-branch') {
                added = sim.net.addBranch();
                added.pos = canvasLoc;
            }

            menu.classList.remove('active');
        }
    };
}

CreateTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};

function EditCellTool(sim, e, obj) {
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
                sim.net.removeCell(obj);
            }
        }
    };
}

EditCellTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};

function EditFiberTool(sim, e, obj) {
    var menu = document.getElementById('fiber-menu');

    this.obj = obj;

    this.menu = menu;
    this.menu.style.left = e.pageX + 'px';
    this.menu.style.top = e.pageY + 'px';
    this.menu.classList.add('active');

    this.menu.onclick = function(e) {
        var j;
        var action;
        if (e.target.matches('li')) {
            action = e.target.getAttribute('data-action');

            if (action === 'delete') {
                sim.net.removeFiber(obj);
            } else if (action === 'inhibit') {
                // find index in cell
                obj.to.inputTypes[obj.outputIndex] = INPUT_INHIBIT;
            } else if (action === 'excite') {
                obj.to.inputTypes[obj.outputIndex] = INPUT_EXCITE;
            }
        }
    };
}

EditFiberTool.prototype.mouseUp = function(e) {
    this.menu.classList.remove('active');
};

function FiberTool(sim, e, cell) {
    this.sim = sim;
    if (this.sim.mousePos.x < cell.pos.x) {
        this.to = cell;
    } else {
        this.from = cell;
    }
}

FiberTool.prototype.mouseUp = function(e) {
    var mousePos = this.sim.mousePos;

    var hit = this.sim.net.cells.find(function (cell) {
        return cell.hitsConnectors(mousePos);
    });

    if (this.from) {
        this.to = hit;
    } else {
        this.from = hit;
    }

    if (this.to && this.from) {
        var f = this.sim.net.addFiber(this.from, this.to);

        this.from.outputs.push(f);
        this.to.inputs.push(f);
    }
};

// WINDOW AND CONTEXT
// -------------------------
//

function getMousePos(canvas, e) {
    var rect = canvas.getBoundingClientRect();
    return new Vec(e.clientX - rect.left, e.clientY - rect.top);
}

function setupDefaultNet(net) {
    var c1 = net.addCell();
    c1.pos.x = 40;
    c1.pos.y = 50;
    c1.threshold = 0;
    c1.angle = ANGLE_WEST;

    var c2 = net.addCell();
    c2.pos.x = 90;
    c2.pos.y = 50;

    var c3 = net.addCell();
    c3.pos.x = 150;
    c3.pos.y = 50;

    var f1 = net.addFiber(c1, c2);
    var f2 = net.addFiber(c2, c3);
    var f3 = net.addFiber(c3, c1);


    c1.outputs.push(f1);
    c2.inputs.push(f1);

    c2.outputs.push(f2);
    c3.inputs.push(f2);

    c3.outputs.push(f3);
    c1.inputs.push(f3);
    c1.inputTypes.push(INPUT_INHIBIT);
}


function Sim() {
    this.selection = [];
    this.play = true;

    this.mousePos = new Vec(0, 0);

    this.playBtn = document.getElementById('play-btn');
    this.playBtn.onclick = this.togglePlay.bind(this);

    this.stepBtn = document.getElementById('step-btn');
    this.stepBtn.onclick = this.step.bind(this);

    this.restartBtn = document.getElementById('restart-btn');
    this.restartBtn.onclick = this.restart.bind(this);

    this.timeDisplay = document.getElementById('time');
    this.storageInput = document.getElementById('storage-input');

    this.loadBtn = document.getElementById('load-btn');
    this.loadBtn.onclick = this.load.bind(this);

    this.saveBtn = document.getElementById('save-btn');
    this.saveBtn.onclick = this.save.bind(this);

    this.canvas = document.getElementById('main-canvas');
    this.ctx = this.canvas.getContext('2d', { alpha: false });

    this.canvas.onmousedown = this.mouseDown.bind(this);
    this.canvas.onmouseup = this.mouseUp.bind(this);
    this.canvas.onmousemove = this.mouseMove.bind(this);

    window.addEventListener('keydown', (function(e) {
        var num;
        if (e.key === 's') {
            // step hotkey
            this.step();
            e.preventDefault();
        } else if (e.key === ' ') {
            // play/pause hotkey
            this.togglePlay();
            e.preventDefault();
        } else if (e.key.localeCompare('0') >= 0 && e.key.localeCompare('9') <= 0) {
            // numbers for threshold
            num = parseInt(e.key);

            this.selection.forEach(function(c) {
                c.threshold = num;
            });

            e.preventDefault();
        } else if (event.key === 'Delete' ||
                   event.key === 'Backspace') {
            // delete selected cells
            this.deleteSelection();
        }
    }).bind(this));

    this.canvas.oncontextmenu = (function(e) {
        e.preventDefault();

        var mousePos = getMousePos(this.canvas, e);

        var hit = this.net.cells.find(function (cell) {
            return cell.hits(mousePos);
        });

        if (hit) {
            this.tool = new EditCellTool(this, e, hit);
        } else {
            hit = this.net.fibers.find(function (fiber) {
                return fiber.hits(mousePos);
            });
            if (hit) {
                this.tool = new EditFiberTool(this, e, hit);
            } else {
                this.tool = new CreateTool(this, e, hit);
            }
        }
    }).bind(this);

    // create a new net
    this.net = new NetView();

    // handle download
    // if they provided one
    var urlParams = new URLSearchParams(window.location.search);
    var downloadUrl = urlParams.get('d');

    if (downloadUrl && downloadUrl.length > 0) {
        this.download(downloadUrl);
    } else {
        setupDefaultNet(this.net);
    }
}

Sim.prototype.togglePlay = function() {
    this.play = !this.play;

    if (this.play) {
        this.playBtn.innerText = 'Pause';
    } else {
        this.playBtn.innerText = 'Play';
    }
};

Sim.prototype.restart = function() {
    this.selection = [];
    this.net.restart();
    this.timeDisplay.innerText = String(this.net.time);
};

Sim.prototype.download = function(url) {
    var req = new XMLHttpRequest();
    req.open('GET', url);
    req.send();

    req.onerror = function(err) {
        alert(err.message);
    };

    req.onload = (function() {
        this.net.load(req.responseText.trim());
    }).bind(this);
};

Sim.prototype.load = function() {
    if (this.net.load(this.storageInput.value)) {
        this.restart();
    } else {
        alert('The data provided was malformed.');
    }
};

Sim.prototype.save = function() {
    this.storageInput.value = this.net.save();
    this.storageInput.select();
};

Sim.prototype.deleteSelection = function() {
    var net = this.net;
    this.selection.forEach(function(c) {
        net.removeCell(c);
    });

    // clear selection
    this.selection = [];
};

Sim.prototype.step = function() {
    this.net.step();
    this.timeDisplay.innerText = String(this.net.time);
};

Sim.prototype.mouseDown = function(e) {
    var mousePos = getMousePos(this.canvas, e);
    this.mousePos = mousePos;

    var hit = this.net.cells.find(function (cell) {
        return cell.hits(mousePos);
    });

    var connectHit = this.net.cells.find(function (cell) {
        return cell.hitsConnectors(mousePos);
    });

    if (hit) {
        var index = this.selection.indexOf(hit);

        if (index === -1) {
            this.selection = [hit];
        }

        this.tool = new MoveTool(this, e);
    } else if (connectHit) {
        this.tool = new FiberTool(this, e, connectHit);
    } else {
        this.tool = new SelectTool(this, e);
    }
};

Sim.prototype.mouseMove = function(e) {
    this.mousePos = getMousePos(this.canvas, e);

    if (this.tool &&
        this.tool.mouseMove) {
        this.tool.mouseMove(e);
    }
};

Sim.prototype.mouseUp = function(e) {
    this.mousePos = getMousePos(this.canvas, e);

    if (this.tool &&
        this.tool.mouseUp) {
        this.tool.mouseUp(e);
    }

    delete this.tool;
};

var gSim = new Sim();

setInterval(function() {
    drawSim(gSim.ctx, gSim.canvas, gSim);
}, 16);


setInterval(function() {
    if (gSim.play) {
        gSim.step();
    }
}, 500);


// RENDERER
// --------------------------
//
function drawSim(ctx, canvas, sim) {
    clearCanvas(ctx, canvas);

    drawBranches(ctx, sim.net);
    drawFibers(ctx, sim.net);
    drawCells(ctx, sim.net);

    if (sim.tool) {
        if (sim.tool instanceof FiberTool) {
            drawPartialFiber(ctx, sim.tool, sim.mousePos);
        } else if (gSim.tool instanceof SelectTool) {
            drawSelectBox(ctx, sim.tool, sim.mousePos);
        }
    }

    drawHoverRing(ctx, sim.net, sim.mousePos);
}


function clearCanvas(ctx, canvas) {
    ctx.fillStyle = '#EFF0F1';
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

function drawSelectBox(ctx, selectTool, mousePos) {
    ctx.strokeStyle = '#000055';
    ctx.lineWidth = 1;
    ctx.setLineDash([4]);

    var p = [];
    p[0] = selectTool.dragInitial;
    p[1] = mousePos;

    ctx.beginPath();
    ctx.rect(p[0].x, p[0].y, p[1].x - p[0].x, p[1].y - p[0].y);
    ctx.stroke();

    ctx.setLineDash([]);
}


function drawCells(ctx, net) {
    // draw the back of the cells
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#FFFFFF';

    // firing and quiet
    drawCellPaths(ctx, net, false, false);
    drawCellPaths(ctx, net, false, true);

    // draw the front of the cells
    // quiet
    ctx.fillStyle = '#444444';
    drawCellPaths(ctx, net, true, false);

    // firing
    ctx.fillStyle = '#FF0000';
    drawCellPaths(ctx, net, true, true);

    // draw the selected cells
    drawSelection(ctx, net, gSim.selection);

    // draw the text
    drawCellLabels(ctx, net);
}

function drawCellLabels(ctx, net) {
    var i;
    var cell;
    var flip;

    ctx.fillStyle = '#000000';
    ctx.font = '14pt monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];

        if (cell.angle === ANGLE_EAST) {
            flip = -1.0;
        } else {
            flip = 1.0;
        }

        ctx.fillText(String(cell.threshold), cell.pos.x + cell.size * 0.5 * flip, cell.pos.y);
    }
}

// draws the path for half the cell
// ctx options are used to configure front or back style
function drawCellPaths(ctx, net, frontPart, active) {
    var i;
    var cell;
    var cellFiring;

    var clockwise;

    for (i = 0; i < net.cells.length; ++i)  {
        cell = net.cells[i];

        cellFiring = net.state[i] ? true : false;

        if (cellFiring === active) {

            if (cell.angle === ANGLE_EAST) {
                clockwise = frontPart;
            }  else {
                clockwise = !frontPart;
            }

            ctx.beginPath();
            ctx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 0.5, Math.PI * 1.5, clockwise);
            ctx.fill();
            ctx.stroke();
        }
    }
}

function drawSelection(ctx, net, sel) {
    ctx.strokeStyle = '#00AA00';
    ctx.lineWidth = 2;

    var i;
    var cell;

    for (i = 0; i < sel.length; ++i) {
        cell = sel[i];
        ctx.beginPath();
        ctx.arc(cell.pos.x, cell.pos.y, cell.size, Math.PI * 2.0, 0.0, false);
        ctx.stroke();
    }

    ctx.lineWidth = 1;
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

function stackedOffset(spread, j, n) {
    return spread * (j - (n - 1) * 0.5);
}

// returns a list of tuples
// [startPoint, endPoint, type]
function updateFiberPoints(net) {
    var i;
    var j;
    var cell;
    var f;
    var N;
    var fudge;
    var yOffset;
    var p;

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];

        N = cell.inputs.length;
        for (j = 0; j < N; ++j) {
            f = cell.inputs[j];
j
            // cache this for rendering
            f.outputIndex = j;

            yOffset = stackedOffset(7.0, j, N);
            p = f.bezierPoints;

            p[0] = new Vec(f.from.pos.x + f.from.size, f.from.pos.y);
            p[3] = new Vec(f.to.pos.x - f.to.size, f.to.pos.y + yOffset);

            // control points
            if (f.from === f.to) {
                // cell connected to itself
                fudge = f.from.size * 2.0;
                p[1] = new Vec(p[0].x + fudge, p[0].y + fudge);
                p[2] = new Vec(p[3].x - fudge, p[3].y + fudge);
            } else {
                // normal fiber
                fudge = Vec.dist(p[0], p[3]) * 0.4;
                p[1] = new Vec(p[0].x + fudge, p[0].y);
                p[2] = new Vec(p[3].x - fudge, p[3].y); 
            }
        }
    }
}

function drawFibers(ctx, net) {
    updateFiberPoints(net);

    // draw quiet fibers
    ctx.strokeStyle = '#000000';
    drawFiberPaths(ctx, net, false);

    // draw quiet connectors
    ctx.fillStyle = '#FFFFFF';
    ctx.strokeStyle = '#000000';
    drawConnectorPaths(ctx, net, false);

    // draw active fibers
    ctx.lineWidth = 2;
    ctx.strokeStyle = '#FF0000';
    drawFiberPaths(ctx, net, true);

    ctx.lineWidth = 1;
    // draw active connectors
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#FF0000';
    drawConnectorPaths(ctx, net, true);
}

// draw "in progress" fibers for
// the fiber connecting tool
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

function drawFiberPaths(ctx, net, active) {
    var i;
    var fiber;
    var fiberActive;
    var p;

    ctx.beginPath();
    for (i = 0; i < net.fibers.length; ++i)  {
        fiber = net.fibers[i];
        fiberActive = net.signals[fiber.index] ? true : false;

        if (fiberActive !== active) {
            continue;
        }

        p = fiber.bezierPoints;
        ctx.moveTo(p[0].x, p[0].y);
        ctx.bezierCurveTo(p[1].x, p[1].y,
                          p[2].x, p[2].y,
                          p[3].x, p[3].y);
    }
    ctx.stroke();
}

function drawConnectorPaths(ctx, net, active) {
    var i;
    var fiber;
    var fiberActive;
    var tuple;
    var p;
    var outputType;

    for (i = 0; i < net.fibers.length; ++i) {
        fiber = net.fibers[i];
        fiberActive = net.signals[fiber.index] ? true : false;
        p = fiber.bezierPoints;

        outputType = fiber.to.inputTypes[fiber.outputIndex];

        if (outputType === INPUT_INHIBIT && fiberActive === active) {
            ctx.beginPath();
            ctx.arc(p[3].x - 4.0, p[3].y, 4.0, 0.0, Math.PI * 2.0, false);
            ctx.fill();
            ctx.stroke();
        }
    }
}
