"use strict";
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

function Label(i) {
    this.text = "";
    this.index = i;
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
    this.labels = [];
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
    return this;
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
};

Vec.dot = function(a, b) {
    return a.x * b.x + a.y * b.y;
};

Vec.bezier = function(t, p1, cp1, cp2, p2) {
    var tInv = 1.0 - t;
    var a = Vec.scale(p1, tInv * tInv * tInv);
    var b = Vec.scale(cp1, 3.0 * tInv * tInv * t);
    var c = Vec.scale(cp2, 3.0 * tInv * t * t);
    var d = Vec.scale(p2, t * t * t);
    return Vec.add(a, Vec.add(b, Vec.add(c, d)));
};

var ANGLE_EAST = 0;
var ANGLE_WEST = 1;

Vec.fromAngle = function(angle) {
    if (angle === ANGLE_EAST) {
        return new Vec(1.0, 0.0);
    } else if (angle === ANGLE_WEST) {
        return new Vec(-1.0, 0.0);
    } else {
        return new Vec(0.0, 0.0);
    }
};

function CellView(i) {
    Cell.call(this, i);
    this.pos = new Vec(0, 0);
    this.angle = ANGLE_EAST;
}

CellView.radius = 15.0;

CellView.prototype = Object.create(Cell.prototype);
CellView.prototype.constructor = CellView;

CellView.prototype.radius = CellView.radius;
CellView.prototype.connectorPadding = 12.0;

CellView.prototype.hits = function(p) {
    return p.inCircle(this.pos, this.radius);
};

CellView.prototype.hitsConnectors = function(p) {
    return p.inCircle(this.pos, this.radius + this.connectorPadding);
};

function LabelView(i) {
    Label.call(this, i);
    this.pos = new Vec(0,0);
}
LabelView.prototype.bounds = function(fontsize) {
    var width = this.text.length * fontsize;
    var height = fontsize;
    var pad = 2;

   return {
       min: new Vec(this.pos.x - width / 2.0 - pad, this.pos.y - height / 2.0 - pad),
       max: new Vec(this.pos.x + width / 2.0 + pad, this.pos.y + height / 2.0 + pad)
   }
};

LabelView.prototype.hits = function(mousePos, fontsize) {
    var bounds = this.bounds(fontsize);
    return mousePos.inBounds(bounds.min, bounds.max);
}

function FiberView(i) {
    Fiber.call(this, i);

    // don't set this.
    // Its driven by the inputTypes
    // on cell
    this.outputIndex = -1;

    // cache of
    // bezier curve points
    // [p0, cp0, cp1, p1]
    this.bezierPoints = new Array(4);
}

FiberView.prototype = Object.create(Fiber.prototype);

// ATTEMPT 1: minimize distance between curve and point
// I did the math for this one. 
// It is relatively easy to get the derivative of
// D(t) = || B(t) - q ||^2
// however actually minimizing that
// requires solving a cubic polynomial
// and I don't want to mess around with Newton's method.

// ATTEMPT 2: Use isPointInStroke
// Maintaining the ctx state is awkward.
// It also doesn't seem to account for lineWidth
// correctly so its not very usable

// ATTEMPT 3: Chop the bezier curve into line segments
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

    // l: vector in direction of line segment
    // d: delta from x0 to q
    // c: component of delta on l
    var l;
    var d;
    var c;

    for (var i = 1; i <= N; ++i) {
        t = i / N;

        // line segment from x0 to x1 along the path
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

BranchView.prototype.radius = 5.0;

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

NetView.prototype.addTextLabel = function() {
    var label = new LabelView(this.labels.length);
    this.labels.push(label);
    return label;
};

NetView.prototype.removeLabel = function(toDelete) {
    var last = this.labels.pop();
    if (last !== toDelete) {
        this.labels[toDelete.index] = last;
        last.index = toDelete.index;
    }
};


NetView.prototype.addCell = function() {
    var cell = new CellView(this.cells.length);
    this.cells.push(cell);
    return cell;
};

NetView.prototype.removeCell = function(toDelete) {
    // detach all fibers
    //
    // slicing is to make sure
    // the array isn't modified underneath us
    toDelete.inputs.slice().forEach(this.removeFiber.bind(this));
    toDelete.outputs.slice().forEach(this.removeFiber.bind(this));

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
    toDelete.to.inputTypes.splice(i, 1);

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

// SERIALIZATION
// =====================

var SERIALIZATION_VERSION = 2;
var SERIALIZATION_SUCCESS = true;
var SERIALIZATION_INVALID_BASE64 = -1;
var SERIALIZATION_INVALID_LENGTH = -2;
var SERIALIZATION_INVALID_VERSION = -3;

NetView.prototype.save = function() {
    var d = [];
    var i, j;

    function write(val) {
        d.push(val);
    }

    // add a placeholder for the data length
    write(0);
    write(SERIALIZATION_VERSION);

    write(this.cells.length);
    write(this.fibers.length);
    write(this.labels.length);

    for (i = 0; i < this.cells.length; ++i) {
        var cell = this.cells[i];

        write(cell.pos.x);
        write(cell.pos.y);
        write(cell.threshold);
        write(cell.angle);

        write(cell.inputs.length);
        for (j = 0; j < cell.inputs.length; ++j) {
            write(cell.inputs[j].index);
            write(cell.inputTypes[j] === INPUT_INHIBIT ? INPUT_INHIBIT : INPUT_EXCITE);
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

    for (i = 0; i < this.labels.length; ++i) {
        var label = this.labels[i];
        write(label.pos.x);
        write(label.pos.y);
        write(label.text.length);
    }

    this.labels.forEach(function(l) {
        for (i = 0; i < l.text.length; ++i) {
            write(l.text.charCodeAt(i));
        }
    });

    // prefix the data with the number of bytes so that load can detect malformed data
    d[0] = d.length * 2; 

    var arr16 = new Uint16Array(d);
    var arr8 = new Uint8Array(arr16.buffer);
    var str = String.fromCharCode.apply(null, arr8);

    return btoa(str);
};

NetView.prototype.load = function(base64) {
    var str;
    var i, j;
    var cell;
    var fiber;
    var label;

    try {
        str = atob(base64);
    } catch (e) {
        return SERIALIZATION_INVALID_BASE64;
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

    function readBlock(length) {
        var x = d.slice(cursor + 1, cursor + 1 + length);
        cursor += length;
        return x;
    }

    if (read() !== d.length * 2) {
        return SERIALIZATION_INVALID_LENGTH;
    }

    var version = read();
    if (version < 1 || version > SERIALIZATION_VERSION) {
        return SERIALIZATION_INVALID_VERSION;
    }

    this.cells = new Array(read());
    this.fibers = new Array(read());

    for (i = 0; i < this.cells.length; ++i) {
        this.cells[i] = new CellView(i);
    }

    for (i = 0; i < this.fibers.length; ++i) {
        this.fibers[i] = new FiberView(i);
    }

    if (version > 1) {
        this.labels = new Array(read());
        for (i = 0; i < this.labels.length; ++i) {
            this.labels[i] = new LabelView(i);
        }
    }

    for (i = 0; i < this.cells.length; ++i) {
        cell = this.cells[i];

        cell.pos.x = read();
        cell.pos.y = read();
        cell.threshold = read();
        cell.angle = read();

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
        fiber = this.fibers[i];
        fiber.from = this.cells[read()];
        fiber.to = this.cells[read()];
    }

    if (version > 1) {
        var textLengths = [];
        for (i = 0; i < this.labels.length; ++i) {
            label = this.labels[i];
            label.pos.x = read();
            label.pos.y = read();
            textLengths.push(read());
        }

        for (i = 0; i < textLengths.length; ++i) {
            var characters = readBlock(textLengths[i]);
            this.labels[i].text = String.fromCharCode.apply(null, characters);
        }
    }

    return SERIALIZATION_SUCCESS;
};

function messageForSerializationError(error) {
    switch (error) {
        case SERIALIZATION_SUCCESS:
            return null;

        case SERIALIZATION_INVALID_BASE64:
            return 'Could not load net. Failed to base64 decode.';

        case SERIALIZATION_INVALID_LENGTH:
            return 'Could not load net. Incorrect checksum or length.';

        case SERIALIZATION_INVALID_VERSION:
            return 'Could not load net. Unsupported serialization version.';

        default:
            return 'Could not load net. Unexpected serialization error.';
    }
}

// TOOLS
// =====================

function SelectTool(sim, e) {
    this.dragInitial = sim.mousePos;
    this.sim = sim;
}

SelectTool.prototype.mouseUp = function(e) {
    var min = Vec.min(this.dragInitial, this.sim.mousePos);
    var max = Vec.max(this.dragInitial, this.sim.mousePos);

    var all = this.sim.net.cells.concat(this.sim.net.labels);

    this.sim.selection = all.filter(function (obj) {
        return obj.pos.inBounds(min, max);
    });
};

function MoveTool(sim, e) {
    this.sim = sim;
    this.initial = this.sim.mousePos;
    this.previous = this.sim.mousePos;
}

MoveTool.prototype.mouseMove = function(e) {
    var i;
    var object;
    var delta = Vec.sub(this.sim.mousePos, this.previous);

    for (i = 0; i < this.sim.selection.length; ++i) {
        object = this.sim.selection[i];
        object.pos.add(delta);
    }

    this.previous = this.sim.mousePos;
};

MoveTool.prototype.mouseUp = function(e) {

};

MoveTool.prototype.cancel = function() {
    var i;
    var object;
    var invDelta = Vec.sub(this.initial, this.sim.mousePos);

    for (i = 0; i < this.sim.selection.length; ++i) {
        object = this.sim.selection[i];
        object.pos.add(invDelta);
    }    
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
            } else if (action == 'new-label') {
                added = sim.net.addTextLabel();
                added.pos = canvasLoc;
                sim.editLabelText(added);
            }
            menu.classList.remove('active');
        }
    };
}

CreateTool.prototype.mouseUp = function(e) {
    this.cancel();
};

CreateTool.prototype.cancel = function() {
    this.menu.classList.remove('active');
};

function EditTextTool(sim, text, callback) {
    this.input = EditTextTool.createTextInputElement(sim);
    this.input.value = text;

    this.input.focus();
    this.input.select();

    this.input.addEventListener("keyup", (function(e) {
        if (e.key === 'Enter' || e.key === 'Return') {
            this.input.blur();
            callback(e.target.value);
        } else if (e.key == 'Escape') {
            this.input.blur();
        }
    }).bind(this));

    this.input.addEventListener("focusout", this.cancel.bind(this));
}

EditTextTool.prototype.cancel = function() {
    this.input.remove();
};

EditTextTool.createTextInputElement = function(sim){
    var dom = document.createElement("INPUT");
    dom.setAttribute("type", "text");
    dom.style.position = "absolute";
    var rect = sim.canvas.getBoundingClientRect();
    dom.style.top = (sim.mousePos.y + rect.top).toString() + "px";
    dom.style.left = (sim.mousePos.x + rect.left).toString() + "px";
    document.body.appendChild(dom);
    return dom;
};

function EditLabelTool(sim, e, label){
    var menu = document.getElementById('label-menu');

    this.menu = menu;
    this.menu.style.left = e.pageX + 'px';
    this.menu.style.top = e.pageY + 'px';
    this.menu.classList.add('active');

    this.menu.onclick = function(e) {
        var action;
        if(e.target.matches('li')){
            action = e.target.getAttribute('data-action');

            if (action === 'edit'){
                sim.editLabelText(label);
            } else if (action === 'delete') {
                sim.net.removeLabel(label);
            }
        }
        menu.classList.remove('active');
    }

    this.input = null;
}

EditLabelTool.prototype.mouseUp = function(e) {
    this.cancel();
};

EditLabelTool.prototype.cancel = function() {
    this.menu.classList.remove('active');

};

function EditCellTool(sim, e, obj) {
    var menu = document.getElementById('cell-menu');

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
                // delete selected cells
                sim.deleteSelection();  
            } else if (action === 'flip') {

                sim.selection.forEach(function(fiber) {
                    if (fiber.angle === ANGLE_EAST) {
                        fiber.angle = ANGLE_WEST;
                    } else {
                        fiber.angle = ANGLE_EAST;
                    }
                });
            }
        }

        menu.classList.remove('active');
    };
}

EditCellTool.prototype.mouseUp = function(e) {
    this.cancel();
};

EditCellTool.prototype.cancel = function() {
    this.menu.classList.remove('active');
};

function EditFiberTool(sim, e, obj) {
    var inputType;
    var menu = document.getElementById('fiber-menu');

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
                sim.net.removeFiber(obj);
            } else if (action === 'toggle') {
                // find index in cell
                if (obj.to.inputTypes[obj.outputIndex] === INPUT_INHIBIT) {
                    inputType = INPUT_EXCITE;
                } else {
                    inputType = INPUT_INHIBIT;
                }
                
                obj.to.inputTypes[obj.outputIndex] = inputType;
            }
        }

        menu.classList.remove('active');
    };
}

EditFiberTool.prototype.mouseUp = function(e) {
    this.cancel();
};

EditFiberTool.prototype.cancel = function() {
    this.menu.classList.remove('active');
};

function FiberTool(sim, e, cell) {
    this.sim = sim;
    this.initial = this.sim.mousePos;

    var dir = Vec.sub(this.sim.mousePos, cell.pos);
 
    if (Vec.dot(dir, Vec.fromAngle(cell.angle))  > 0.0) {
        this.from = cell;
    } else {
        this.to = cell;
    }
}

FiberTool.prototype.mouseUp = function(e) {
    var mousePos = this.sim.mousePos;

    var hit = this.sim.net.cells.find(function (cell) {
        return cell.hitsConnectors(mousePos);
    });

    if (!hit) {
        // didn't click anything
        return;
    }

    if (this.from) {
        this.to = hit;
    } else {
        this.from = hit;
    }

    if (this.from === this.to) {
        // connecting to ourselves

        // we need some distance to make sure this isn't
        // an accidental click
        if (Vec.sub(this.sim.mousePos, this.initial).lenSqr() < 
            CellView.radius * CellView.radius) {
            return;
        }
    }

    var f = this.sim.net.addFiber(this.from, this.to);
    this.from.outputs.push(f);
    this.to.inputs.push(f);
    this.to.inputTypes.push(INPUT_EXCITE);
};



// WINDOW AND CONTEXT
// -------------------------

function getMousePos(canvas, e) {
    var rect = canvas.getBoundingClientRect();
    return new Vec(e.clientX - rect.left, e.clientY - rect.top);
}

var DefaultNet = "dgICAAgACAAGAEoB6QAAAAAAAQAFAAEAAwAAAAEABgASAmsAAQAAAAEAAAAAAAEAAgDqAbIAAQAAAAEAAQAAAAEAAwB6AtwAAwAAAAMAAgAAAAMAAAAHAAAAAQAEAAABLAEBAAEAAQAEAAAAAQAFAAcC8AABAAAAAQAGAAAAAQAHALwBLQIAAAAAAAAAAFgCFQIBAAAAAAAAAAAAAQAAAAIAAQADAAIAAwADAAQABAAAAAAABQAFAAMAAgIzACsA7gBlACQAxQDfAA4AfgGtAS0A8gHoASUA1ABOAR0AUwBlAGwAZQBjAHQAIABhAG4AZAAgAGQAcgBhAGcAIABuAGUAdQByAG8AbgBzACAAdwBpAHQAaAAgAHQAaABlACAAbABlAGYAdAAgAG0AbwB1AHMAZQBTAGUAdAAgAHQAaAByAGUAcwBoAG8AbABkACAAdwBpAHQAaAAgAG4AdQBtAGIAZQByACAAawBlAHkAcwAgACgAMAAtADkAKQAwACAAYQBsAHcAYQB5AHMAIABmAGkAcgBlAHMAUgBpAGcAaAB0ACAAYwBsAGkAYwBrACAAbwBuACAAZgBpAGIAZQByAHMAIABhAG4AZAAgAG4AZQB1AHIAbwBuAHMAIABmAG8AcgAgAG8AcAB0AGkAbwBuAHMAQwBsAGkAYwBrACAAYQBuAGQAIABkAHIAYQBnACAAZgBpAGIAZQByAHMAIABiAGUAdAB3AGUAZQBuACAAbgBlAHUAcgBvAG4AcwB1AG4AbABlAHMAcwAgAGkAdAAgAHIAZQBjAGUAaQB2AGUAcwAgAGEAbgAgAGkAbgBoAGkAYgBpAHQA";

function Sim() {
    this.selection = [];
    this.play = true;

    this.mousePos = new Vec(0, 0);
    this.fontsize = 14.0;

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

    this.canvas.onselectstart = function() {
        return false;
    };

    window.addEventListener('keydown', (function(e) {
        var num;

        if (document.activeElement !== document.body || e.metaKey) {
            // do nothing in case the text field is focused or the user is holding a modifier
        } else if (e.key === 's') {
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

            this.selection.forEach(function(obj) {
                if (obj instanceof CellView) {
                    obj.threshold = num;
                }
            });

            e.preventDefault();
        } else if (event.key === 'Delete' ||
                   event.key === 'Backspace') {
            // delete selected cells
            this.deleteSelection();
            e.preventDefault();
        } else if (event.key === 'Escape') {
            if (this.tool && this.tool.cancel) {
                this.tool.cancel();
                delete this.tool;
            }
            e.preventDefault();
        }
    }).bind(this));

    this.canvas.oncontextmenu = (function(e) {
        e.preventDefault();

        if (this.tool && this.tool.cancel) {
            this.tool.cancel();
            delete this.tool;
        }

        var mousePos = getMousePos(this.canvas, e);

        var hit = this.net.cells.find(function (cell) {
            return cell.hits(mousePos);
        });

        if (hit) {
            this.tool = new EditCellTool(this, e, hit);
        } else {
            var fontsize = this.fontsize;
            hit = this.net.labels.find(function (label) {
                return label.hits(mousePos, fontsize);
            });

            if (hit) {
                this.tool = new EditLabelTool(this, e, hit);
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
        this.net.load(DefaultNet);
    }
};

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
        alert('Could not download net. Unknown XHR network error.');
    };

    req.onload = (function() {
        var ret;

        if ( req.status >= 200 && req.status < 300) {
            ret = this.net.load(req.responseText.trim());

            if (ret !== SERIALIZATION_SUCCESS) {
                alert(messageForSerializationError(ret));
            }
        } else {
            alert('Could not download net. Error status: ' + req.status);
        }
    }).bind(this);
};

Sim.prototype.load = function() {
    var ret = this.net.load(this.storageInput.value);

    if (ret === SERIALIZATION_SUCCESS) {
        this.restart();
    } else {
        alert(messageForSerializationError(ret));
    }
};

Sim.prototype.save = function() {
    this.storageInput.value = this.net.save();
    this.storageInput.select();
};

Sim.prototype.deleteSelection = function() {
    var net = this.net;
    this.selection.forEach(function(obj) {
        if (obj instanceof CellView) {
            net.removeCell(obj);
        } else if (obj instanceof LabelView) {
            net.removeLabel(obj);
        }
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

    if (this.tool && this.tool.cancel) {
        this.tool.cancel();
        delete this.tool;
    }

    var hit = this.net.cells.find(function (cell) {
        return cell.hits(mousePos);
    });

    var connectHit = this.net.cells.find(function (cell) {
        return cell.hitsConnectors(mousePos);
    });
    
    var fontSize = this.fontsize;
    var labelHit = this.net.labels.find(function (label) {
        return label.hits(mousePos,fontSize);
    });

    if (hit) {
        var index = this.selection.indexOf(hit);

        if (index === -1) {
            this.selection = [hit];
        }

        this.tool = new MoveTool(this, e);
    } else if (labelHit) { 
        var index = this.selection.indexOf(labelHit);

        if (index === -1) {
            this.selection = [labelHit];
        }

        this.tool = new MoveTool(this, e);
        
    } else if (connectHit) {
        this.tool = new FiberTool(this, e, connectHit);
    } else {
        this.tool = new SelectTool(this, e);
    }
};

Sim.prototype.editLabelText = function(label) {
    if (this.tool && this.tool.cancel) {
        this.tool.cancel();
        delete this.tool;
    }
    var net = this.net;
    this.tool = new EditTextTool(this, label.text, function(val) {
        if (!val) {
            net.removeLabel(label);
        } else {
            label.text = val;
        }
    });
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

    drawFibers(ctx, sim.net);
    drawCells(ctx, sim.net, sim.fontsize);

    if (sim.tool) {
        if (sim.tool instanceof FiberTool) {
            drawPartialFiber(ctx, sim.tool, sim.mousePos);
        } else if (gSim.tool instanceof SelectTool) {
            drawSelectBox(ctx, sim.tool, sim.mousePos);
        }
    }

    drawHoverRing(ctx, sim.net, sim.mousePos);
    drawTextLabels(ctx, sim.net, sim.selection, sim.fontsize);
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
        if (cell.hitsConnectors(mousePos) &&
            !cell.hits(mousePos)) {

            radius = cell.radius + cell.connectorPadding;
            ctx.strokeStyle = '#0000FF';
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
    ctx.strokeStyle = '#009900';
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

function drawTextLabels(ctx, net, selection, fontsize){
    ctx.font =  fontsize.toString() + 'pt monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    var marked = {};

    for (i = 0; i < selection.length; ++i) {
        if (selection[i] instanceof LabelView) {
            marked[selection[i].index] = true;
        }
    }



    /*
        var bounds = sel[i].bounds(fontsize);

        ctx.beginPath();
        ctx.moveTo(bounds.min.x, bounds.max.y);
        ctx.lineTo(bounds.max.x, bounds.max.y);
        ctx.stroke();
        */

    var label;
    var i;
    for (i = 0; i < net.labels.length; i++){
        label = net.labels[i];

        if (marked[i]) {
            ctx.fillStyle = '#009900';
        } else {
            ctx.fillStyle = '#000000';
        }
        ctx.fillText(net.labels[i].text, label.pos.x, label.pos.y);
    }
}

function drawCells(ctx, net, labelFontSize) {
    // draw the front of the cells
    // quite
    ctx.fillStyle = '#444444';
    ctx.strokeStyle = '#000000';
    drawCellFronts(ctx, net, false);

    // firing
    ctx.fillStyle = '#FF0000';
    drawCellFronts(ctx, net, true);

    // draw the back of the cells  
    // firing and quiet
    ctx.fillStyle = '#FFFFFF';
    drawCellBacks(ctx, net);

    ctx.strokeStyle = '#FFFFFF';
    drawCellArrows(ctx, net);

    // draw the selected cells
    ctx.strokeStyle = '#009900';
    drawCellSelection(ctx, net, gSim.selection, labelFontSize);

    // draw the text
    drawCellLabels(ctx, net);
}

function drawCellLabels(ctx, net) {
    var i;
    var cell;
    var dir;
    var labelPos;

    ctx.fillStyle = '#000000';
    ctx.font = '14pt monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];
        dir = Vec.fromAngle(cell.angle);

        labelPos = Vec.sub(cell.pos, Vec.scale(dir, cell.radius * 0.5));
        ctx.fillText(String(cell.threshold), labelPos.x, labelPos.y);
    }
}

function drawCellFronts(ctx, net, active) {
    var i;
    var cell;
    var cellFiring;
    var clockwise;

    for (i = 0; i < net.cells.length; ++i)  {
        cell = net.cells[i];
        clockwise = (cell.angle === ANGLE_EAST);
        cellFiring = net.state[i] ? true : false;

        if (cellFiring !== active) {
            continue;
        }

        ctx.beginPath();
        ctx.arc(cell.pos.x, cell.pos.y, cell.radius, Math.PI * 0.5, Math.PI * 1.5, clockwise);
        ctx.fill();
        ctx.stroke();
    }
}

// draws the path for half the cell
// ctx options are used to configure front or back style
function drawCellBacks(ctx, net) {
    var i;
    var cell;
    var clockwise;

    for (i = 0; i < net.cells.length; ++i)  {
        cell = net.cells[i];
        clockwise = cell.angle !== ANGLE_EAST;

        ctx.beginPath();
        ctx.arc(cell.pos.x, cell.pos.y, cell.radius, Math.PI * 0.5, Math.PI * 1.5, clockwise);
        ctx.fill();
        ctx.stroke();
    }
}

function drawCellArrows(ctx, net) {
    var i;
    var cell;
    var dir;

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];
        dir = Vec.fromAngle(cell.angle);

        ctx.beginPath();
        ctx.moveTo(cell.pos.x + dir.x * 4.0, cell.pos.y + cell.radius * 0.4);
        ctx.lineTo(cell.pos.x + dir.x * cell.radius * 0.6, cell.pos.y);
        ctx.lineTo(cell.pos.x + dir.x * 4.0, cell.pos.y - cell.radius * 0.4);
        ctx.stroke();
    }
}


function drawCellSelection(ctx, net, sel, fontsize) {
    var i;
    var cell;

    ctx.lineWidth = 2;
    for (i = 0; i < sel.length; ++i) {
        if (sel[i] instanceof CellView) {
            cell = sel[i];
            ctx.beginPath();
            ctx.arc(cell.pos.x, cell.pos.y, cell.radius, Math.PI * 2.0, 0.0, false);
            ctx.stroke();
        }
    }
    ctx.lineWidth = 1;
}

/*
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
*/

function stackedOffset(spread, j, n) {
    return spread * (j - (n - 1) * 0.5);
}

// returns a list of tuples
// [startPoint, endPoint, type]
function updateFiberPoints(net) {
    var i, j, N;
    var cell;
    var f;
    var fudge;
    var yOffset;
    var p;
    var fromDir, toDir;

    for (i = 0; i < net.cells.length; ++i) {
        cell = net.cells[i];
        N = cell.inputs.length;

        for (j = 0; j < N; ++j) {
            f = cell.inputs[j];
            // cache this for rendering
            f.outputIndex = j;

            // rotation vectors
            fromDir = Vec.fromAngle(f.from.angle);
            toDir = Vec.fromAngle(f.to.angle);

            yOffset = stackedOffset(7.0, j, N);
            p = f.bezierPoints;

            p[0] = Vec.add(f.from.pos, Vec.scale(fromDir, f.from.radius));
            p[3] = Vec.add(Vec.add(f.to.pos, Vec.scale(toDir, -f.to.radius)), new Vec(0.0, yOffset));

            // control points
            if (f.from === f.to) {
                // cell connected to itself
                fudge = f.from.radius * 1.6;
                p[1] = Vec.add(Vec.add(f.from.pos, Vec.scale(fromDir, fudge * 1.5)), new Vec(0.0, -fudge * 2.0));
                p[2] = Vec.add(Vec.add(f.to.pos, Vec.scale(toDir, -fudge * 3.0)), new Vec(0.0, -fudge));
            } else {
                // normal fiber
                fudge = Vec.dist(p[0], p[3]) * 0.5;

                p[1] = Vec.add(f.from.pos, Vec.scale(fromDir, fudge));
                p[2] = Vec.add(f.to.pos, Vec.scale(toDir, -fudge));
            }
        }
    }
}

function drawFibers(ctx, net) {
    updateFiberPoints(net);

    // draw quiet fibers
    ctx.lineWidth = 1;
    ctx.strokeStyle = '#000000';
    drawFiberPaths(ctx, net, false);

    // draw active fibers
    ctx.lineWidth = 2;
    ctx.strokeStyle = '#FF0000';
    drawFiberPaths(ctx, net, true);

    // draw quiet connectors
    ctx.lineWidth = 1;
    ctx.fillStyle = '#FFFFFF';
    ctx.strokeStyle = '#000000';
    drawConnectorPaths(ctx, net, false);

    // draw active connectors
    ctx.lineWidth = 1;
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#FF0000';
    drawConnectorPaths(ctx, net, true);
}

// draw "in progress" fibers for
// the fiber connecting tool
function drawPartialFiber(ctx, tool, mousePos) {
    var p = [];
    var dir;

    if (tool.from) {
        dir = Vec.fromAngle(tool.from.angle);
        p[0] = Vec.add(tool.from.pos, Vec.scale(dir, tool.from.radius));
        p[3] = mousePos;
    } else {
        dir = Vec.fromAngle(tool.to.angle); 
        p[0] = mousePos;
        p[3] = Vec.add(tool.to.pos, Vec.scale(dir, -tool.to.radius));
    }

    var fudge = Vec.dist(p[0], p[3]) * 0.4;
    p[1] = Vec.add(p[0], Vec.scale(dir, fudge));
    p[2] = Vec.add(p[3], Vec.scale(dir, -fudge));

    ctx.strokeStyle = '#000000';
    ctx.lineWidth = 2;
    ctx.setLineDash([2]);

    ctx.beginPath();
    ctx.moveTo(p[0].x, p[0].y);
    ctx.bezierCurveTo(p[1].x, p[1].y,
                      p[2].x, p[2].y,
                      p[3].x, p[3].y);
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
    var p;
    var dir;
    var outputType;
    var radius = 4.0;
    var origin;

    for (i = 0; i < net.fibers.length; ++i) {
        fiber = net.fibers[i];
        fiberActive = net.signals[fiber.index] ? true : false;
        p = fiber.bezierPoints;

        outputType = fiber.to.inputTypes[fiber.outputIndex];

        if (outputType === INPUT_INHIBIT && fiberActive === active) {
            dir = Vec.fromAngle(fiber.to.angle);
            origin = Vec.add(p[3], Vec.scale(dir, -radius));

            ctx.beginPath();
            ctx.arc(origin.x, origin.y, radius, 0.0, Math.PI * 2.0, false);
            ctx.fill();
            ctx.stroke();
        }
    }
}
