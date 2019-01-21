## Simulation

### Data Structures

***Cell***
- input fibers[]
- input fiber types[] (whether inhibitatory or not)
- output fiber
- threshold (N)
- firing (boolean)

***Branch***
- (no fire delay)
- input fiber
- output fibers[]

***Fiber***
- input
- output

## State
- which cells were firing
- does not depend on wiring, or anything else

### Algorithms

```
propogate: state<t> -> state<t+1>
    for each cell that is firing:
        mark all connected fibers as firing
        stop firing

    for each cell
        look at all connected fibers
        if inhibitory
            stay quite
        if above threshold
            then fire
```

## Display

## Data Structures

CellView
- x, y
- angle
- size

FiberView
- path points

BranchView
- x, y

## Algorithms

```
render: state<t> -> state<t+1> -> View -> Canvas
    

```

## Interaction

Selection
- drag rectangle to select cells and branches

Create Cell/Branch
- Right click with no selection, and pick "Create Cell/Branch"

Delete Cell/branch
- Right click with selection, and delete

Create a fiber
- each cell and branch shows the output slots 
- the user clicks with their mouse on the area around the output,
  and starts dragging to create a fiber. When they are over an input
  area they release the mouse to create a connection.

Delete a fiber
- 


Rotate a cell


Adjust cell threshold

## Tools


Select Mode
- click on nodes to move around
- type number to change threshold

Cell Mode
- click to make nodes
- right click to delete nodes

Branch Mode
- click to make branches
- right click to delete branches

Wiring Mode
- click and drag to make wires
- right click to delete wires


