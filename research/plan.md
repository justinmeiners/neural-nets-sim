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



