import AocDay18.Graph

structure Coord where
  x: Nat
  y: Nat
deriving Repr, BEq, Ord, Hashable

def parseInput (input: String) : Array Coord :=
  let lines := input.splitOn "\n"
  lines.foldl (fun acc line =>
    let coords := line.splitOn ","
    match coords with
    | [x, y] =>
      let xNum := x.trim.toNat!
      let yNum := y.trim.toNat!
      acc.push { x := xNum, y := yNum }
    | _ => acc
  ) #[]

structure Grid where
  width: Nat
  height: Nat
  corrupted: Array Coord
deriving Repr, BEq

def Grid.isCorrupted (self: Grid) (coord: Coord) : Bool :=
  self.corrupted.any (fun c => c == coord)

def Grid.isValid (self: Grid) (coord: Coord) : Bool :=
  coord.x < self.width &&
  coord.y < self.height &&
  !self.isCorrupted coord

def coordToNode (coord: Coord) (width: Nat) : Node :=
  { id := coord.x + coord.y * width }

def nodeToCoord (node: Node) (width: Nat) : Coord :=
  { x := node.id % width, y := node.id / width }

def Grid.toGraph (self: Grid) : Graph :=
  let edges := (List.range (self.width * self.height)).toArray
  let initialEdges := edges.map (fun _ => [])

  let finalEdges := (List.range self.height).foldl (fun acc y =>
    (List.range self.width).foldl (fun innerAcc x =>
      let curr := { x := x, y := y }
      if !self.isCorrupted curr then
        let neighbors := [
          some { x := x + 1, y := y },
          if x > 0 then some { x := x - 1, y := y } else none,
          some { x := x, y := y + 1 },
          if y > 0 then some { x := x, y := y - 1 } else none
        ].filterMap id
        let validNeighbors := neighbors.filter (self.isValid ·)
        let edges := validNeighbors.map (fun dest =>
          { src := (coordToNode curr self.width), dest := (coordToNode dest self.width), weight := 1 })
        innerAcc.set! (coordToNode curr self.width).id edges
      else
        innerAcc
    ) acc
  ) initialEdges

  { edges := finalEdges }


structure Problem where
  width: Nat
  graph: Graph
  startNode: Node
  endNode: Node

def readProblem (s: String) (width height: Nat) (numObs : Option Nat) : Problem :=
  let coords := parseInput s
  let grid: Grid := match numObs with
    | some numObs => { width, height, corrupted := coords.take numObs }
    | none => { width, height, corrupted := coords }
  let graph := grid.toGraph
  let startNode := coordToNode { x := 0, y := 0 } grid.width
  let endNode := coordToNode { x := (width-1), y := (height-1) } grid.width
  { width, graph, startNode, endNode }
