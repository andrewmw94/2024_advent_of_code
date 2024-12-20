import AocDay20.Graph

def minImprovement := 100

structure Coord where
  x: Nat
  y: Nat
deriving Repr, BEq, Ord, Hashable

instance : Inhabited Coord where
  default := { x := 0, y := 0 }

structure Grid where
  width: Nat
  height: Nat
  data: Array (Array Char)
  startCoord: Coord
  endCoord: Coord
deriving Repr, BEq

def String.splitLines (s: String) : Array String :=
  s.splitOn "\n" |>.toArray

def findChar (grid: Array (Array Char)) (target: Char) : Option Coord := do
  for i in [0:grid.size] do
    for j in [0:grid[i]!.size] do
      if grid[i]![j]! == target then
        return ⟨j, i⟩
  none

def parseInput (input: String) : Id Grid := do
  let lines := input.splitLines

  -- Ensure we have at least one line
  let height := lines.size
  let width := lines[0]!.length

  -- Convert each line to array of chars and validate width
  let mut data := #[]
  for line in lines do
    data := data.push (line.toList.toArray)

  -- Find start and end positions
  let grid := data
  let startCoord ← findChar grid 'S'
  let endCoord ← findChar grid 'E'

  {
    width := width,
    height := height,
    data := data,
    startCoord := startCoord.get!,
    endCoord := endCoord.get!
  }


def Grid.isCorrupted (self: Grid) (coord: Coord) : Bool :=
  (self.data.get! coord.y).get! coord.x == '#'

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
  height: Nat
  graph: Graph
  startCoord: Coord
  endCoord: Coord

def readProblem (s: String) : Problem :=
  let grid := parseInput s
  let graph := grid.toGraph
  let width := grid.width
  let height := grid.height
  let startCoord := grid.startCoord
  let endCoord := grid.endCoord
  { width, height, graph, startCoord, endCoord }

-- r,c = cost to end with zero cheats remaining
def getCostsToEndZeroCheats (p: Problem) : Id (Array (Array (Option Nat))) := do
  let m := p.graph.dijkstra (coordToNode p.endCoord p.width)
  let mut costs := Array.mkArray p.height (Array.mkArray p.width none)
  for node in m.keys do
    let coord := nodeToCoord node p.width
    let newRow := ((costs.get! coord.y).set! coord.x) (some (m.get! node))
    costs := costs.set! coord.y newRow
  costs

def Coord.toInts (c: Coord) : Int × Int := (c.x, c.y)

def Int.abs (n: Int) : Nat := if n < 0 then (-n).toNat else n.toNat

def manhattanDistance (a: Coord) (b: Coord) : Nat :=
  (((a.x) : Int ) - ((b.x) : Int)).abs + (((a.y) : Int ) - ((b.y) : Int)).abs

def neighborsInNSteps (c: Coord) (n: Nat) (problem: Problem) : Id (List Coord) := do
  let mut l := []
  for dx in [:2*n+1] do
    let dx := -1*(n: Int) + dx
    for dy in [:2*n+1] do
      let dy := -1*(n: Int) + dy
      let x := (c.x: Int) + dx
      let y := (c.y: Int) + dy
      if x >= 0 && y >= 0 && x < problem.width && y < problem.height then
        let nc := { x := x.toNat, y := y.toNat }
        if manhattanDistance c nc <= n then
          l := nc :: l
  l

def countShortCuts (costs : Array (Array (Option Nat))) (problem: Problem) (shortcutLimit : Nat) : Id Nat := do
  let mut count := 0
  for y in [:costs.size] do
    for x in [:(costs.get! 0).size] do
      match ((costs.get! y).get! x) with
      | none => ()
      | some oldCost =>
        let neighbors ← neighborsInNSteps { x := x, y := y } shortcutLimit problem
        let coord := { x := x, y := y }
        for neighbor in neighbors do
          match (costs.get! neighbor.y).get! neighbor.x with
          | some cost =>
            if cost + (minImprovement + (manhattanDistance neighbor coord)) <= oldCost then
              count := Nat.add count 1
              -- dbg_trace s!"{Coord.toInts { x := x, y := y }} -> {Coord.toInts neighbor} saves {oldCost - cost - 2}"
          | none => ()
  count
