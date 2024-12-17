import Init.System.FilePath
import Init.System.IO

import Std.Data.HashMap

inductive Direction
  | North
  | East
  | South
  | West
deriving BEq, Hashable

structure State where
  row : Nat
  col : Nat
  dir : Direction
deriving BEq, Hashable

structure Node where
  state : State
  cost : Nat
  parent : Option Node
deriving BEq

instance : Ord Node where
  compare a b := compare a.cost b.cost

instance : Inhabited Node where
  default := { state := { row := 0, col := 0, dir := Direction.North }, cost := 0, parent := none }

instance : Min Node where
  min a b := if a.cost < b.cost then a else b

structure Queue where
  nodes : Array Node

instance : Inhabited Queue where
  default := { nodes := Array.empty }

def Queue.make (l: List Node): Queue := { nodes := l.toArray }

def Queue.min (q: Queue) : Node :=
  match q.nodes.toList.min? with
  | some n => n
  | none => panic! "empty queue"

def Queue.isEmpty (q: Queue) : Bool :=
  q.nodes.isEmpty

def Queue.deleteMin (q: Queue) : Queue :=
  match q.nodes.toList.min? with
  | some n => { nodes := q.nodes.erase n }
  | none => panic! "empty queue"

def Queue.insert (q: Queue) (n: Node) : Queue :=
  {q with nodes := q.nodes.push n}

def parseMap (input : String) : Array (Array Char) :=
  let lines := input.splitOn "\n"
  lines.map (·.toList.toArray) |>.toArray

def findStart (grid : Array (Array Char)) : Option (Nat × Nat) := do
  for i in [:grid.size] do
    for j in [:grid[i]!.size] do
      if grid[i]![j]! == 'S' then
        return (i, j)
  none

def findEnd (grid : Array (Array Char)) : Option (Nat × Nat) := do
  for i in [:grid.size] do
    for j in [:grid[i]!.size] do
      if grid[i]![j]! == 'E' then
        return (i, j)
  none

def turnCost : Nat := 1000
def moveCost : Nat := 1

def moveForward (state : State) : State :=
  match state.dir with
  | Direction.North => { state with row := state.row - 1 }
  | Direction.South => { state with row := state.row + 1 }
  | Direction.East  => { state with col := state.col + 1 }
  | Direction.West  => { state with col := state.col - 1 }

def turnLeft (state : State) : State :=
  { state with dir :=
    match state.dir with
    | Direction.North => Direction.West
    | Direction.West  => Direction.South
    | Direction.South => Direction.East
    | Direction.East  => Direction.North }

def turnRight (state : State) : State :=
  { state with dir :=
    match state.dir with
    | Direction.North => Direction.East
    | Direction.East  => Direction.South
    | Direction.South => Direction.West
    | Direction.West  => Direction.North }

def isValid (grid : Array (Array Char)) (row col : Nat) : Bool :=
  row < grid.size && col < grid[0]!.size && grid[row]![col]! != '#'

def solve (input : String) : Option (List Node) := do
  let grid := parseMap input
  let (startRow, startCol) ← findStart grid
  let (endRow, endCol) ← findEnd grid

  let start : State := {
    row := startRow
    col := startCol
    dir := Direction.East
  }

  let mut queue := Queue.make [ {state := start, cost := 0, parent:= none} ]
  let mut visited: Std.HashMap State Nat := Std.HashMap.empty

  let mut costToGoal : Option Nat := none

  let mut goalPaths := []

  while !queue.isEmpty && (costToGoal.isNone ∨ queue.min.cost <= costToGoal.get!) do
    -- dbg_trace s!"visited: {visited.size}, queue: {queue.nodes.size}"
    let node := queue.min
    queue := queue.deleteMin
    if visited.contains node.state ∧ visited.get! node.state < node.cost then
      continue

    visited := visited.insert node.state node.cost

    if node.state.row == endRow && node.state.col == endCol then
      goalPaths := node :: goalPaths
      if costToGoal.isNone then
        costToGoal := some node.cost

    -- Try moving forward
    let nextState := moveForward node.state
    if isValid grid nextState.row nextState.col then
      let newCost := node.cost + moveCost
      if !visited.contains nextState || (visited.get! nextState == newCost) then
        queue := queue.insert { state := nextState, cost := newCost , parent := node}

    -- Try turning left
    let leftState := turnLeft node.state
    let leftCost := node.cost + turnCost
    if !visited.contains leftState || (visited.get! leftState == leftCost) then
      queue := queue.insert { state := leftState, cost := leftCost , parent := node}

    -- Try turning right
    let rightState := turnRight node.state
    let rightCost := node.cost + turnCost
    if !visited.contains rightState || (visited.get! rightState == rightCost) then
      queue := queue.insert { state := rightState, cost := rightCost , parent := node}

  if costToGoal.isSome then
    goalPaths
  else
    none

-- We know all of our paths are finite and acyclic, but we can't prove termination without that knowledge
partial def getPathFromNode (n: Node) : List (Nat × Nat) :=
  match n.parent with
  | none => [(n.state.row, n.state.col)]
  | some p => (n.state.row, n.state.col) :: getPathFromNode p

def main : IO Unit := do
  let contents ← IO.FS.readFile "test.txt"
  let paths := solve contents
  match paths with
  | none => IO.println "No path found"
  | some nodes => do
    let positions := nodes.map getPathFromNode
    let all_positions := positions.flatten
    let unique_positions := all_positions.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
    IO.println s!"{unique_positions.length}"
