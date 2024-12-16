import Lean.Data.HashMap

def Position := Nat × Nat
deriving Repr

def Grid := Array (Array Char)

def Position.move (p: Position) (dir: Char) : Option Position :=
  let (i, j) := p
  match dir with
  | '^' => some (i - 1, j)
  | 'v' => some (i + 1, j)
  | '<' => some (i, j - 1)
  | '>' => some (i, j + 1)
  | _ => none

structure State where
  grid : Grid
  robot : Position

def State.getCell (s: State) (p: Position) : Option Char :=
  if p.fst < 0 || p.fst >= s.grid.size || p.snd < 0 || p.snd >= (s.grid.get! 0).size then
    none
  else
    some ((s.grid.get! p.fst).get! p.snd)

def widenMap (input: String) : String := Id.run do
  let mut result := ""
  for c in input.data do
    result := result ++ match c with
      | '#' => "##"
      | 'O' => "[]"
      | '.' => ".."
      | '@' => "@."
      | '\n' => "\n"
      | _ => toString c
  result

def parseGrid (input : String) : Id State := do
  let lines := input.splitOn "\n" |>.filter (·.length > 0)
  let height := lines.length
  let width := if height > 0 then lines[0]!.length else 0
  let grid := lines.map (·.data.toArray)
  let mut robotPos : Position := (0, 0)

  -- Find robot position
  for i in [0:height] do
    for j in [0:width] do
      if grid[i]![j]! == '@' then
        robotPos := (i, j)

  { grid := grid.toArray, robot := robotPos }

def isWall (grid : Grid) (pos : Position) : Bool :=
  let (i, j) := pos
  if i >= grid.size || j >= (grid.get! 0).size then true
  else (grid.get! i).get! j == '#'

def isBox (grid : Grid) (pos : Position) : Bool :=
  let (i, j) := pos
  if i >= grid.size || j >= (grid.get! 0).size then false
  else
    let cell := (grid.get! i).get! j
    cell == '[' || cell == ']'

def dirToV (dir: Char) : (Int × Int) :=
  match dir with
    | '^' => (-1, 0)
    | 'v' => (1, 0)
    | '<' => (0, -1)
    | '>' => (0, 1)
    | _ => (0, 0)

def vAdd (v1: (Int × Int)) (v2: (Int × Int)) : (Int × Int) :=
  (v1.1 + v2.1, v1.2 + v2.2)

def movePosition (pos : Position) (dir : Char) : Option Position :=
  let (i, j) := pos
  let candidate_position := vAdd (i, j) (dirToV dir)
  if candidate_position.fst < 0 || candidate_position.snd < 0 then
    none
  else
    some ⟨candidate_position.fst.toNat, candidate_position.snd.toNat⟩

partial def verticalAffectedBoxHelper (s: State) (last_r: Int) (affected_cols: List Nat) (rDelta : Int) (soFar : List Position) : Id (Option (List Position)) := do
  let numRows := s.grid.size
  let r := last_r + rDelta

  if r < 0 || r >= numRows then
    some soFar -- Should never happen since wall surrounds grid
  else
    let mut l: List Position := []
    let mut new_affected_cols := []
    for c in affected_cols do
      if (s.grid.get! r.toNat).get! c == '#' then
        return none -- Hit wall, whole tree can't move
      else if (s.grid.get! r.toNat).get! c == '['then
        l := ⟨ r.toNat, c ⟩ :: ⟨ r.toNat, (c+1) ⟩ :: l
        new_affected_cols := c :: (c+1) :: new_affected_cols
      else if (s.grid.get! r.toNat).get! c == ']'then
        l := ⟨ r.toNat, c ⟩ :: ⟨ r.toNat, (c-1) ⟩ :: l
        new_affected_cols := c :: (c-1) :: new_affected_cols
    if new_affected_cols.length == 0 then --No more boxes
      some soFar
    else -- Add children of these boxes
      verticalAffectedBoxHelper s r new_affected_cols rDelta (l ++ soFar)


def getAffectedBoxes (s: State) (dir: Char) : Id (List Position) := do
  let mut l := []
  let robotPos := s.robot
  let (rowStep, colStep) := dirToV dir
  let mut r := Int.ofNat robotPos.fst
  let mut c := Int.ofNat robotPos.snd
  let numRows := s.grid.size
  let numCols := (s.grid.get! 0).size

  r := r + rowStep
  c := c + colStep

  if dir == '<' || dir == '>' then
    while r >= 0 && r < numRows && c >= 0 && c < numCols do
      if (s.grid.get! r.toNat).get! c.toNat == '[' || (s.grid.get! r.toNat).get! c.toNat == ']' then
        l := ⟨ r.toNat, c.toNat ⟩ :: l
      else if (s.grid.get! r.toNat).get! c.toNat == '#' then
        return []
      else if (s.grid.get! r.toNat).get! c.toNat == '.' then
        break
      r := r + rowStep
      c := c + colStep
    l
  else
    if r >= 0 && r < numRows && c >= 0 && c < numCols then
      if (s.grid.get! r.toNat).get! c.toNat == '[' then
        let affectedBoxes := verticalAffectedBoxHelper s r [c.toNat, c.toNat+1] rowStep [⟨r.toNat, c.toNat⟩, ⟨r.toNat, (c+1).toNat⟩]
        match affectedBoxes with
        | none => []
        | some affectedBoxes => affectedBoxes
      else if (s.grid.get! r.toNat).get! c.toNat == ']' then
        let affectedBoxes := verticalAffectedBoxHelper s r [c.toNat, c.toNat-1] rowStep [⟨r.toNat, c.toNat⟩, ⟨r.toNat, (c-1).toNat⟩]
        match affectedBoxes with
        | none => []
        | some affectedBoxes => affectedBoxes
      else
        []
    else
      return []


def shiftBoxes (s: State) (dir: Char) (boxes: List Position) : Id State := do
  let mut newGrid := s.grid
  let (rowStep, colStep) := dirToV dir
  for box in boxes do
    let (r, c) := box
    let char := (s.grid.get! r).get! c
    let newR := r + rowStep
    let newC := c + colStep
    newGrid := newGrid.set! r ((newGrid.get! r).set! c '.')
    newGrid := newGrid.set! newR.toNat ((newGrid.get! newR.toNat).set! newC.toNat char)
  { grid := newGrid, robot := s.robot }

def tryMove (state : State) (dir : Char) : Id State := do
  let newRobotPos := movePosition state.robot dir

  match newRobotPos with
  | none => state
  | some newRobotPos =>

  -- If moving into wall, don't move
  if isWall state.grid newRobotPos then
    state
  else
    let mut newGrid := state.grid
    if isBox state.grid newRobotPos then
      let affectedBoxes := getAffectedBoxes state dir
      if affectedBoxes.isEmpty then
        return state
      newGrid := (shiftBoxes state dir affectedBoxes).grid
    let (oldI, oldJ) := state.robot
    let (newI, newJ) := newRobotPos
    newGrid := newGrid.set! oldI ((newGrid.get! oldI).set! oldJ '.')
    newGrid := newGrid.set! newI ((newGrid.get! newI).set! newJ '@')
    { grid := newGrid, robot := newRobotPos }

def calculateGPS (grid : Grid) : Id Nat := do
  let mut sum := 0
  for i in [0:grid.size] do
    for j in [0:(grid.get! 0).size] do
      if (grid.get! i).get! j == '[' then
        sum := sum + (100 * i + j)
  sum

def printState (state : State) : IO Unit := do
  for row in state.grid.toList do
    for c in row do
      IO.print c
    IO.println ""
  IO.println s!"Robot position: {state.robot.fst}, {state.robot.snd}"

def getStateAndMoves (input: String) : (State × List Char) :=
  let parts := input.splitOn "\n\n"
  let initState := parseGrid (widenMap parts[0]!)
  let moves := parts[1]!.replace "\n" "" |>.data
  (initState, moves)

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  let (initState, moves) := getStateAndMoves input
  printState initState

  let mut newState := initState

  printState newState

  for i in [0:moves.length] do
    newState := tryMove newState (moves.get! i)

  printState newState
  IO.println s!"{toString (calculateGPS newState.grid)}"
