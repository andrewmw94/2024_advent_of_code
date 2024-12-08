inductive Heading where
  | North
  | East
  | South
  | West
deriving Repr, BEq

def Heading.toStep (h : Heading) : Int × Int :=
  match h with
  | Heading.North => (0, 1)
  | Heading.East => (1, 0)
  | Heading.South => (0, -1)
  | Heading.West => (-1, 0)

def Heading.rotate (h : Heading) : Heading :=
  match h with
  | Heading.North => Heading.East
  | Heading.East => Heading.South
  | Heading.South => Heading.West
  | Heading.West => Heading.North

structure Grid where
  data : Array (Array Char)
  extraObs : Option (Int × Int)

def Grid.getCellInner (g : Grid) (x : Int) (y : Int) : Option Char := do
  if y < 0 ∨ y >= g.data.size then
    none
  else
  let row := g.data.get! y.toNat
  if x < 0 ∨ x >= row.size then
    none
  else
    some (row.get! x.toNat)

def Grid.getCell (g : Grid) (x : Int) (y : Int) : Option Char := do
  match g.extraObs with
  | some p => if (x, y) == p then some '#' else getCellInner g x y
  | _ => getCellInner g x y

structure Pose where
  x : Int
  y : Int
  heading : Heading
deriving Repr, BEq

structure State where
  grid : Grid
  pose : Pose
  history: List Pose


def readState (input : String) : Id State := do
  let rows := input.splitOn "\n"
  -- Convert each row into an Array of characters
  let mut grid := #[]
  let mut invertedPose := {x:=0, y:= 0, heading:= Heading.North : Pose}

  for row in rows do
    let chars := row.toList
    if row.any (fun c => ¬(c == '.' ∨ c == '#')) then
      let x := chars.findIdx (fun c => ¬(c == '.' ∨ c == '#'))
      let y := grid.size
      let heading := match chars.get! x with
        | '^' => Heading.North
        | 'v' => Heading.South
        | '<' => Heading.West
        | '>' => Heading.East
        | _ => Heading.North
      invertedPose := {x, y, heading}
    grid := grid.push (chars.map (λ c => if c == '#' then '#' else '.')).toArray

  let pose := {invertedPose with y := (grid.size : Int) - invertedPose.y - 1 }
  return {grid := {data := grid.reverse, extraObs := none}, pose, history := [pose]}

inductive moveResults where
  | newState : State → moveResults
  | leftGrid : moveResults
  | cycle : moveResults

def getNextState (s : State) : moveResults :=
  let (dx, dy) := s.pose.heading.toStep
  let x := s.pose.x + dx
  let y := s.pose.y + dy
  let heading := s.pose.heading
  let newCell := s.grid.getCell x y
  let newPose : Option Pose := match newCell with
    | some '.' => some {x, y, heading}
    | some '#' => some {x:=s.pose.x, y:=s.pose.y, heading:=heading.rotate}
    | _ => none
    match newPose with
    | some p =>
      if s.history.contains p then
        .cycle
      else
        .newState {s with pose := p, history := p :: s.history}
    | _ => .leftGrid

-- Part 1 assumes the guard always leaves the grid.
-- Lean, doesn't know this, so it can't prove termination.
-- Adding this (the knowledge that the guard always leaves on our inputs) as an assumption can be done by using `partial`.
-- For this version, you can comment out lines 82 and 83 above.
--
-- We actually can prove termination if we define `getNextState` to stop once a cycle is reached
-- To prove termination, we need to show that the number of unvisited poses decreases each iteration.
-- `getFutureStates` returns a list of future states until we leave the grid or reach a cycle.
partial def getFutureStates (s : State) : List State :=
  match getNextState s with
  | .newState s' => s' :: getFutureStates s'
  | _ => []
