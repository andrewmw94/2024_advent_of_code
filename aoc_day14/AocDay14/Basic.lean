structure Robot where
  pos_x : Int
  pos_y : Int
  vel_x : Int
  vel_y : Int
deriving Inhabited

def moveRobot (r : Robot) (width height : Int) (numSteps : Int) : Robot :=
  let new_x := (r.pos_x + r.vel_x * numSteps) % width
  let new_y := (r.pos_y + r.vel_y * numSteps) % height
  { r with pos_x := new_x, pos_y := new_y }

def getQuadrant (r : Robot) (width height : Int) : Option Int :=
  let mid_x := width / 2
  let mid_y := height / 2
  if r.pos_x = mid_x || r.pos_y = mid_y then
    none
  else
    some $
      if r.pos_x < mid_x then
        if r.pos_y < mid_y then 1     -- Top-left
        else 3                        -- Bottom-left
      else
        if r.pos_y < mid_y then 2     -- Top-right
        else 4                        -- Bottom-right

def parsePosition (s : String) : Option (Int × Int) :=
  let parts := s.drop 2              -- Remove "p="
  match parts.splitOn "," with
  | [x, y] => some (x.toInt!, y.toInt!)
  | _ => none

def parseVelocity (s : String) : Option (Int × Int) :=
  let parts := s.drop 2              -- Remove "v="
  match parts.splitOn "," with
  | [x, y] => do
    let x ← x.toInt?
    let y ← y.toInt?
    some (x, y)
  | _ => none

def parseRobot (line : String) : Option Robot := do
  let parts := line.splitOn " "
  match parts with
  | [pos, vel] => do
    let (px, py) ← parsePosition pos
    let (vx, vy) ← parseVelocity vel
    some { pos_x := px, pos_y := py, vel_x := vx, vel_y := vy }
  | _ => none

def readProblem (input : String) : List Robot :=
  input.splitOn "\n"
    |>.map (λ line => parseRobot line)
    |>.filterMap id

def countInQuadrant (robots : List Robot) (width height quadrant : Int) : Int :=
  robots.foldl (λ count r =>
    match getQuadrant r width height with
    | some q => if q = quadrant then count + 1 else count
    | none => count
  ) 0

def calculateSafetyFactor (robots : List Robot) (width height : Int) : Int :=
  let q1 := countInQuadrant robots width height 1
  let q2 := countInQuadrant robots width height 2
  let q3 := countInQuadrant robots width height 3
  let q4 := countInQuadrant robots width height 4
  q1 * q2 * q3 * q4

def solvePuzzle (robots: List Robot) (steps: Int) : Int :=
  let width := 101
  let height := 103

  let finalRobots := robots.map (λ r => moveRobot r width height steps)
  calculateSafetyFactor finalRobots width height
