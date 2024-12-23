import Init.System.FilePath
import Init.System.IO
import AocDay21

structure Coord where
  x : Int
  y : Int
deriving Repr

instance : Inhabited Coord := ⟨{ x := 0, y := 0 }⟩

inductive Move where
  | Up : Move
  | Down : Move
  | Left : Move
  | Right : Move
  | Press : Move
deriving Repr, BEq

instance : Inhabited Move := ⟨Move.Press⟩

def Move.toString : Move -> String
  | Move.Up => "^"
  | Move.Down => "v"
  | Move.Left => "<"
  | Move.Right => ">"
  | Move.Press => "A"

instance : ToString Move := ⟨Move.toString⟩


def charToMove (s: Char) : Move :=
  match s with
  | '^' => Move.Up
  | 'v' => Move.Down
  | '<' => Move.Left
  | '>' => Move.Right
  | 'A' => Move.Press
  | _ => panic! s!"Invalid move {s}"

def Move.toDeltaCoord (m: Move) : Coord :=
  match m with
  | Move.Up => { x := 0, y := -1}
  | Move.Down => { x := 0, y := 1 }
  | Move.Left => { x := -1, y := 0 }
  | Move.Right => { x := 1, y := 0 }
  | Move.Press => { x := 0, y := 0 }

instance : Add Coord where
  add c1 c2 := { x := c1.x + c2.x, y := c1.y + c2.y }

def movesToString : List Move -> String
  | [] => "[]"
  | x::xs => toString x ++ xs.foldl (fun acc x => acc ++ toString x) ""

def digitToCoords (d: Char) : Coord :=
  match d with
  | '1' => { x := 0, y := 2 }
  | '2' => { x := 1, y := 2 }
  | '3' => { x := 2, y := 2 }
  | '4' => { x := 0, y := 1 }
  | '5' => { x := 1, y := 1 }
  | '6' => { x := 2, y := 1 }
  | '7' => { x := 0, y := 0 }
  | '8' => { x := 1, y := 0 }
  | '9' => { x := 2, y := 0 }
  | '0' => { x := 1, y := 3 }
  | 'A' => { x := 2, y := 3 }
  | _ => panic! s!"Invalid digit {d}"

def moveToCoords (m: Move) : Coord :=
  match m with
  | Move.Up => { x := 1, y := 0 }
  | Move.Down => { x := 1, y := 1 }
  | Move.Left => { x := 0, y := 1 }
  | Move.Right => { x := 2, y := 1 }
  | Move.Press => { x := 2, y := 0 }

def Coord.coordToMove (c: Coord) : Move :=
  match c with
  | { x := 1, y := 0 } => Move.Up
  | { x := 1, y := 1 } => Move.Down
  | { x := 0, y := 1 } => Move.Left
  | { x := 2, y := 1 } => Move.Right
  | { x := 2, y := 0 } => Move.Press
  | _ => panic! s!"Invalid coord {repr c}"

def Int.abs (x : Int) : Nat :=
  if x < 0 then (-x).toNat else x.toNat

def checkLegalArrows (startC: Coord) (moves: List Move) : Id Bool := do
  let mut lastCoord := startC
  for m in moves do
    lastCoord := lastCoord + m.toDeltaCoord
    if lastCoord.x == 0 ∧ lastCoord.y == 0 then
      return false
  true

def movesBetweenMoves (s: Move) (e: Move) : List Move :=
  let startCoord := moveToCoords s
  let endCoord := moveToCoords e
  let dy := endCoord.y - startCoord.y
  let dx := endCoord.x - startCoord.x
  let vMoves := List.replicate dy.abs (if dy > 0 then Move.Down else Move.Up)
  let hMoves := List.replicate dx.abs (if dx > 0 then Move.Right else Move.Left)
  let vThenH := vMoves ++ hMoves
  let hThenV := hMoves ++ vMoves

  if !checkLegalArrows startCoord vThenH then
    hThenV ++ [Move.Press]
  else if !checkLegalArrows startCoord hThenV then
    vThenH ++ [Move.Press]
  else
    if dx > 0 then -- Right is never worse than up/down
      vThenH ++ [Move.Press]
    else
      hThenV ++ [Move.Press]

def getMoveSeqMoves (l : List Move) : Id (List Move) := do
  let mut moves := []
  let mut lastMove := Move.Press
  for i in [:l.length] do
    moves := moves ++ movesBetweenMoves lastMove l[i]!
    lastMove := l[i]!
  moves

def checkLegalDigits (startC: Coord) (moves: List Move) : Id Bool := do
  let mut lastCoord := startC
  for m in moves do
    lastCoord := lastCoord + m.toDeltaCoord
    if lastCoord.x == 0 ∧ lastCoord.y == 3 then
      return false
  true

def moveBetweenDigits (startC: Char) (endC: Char) : List Move :=
  let startCoord := digitToCoords startC
  let endCoord := digitToCoords endC
  let dy := endCoord.y - startCoord.y
  let dx := endCoord.x - startCoord.x
  let vMoves := List.replicate dy.abs (if dy > 0 then Move.Down else Move.Up)
  let hMoves := List.replicate dx.abs (if dx > 0 then Move.Right else Move.Left)
  let vThenH := vMoves ++ hMoves
  let hThenV := hMoves ++ vMoves

  if !checkLegalDigits startCoord vThenH then
    hThenV ++ [Move.Press]
  else if !checkLegalDigits startCoord hThenV then
    vThenH ++ [Move.Press]
  else
    if (getMoveSeqMoves (getMoveSeqMoves vThenH)).length < (getMoveSeqMoves (getMoveSeqMoves hThenV)).length then
      vThenH ++ [Move.Press]
    else
      hThenV ++ [Move.Press]

def getMoveSeqDigits (s : String) : Id (List Move) := do
  let chars := s.toList
  let mut moves := []
  let mut lastDigit := 'A'
  for i in [:chars.length] do
    moves := moves ++ moveBetweenDigits lastDigit chars[i]!
    lastDigit := chars[i]!
  moves

def charsToNat (chars : List Char) : Nat :=
  chars.foldl (fun acc d => 10 * acc + (d.toNat - '0'.toNat)) 0

def getMoveMovesFive (l : List Move) : List Move :=
  (getMoveSeqMoves (getMoveSeqMoves (getMoveSeqMoves (getMoveSeqMoves (getMoveSeqMoves l)))))

structure Table (α : Type) where
  data: Array (Array α)
deriving Repr, Inhabited

def Move.toNat (m: Move) : Nat :=
  match m with
  | Move.Up => 0
  | Move.Down => 1
  | Move.Left => 2
  | Move.Right => 3
  | Move.Press => 4

def Move.ofNat (n : Nat) : Move :=
  match n with
  | 0 => Move.Up
  | 1 => Move.Down
  | 2 => Move.Left
  | 3 => Move.Right
  | 4 => Move.Press
  | _ => panic! s!"Invalid move {n}"

def Table.get (t: Table α) (s: Move) (e: Move) [Inhabited α] : α :=
  let row := t.data.get! s.toNat
  let col := row.get! e.toNat
  col

def buildTableMoveToTwelveMoves : Id (Table (List Move)) := do
  let mut table: Array (Array (List Move)) := Array.mkArray 5 (Array.mkArray 5 [])
  for i in [:5] do
    for j in [:5] do
      let base := movesBetweenMoves (Move.ofNat i) (Move.ofNat j) -- This is one level
      let row := table.get! i
      table := (table.set! i (row.set! j (getMoveSeqMoves (getMoveMovesFive (getMoveMovesFive base))))) -- This is 11 more levels
  {data := table}

def buildTableMoveToCounts (movetoNMoves: Table (List Move)) : Table Nat :=
  let data := movetoNMoves.data.map (fun row => row.map (fun col => col.length))
  {data}

def getFinalMoveCount (moveToMovesTable: Table (List Move)) (moveToCountTable: Table Nat) (s: String) : Id Nat := do
  let digitMoves := getMoveSeqDigits s
  let oneArrowMoves := getMoveSeqMoves digitMoves
  let workingList := (Move.Press, oneArrowMoves.head!) :: oneArrowMoves.zip (oneArrowMoves.drop 1)
  let thirteenStepMoves := (workingList.map (fun p => moveToMovesTable.get p.fst p.snd)).flatten

  let mut total := 0
  let mut lastMove := Move.Press
  for m in thirteenStepMoves do
    let count := moveToCountTable.get lastMove m
    total := total + count
    lastMove := m
  total

def main : IO Unit := do
  -- Timing isn't correct
  IO.println "Running"
  let startTime ← IO.monoNanosNow
  let tableMoveToTwelveMoves := buildTableMoveToTwelveMoves
  let tableMoveToTwelveCounts := buildTableMoveToCounts tableMoveToTwelveMoves
  IO.println "Ended"
  let now ← IO.monoNanosNow
  let elapsed := Nat.sub now startTime
  IO.println s!"Finished computing table in {elapsed} nanoseconds"
  IO.println s!"Finished computing table in {elapsed/1000000} milliseconds"

  let contents ← IO.FS.readFile "input.txt"
  let l := readProblem contents

  let mut total := 0
  for s in l do
    let s_num := charsToNat (s.toList.filter (Char.isDigit))
    let moveCount := getFinalMoveCount tableMoveToTwelveMoves tableMoveToTwelveCounts s
    IO.println s!"Number of moves: {moveCount}"
    total := total + Nat.mul moveCount s_num
  IO.println s!"{total}"

-- Inverse of getMoveSeqMoves
def moveSeqToMoves (l : List Move) : Id (List Move) := do
  let mut moves := []
  let mut lastCoord := moveToCoords Move.Press
  for i in [:l.length] do
    lastCoord := lastCoord + l[i]!.toDeltaCoord
    if l[i]! == Move.Press then
      moves := moves ++ [lastCoord.coordToMove]
  moves

-- Having the middle robot (of the three robots in part 1) press `<<^^A` is shorter than `^^<<A` because `^` is closer to `A` than `<` is.
-- Doing this generalizes to the chain of 25 robots (and I assert without proof that looking ahead two steps is sufficient).
