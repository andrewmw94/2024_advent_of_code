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

def moveBetweenMoves (s: Move) (e: Move) : List Move :=
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
    moves := moves ++ moveBetweenMoves lastMove l[i]!
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

  let hyp := match (getMoveSeqMoves vThenH).length != (getMoveSeqMoves hThenV).length with
    | true => dbg_trace "Not so clever"; true
    | false => false

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

def main : IO Unit := do

  let startTime ← IO.monoNanosNow

  let contents ← IO.FS.readFile "input.txt"
  let l := readProblem contents
  let mut total := 0
  for s in l do
    let digitMoves := getMoveSeqDigits s
    let moveMoves := getMoveSeqMoves digitMoves
    let moveMoveMoves := getMoveSeqMoves moveMoves

    -- IO.println s!"digit moves: {movesToString digitMoves}"
    -- IO.println s!"move moves: {movesToString moveMoves}"
    -- IO.println s!"move move moves: {movesToString moveMoveMoves}"

    let s_num := charsToNat (s.toList.filter (Char.isDigit))
    IO.println s!"Number of moves: {moveMoveMoves.length}"
    total := total + moveMoveMoves.length * s_num


  let now ← IO.monoNanosNow
  let elapsed := Nat.sub now startTime
  IO.println s!"Finished in {elapsed} nanoseconds"
  IO.println s!"Finished in {elapsed/1000} microseconds"
  IO.println s!"Finished in {elapsed/1000000} milliseconds"

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


-- #eval movesToString (moveSeqToMoves ("<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".toList.map charToMove)) -- Optimal solution for 379
-- #eval movesToString (moveSeqToMoves ("<A>Av<<AA>^AA>AvAA^A<vAAA>^A".toList.map charToMove)) -- ^A<<^^A>>AvvvA

-- #eval (getMoveSeqMoves ("<A>Av<<AA>^AA>AvAA^A<vAAA>^A".toList.map charToMove)).length

-- #eval movesToString (moveSeqToMoves ("v<<A^>>AvA^Av<<A^>>AAv<A<A^>>AA<Av>AA^Av<A^>AA<A>Av<A<A^>>AAA<Av>A^A".toList.map charToMove)) -- My soln for 379
-- #eval movesToString (moveSeqToMoves ("<A>A<AAv<AA^>>AvAA^Av<AAA^>A".toList.map charToMove)) -- ^A^^<<A>>AvvvA

-- #eval (getMoveSeqMoves ("<A>A<AAv<AA^>>AvAA^Av<AAA^>A".toList.map charToMove)).length
-- Having the middle robot press `<<^^A` is shorter than `^^<<A` because `^` is closer to `A` than `<` is.
