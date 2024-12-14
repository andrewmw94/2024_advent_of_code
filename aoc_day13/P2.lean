import Init.System.FilePath
import Init.System.IO
import AocDay13

partial def gcd (a b : Nat) : Nat :=
  match b with
  | 0 => a
  | _ => gcd b (a % b)

def lcm (a b : Nat) : Nat :=
  match a, b with
  | 0, _ => 0
  | _, 0 => 0
  | _, _ =>
    let g := gcd a b
    (a / g) * b

def solveColinear (target expensive cheap: Int) : Id (Option (Nat × Nat)) := do
  let remainder := target % (lcm expensive.toNat cheap.toNat)
  if 3*cheap > expensive then -- cheap is more efficient
    let num_non_remainder := (target - remainder) / cheap
    let max_efficient := remainder / cheap
    for inv_num_efficient in [:max_efficient.toNat+1] do
      let num_efficient := max_efficient - inv_num_efficient
      if (remainder - (num_efficient * cheap)) % expensive == 0 then
        let num_inefficient := (remainder - (num_efficient * cheap)) / expensive
        return some (num_inefficient.toNat, (num_efficient + num_non_remainder).toNat)
    return none
  else -- expensive is more efficient
    let num_non_remainder := (target - remainder) / expensive
    let max_efficient := remainder / expensive
    for inv_num_efficient in [:max_efficient.toNat+1] do
      let num_efficient := max_efficient - inv_num_efficient
      if (remainder - (num_efficient * expensive)) % cheap == 0 then
        let num_inefficient := (remainder - (num_efficient * expensive)) / cheap
        return some ((num_efficient + num_non_remainder).toNat, num_inefficient.toNat)
    return none

#eval solveColinear 40 20 10
#eval solveColinear 40 40 10
#eval solveColinear 50 40 10
#eval solveColinear 60 40 10
#eval solveColinear 61 40 10

def determinant (a b c d: Int) : Int := a * d - b * c

def solveEqn (p: ClawMachine) : Option (Nat × Nat) :=
  let D := determinant p.buttonA.x p.buttonB.x p.buttonA.y p.buttonB.y
  let A_B_collinear := D == 0
  if A_B_collinear then
    let prize_collinear := determinant p.buttonA.x p.prize.x p.buttonA.y p.prize.y == 0
    if ¬prize_collinear then
      none
    else
      -- Not used in AoC :-(
      solveColinear p.prize.x p.buttonA.x p.buttonB.x
  else
    let Dx := determinant p.prize.x p.buttonB.x p.prize.y p.buttonB.y
    let Dy := determinant p.buttonA.x p.prize.x p.buttonA.y p.prize.y
    if Dx % D == 0 && Dy % D == 0 then
      let x := Dx / D
      let y := Dy / D
      if x >= 0 && y >= 0 then
        some (x.toNat, y.toNat)
      else
        none
    else
      none

def myCM := { buttonA := { x := 94, y := 34 }, buttonB := { x := 22, y := 67 }, prize := { x := 8400, y := 5400 } : ClawMachine}

#eval solveEqn myCM

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem contents

  -- let adjustedProblems := s.map (fun p => { p with prize := { x := p.prize.x + 10000000000000, y := p.prize.y + 10000000000000 } })
  let adjustedProblems := s

  let solns := adjustedProblems.map solveEqn |>.filterMap id

  let costs := solns.map getSolnCost

  let sum := costs.foldl Nat.add 0
  IO.println s!"{sum}"
