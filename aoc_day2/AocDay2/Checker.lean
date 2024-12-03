def checkMonotone (list : List Int) : Bool :=
  match list with
  | [] | [_] => true  -- Empty list is considered monotonic
  | _ =>
    let z := List.zip list list.tail!
    z.all (fun (a, b) => a ≤ b) || z.all (fun (a, b) => a ≥ b)

def checkStepSize (list : List Int) : Bool :=
  match list with
  | [] | [_] => true  -- Empty list is considered monotonic
  | _ =>
    let z := List.zip list list.tail!
    z.all (fun (a, b) => Int.natAbs (a - b) > 0 && Int.natAbs (a - b) < 4)

-- Monotonically increasing or decreasing
-- Abs diff between adjacent elements is at least 1 and at most 3
def checkSafe (l: List Int) : Bool :=
  checkMonotone l && checkStepSize l
