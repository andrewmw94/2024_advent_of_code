def parseConstraintLine (s: String) : (Nat × Nat) :=
  let parts := s.splitOn "|"
  ((parts.get! 0).toNat!, (parts.get! 1).toNat!)

def parseListLine (s: String) : List Nat :=
  let parts := s.splitOn ","
  parts.map (·.toNat!)

def parseFileString (input : String) : (List (Nat × Nat) × List (List Nat)) :=
  -- Split input into lines
  let lines := input.splitOn "\n"

  -- Find the index of the empty line that separates constraints from data
  let separator := lines.findIdx (·.isEmpty)

  -- Split into constraint lines and data lines
  let constraintLines := lines.take separator
  let dataLines := lines.drop (separator + 1)

  -- Parse both sections and return as a pair
  (constraintLines.map parseConstraintLine, dataLines.map parseListLine)


def listIntersect (l1: List Nat) (l2: List Nat) : List Nat :=
  l1.filter (λ x => l2.elem x)

def checkListValidForwards (constraints: List (Nat × Nat)) (l: List Nat) : Bool :=
  match l with
  | [] => true
  | h :: t =>
    let relevantConstraints := constraints.filter (λ p => p.snd == h)
    let predecessors := relevantConstraints.map (λ p => p.fst)
    if listIntersect t predecessors != [] then
      false
    else
      checkListValidForwards constraints t

def checkListValidBackwards (constraints: List (Nat × Nat)) (l: List Nat) : Bool :=
  match l with
  | [] => true
  | h :: t =>
    let relevantConstraints := constraints.filter (λ p => p.fst == h)
    let successors := relevantConstraints.map (λ p => p.snd)
    if listIntersect t successors != [] then
      false
    else
      checkListValidBackwards constraints t


def getMiddleElement (l: List Nat) : Nat :=
  -- Not explicitly stated in the problem, but the lists have an odd length
  l.get! (l.length / 2)
