import Init.System.FilePath
import Init.System.IO
import AocDay5

def orderByConstraints (input: List Nat) (constraints: List (Nat × Nat)) : List Nat :=
  let relevantConstraints := constraints.filter (λ p => input.contains p.fst ∧ input.contains p.snd)
  -- Because the constraints are the transitively closed and exhaustive, we can sort by number of occurences of p.fst of the relevant constraints
  let counts := input.map (λ x => ((relevantConstraints.filter (λ p => p.fst == x)).length, x))
  let orderedCounts := counts.mergeSort (λ a b => a.1 > b.1)
  orderedCounts.map (λ p => p.snd)

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let p := parseFileString contents
  let constraints := p.fst
  let data := p.snd

  -- Not stated, but the constraints are already closed and exhaustive
  let invalidLists := data.filter (λ l => ¬ (checkListValidForwards constraints l ∧ checkListValidBackwards constraints l.reverse))
  let sortedInvalidLists := invalidLists.map (λ l => orderByConstraints l constraints)
  let middles := sortedInvalidLists.map getMiddleElement
  let sum := middles.foldl (λ acc x => acc + x) 0
  IO.print s!"Sum: {sum}\n"
