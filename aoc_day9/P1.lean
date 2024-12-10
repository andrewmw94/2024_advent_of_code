import Init.System.FilePath
import Init.System.IO
import AocDay9

def nextEmpty (input: Array (Option Nat)) (idx: Nat) : Nat :=
  if h:idx < input.size then
    if (input.get ⟨idx, h⟩).isNone then
      idx
    else
      nextEmpty input (idx + 1)
  else
    idx
termination_by input.size - idx

def compactify (input: Array (Option Nat)) (front_idx : Nat) (back_idx : Nat) (soFar: Array (Option Nat)): Array (Option Nat) :=
  if back_idx <= front_idx then
    soFar
  else
    let c := input.get! back_idx
    match c with
    | none => compactify input front_idx (back_idx - 1) soFar
    | some n =>
      let newFront := nextEmpty input (front_idx + 1)
      let soFar' := soFar.set! front_idx c
      let soFar'' := soFar'.set! back_idx none
      compactify input newFront (back_idx - 1) soFar''

def checksum (input: Array (Option Nat)) : Id Nat := do
  let mut sum := 0
  for idx in [:input.size] do
    let c := input.get! idx
    match c with
    | none => continue
    | some n =>
      sum := sum + (n * idx)
  sum

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem contents
  let a := s.toArray
  let compact := compactify a (nextEmpty a 0) (a.size - 1) a
  -- IO.println s!"{compact}"
  IO.println s!"{checksum compact}"
