import AocDay1P1
import Batteries.Data.List.Basic


def compute_dist_from_sorted_lists (l1: List Int) (l2: List Int) : Int :=
  match l1, l2 with
  | [], _ => 0
  | _, [] => 0
  | h1::t1, h2::t2 => Int.natAbs (h1 - h2) + compute_dist_from_sorted_lists t1 t2


def main : IO Unit := do
  let ⟨l1, l2⟩  ← readTwoColumnFile "numbers.txt"
  let dist := compute_dist_from_sorted_lists l1.mergeSort l2.mergeSort
  IO.println s!"Distance: {dist}"
