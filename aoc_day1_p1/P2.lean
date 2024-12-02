import AocDay1P1
import Batteries.Data.List.Basic

def getCount (n: Int) (l: List Int) : Int :=
  l.foldl (fun acc x => if x == n then acc + 1 else acc) 0

def compute_dist_naive (l1: List Int) (l2: List Int) : Int :=
  match l1 with
  | [] => 0
  | h1::t1 => h1 * getCount h1 l2 + compute_dist_naive t1 l2



def main : IO Unit := do
  let ⟨l1, l2⟩  ← readTwoColumnFile "numbers.txt"
  let dist := compute_dist_naive l1 l2
  IO.println s!"Distance: {dist}"
