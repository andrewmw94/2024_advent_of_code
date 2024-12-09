import Init.System.FilePath
import Init.System.IO
import AocDay8

def sub (a b: Int × Int) : Int × Int :=
  (a.1 - b.1, a.2 - b.2)

def add (a b: Int × Int) : Int × Int :=
  (a.1 + b.1, a.2 + b.2)

def getAllPairs {α : Type} (l: List α) : Id (List (α × α)) := do
  let mut pairs := []
  for x in l do
    for y in l do
      let pair := (x, y)
      pairs := pair :: pairs
  pairs

def computeAntipoles (l: Array (Int × Int)) : Id (List (Int × Int)) := do
  let all_pairs ← getAllPairs l.toList
  let mut antipoles := []
  for p in all_pairs do
    let p1 := p.fst
    let p2 := p.snd
    if p1 != p2 then
      let v := sub p2 p1
      antipoles := (add p2 v) :: antipoles
      antipoles := (sub p1 v) :: antipoles
  antipoles

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let grid := readProblem contents
  let alphas := findAlphas grid
  let mut antipoles: List (Int × Int) := []
  for p in alphas.toList do
    antipoles := (computeAntipoles p.snd).append antipoles
  antipoles := antipoles.filter (fun p => (grid.getCell p.1 p.2).isSome)
  let uniqueAntipoles := antipoles.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
  IO.println s!"{uniqueAntipoles.length}"
