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

def projectToGridEnd (g: Grid) (p: (Int × Int)) (step: (Int × Int)) : Id (List (Int × Int)) := do
  let mut res := []
  let mut current := p
  while (g.getCell current.1 current.2).isSome do
    res := current :: res
    current := add current step
  res

def computeAntipoles (g: Grid) (l: Array (Int × Int)) : Id (List (Int × Int)) := do
  let all_pairs ← getAllPairs l.toList
  let mut antipoles := []
  for p in all_pairs do
    let p1 := p.fst
    let p2 := p.snd
    if p1 != p2 then
      let v := sub p2 p1
      antipoles := (projectToGridEnd g p2 v).append antipoles
      antipoles := (projectToGridEnd g p1 (sub (0,0) v)).append antipoles
  antipoles

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let grid := readProblem contents
  let alphas := findAlphas grid
  let mut antipoles: List (Int × Int) := []
  for p in alphas.toList do
    antipoles := (computeAntipoles grid p.snd).append antipoles
  antipoles := antipoles.filter (fun p => (grid.getCell p.1 p.2).isSome)
  let uniqueAntipoles := antipoles.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
  IO.println s!"{uniqueAntipoles.length}"
