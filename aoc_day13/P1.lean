import Init.System.FilePath
import Init.System.IO
import AocDay13

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem contents

  let solns := s.map getCheapestSolution |>.filterMap id
  let costs := solns.map getSolnCost

  let sum := costs.foldl Nat.add 0
  IO.println s!"{sum}"
