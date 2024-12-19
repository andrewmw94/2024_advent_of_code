import Init.System.FilePath
import Init.System.IO
import AocDay18

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let mut lowerBound := 1024
  let mut upperBound := 3450
  while lowerBound + 1 < upperBound do
    let middle := (lowerBound + upperBound) / 2
    let p := readProblem contents 71 71 (some middle)
    let cost := p.graph.dijkstra p.startNode p.endNode
    if cost.isSome then
      lowerBound := middle
    else
      upperBound := middle
  IO.println s!"{lowerBound}"
  IO.println s!"{upperBound}"
