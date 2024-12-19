import Init.System.FilePath
import Init.System.IO
import AocDay18

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let p := readProblem contents 71 71 (some 1024)
  let cost := p.graph.dijkstra p.startNode p.endNode
  IO.println s!"{cost}"
