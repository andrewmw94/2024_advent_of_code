import Init.System.FilePath
import Init.System.IO
import AocDay14

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let robots := readProblem contents
  let score := solvePuzzle robots 100
  IO.println s!"safety factor: {score}"
