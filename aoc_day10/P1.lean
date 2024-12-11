import Init.System.FilePath
import Init.System.IO
import AocDay10

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let g := readProblem contents
  let nines := getNines g
  let zeros := (nines.map (λ (r, c) => getUniqueZeros g r c)).flatten
  IO.println s!"{zeros.length}"
