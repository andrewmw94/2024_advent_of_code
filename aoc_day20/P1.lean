import Init.System.FilePath
import Init.System.IO
import AocDay20

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let p := readProblem contents
  let costs := getCostsToEndZeroCheats p
  let ans := countShortCuts costs p 2
  IO.println s!"{ans}"
