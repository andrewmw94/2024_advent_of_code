import Init.System.FilePath
import Init.System.IO
import AocDay22

def generateNthSecret (initial : Nat) (n : Nat) : Nat :=
  match n with
  | 0 => initial
  | n + 1 => generateNthSecret (generateNextSecret initial) n

def solve (inputs : List Nat) : Nat :=
  inputs.map (fun x => generateNthSecret x 2000)
    |> List.foldl (· + ·) 0

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem contents
  let result := solve s
  IO.println s!"{result}"
