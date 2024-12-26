import Init.System.FilePath
import Init.System.IO
import AocDay24



/-- Solve the puzzle given input lines -/
def solvePuzzle (input : String) : Nat :=
  let lines := input.trim.splitOn "\n"
  let initialState := initCircuit lines
  -- dbg_trace s!"initialState: {repr initialState}"

  let finalState := simulateCircuit initialState
  let zValues := getWireValues "z" finalState.wires
  dbg_trace s!"zValues: {zValues}"
  binaryToDecimal zValues.reverse  -- Reverse because z00 is least significant bit

def main : IO Unit := do
  let contents ← IO.FS.readFile "trial.txt"
  let res := solvePuzzle contents
  IO.println s!"{res}"
