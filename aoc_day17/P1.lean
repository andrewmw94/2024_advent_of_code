import Init.System.FilePath
import Init.System.IO
import AocDay17

partial def runHelper (state: ComputerState) (program : List Nat) : ComputerState :=
  if state.ip >= program.length then
    state
  else
    let newState := executeInstruction state program
    runHelper newState program

def runProgram (initA initB initC : Nat) (program : List Nat) : List Nat :=
  let init := ComputerState.mk initA initB initC 0 []
  let state := runHelper init program
  state.outputs

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let parsed := parseInput contents
  let result := runProgram parsed.regA parsed.regB parsed.regC parsed.program
  IO.println s!"{result}"
