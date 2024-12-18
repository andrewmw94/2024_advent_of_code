import Init.System.FilePath
import Init.System.IO
import AocDay17

def matchesSoFar (soFar: List Nat) (desiredOutput: List Nat): Bool :=
  soFar == desiredOutput.take soFar.length

partial def matchesOutput (state: ComputerState) (program : List Nat) (desiredOutput: List Nat): Bool :=
  if state.ip >= program.length then
    state.outputs == desiredOutput
  else if state.outputs.length >= desiredOutput.length then
    state.outputs.take desiredOutput.length == desiredOutput
  else
    let newState := executeInstruction state program
    if matchesSoFar newState.outputs desiredOutput then
      matchesOutput newState program desiredOutput
    else
      false

-- def magic_suffix := "111000011010000010101000101010"
def magic_suffix := "100100000010101000101010"

def toBinary (n : Nat) : List Bool :=
  if n = 0 then
    [false]
  else
    let rec toBinaryAux (n : Nat) (acc : List Bool) : List Bool :=
      if n = 0 then
        acc
      else
        toBinaryAux (n / 2) ((n % 2 = 1) :: acc)
    toBinaryAux n []

-- Helper to convert binary to string for visualization
def binaryToString (bits : List Bool) : String :=
  let bitStrs := bits.map (fun b => if b then "1" else "0")
  String.intercalate "" bitStrs

def fromBinary (bits : List Char) : Nat :=
  let rec fromBinaryAux (bits : List Char) (acc : Nat) : Nat :=
    match bits with
    | [] => acc
    | '0'::rest => fromBinaryAux rest (2 * acc)
    | '1'::rest => fromBinaryAux rest (2 * acc + 1)
    | _ => 0
  fromBinaryAux bits 0

-- Value of A that works but is not the minimum:
def tooHigh := fromBinary "101110000000110100111110100100000010101000101010".toList

def findProgramRepro (_ initB initC : Nat) (program : List Nat) (desiredOutput: List Nat): Option Nat := do
  -- let magicSuffixNat := fromBinary magic_suffix.toList
  -- let searchStartDigit := 2 ^ magic_suffix.length
  let max := 1000000000
  for a in [:max+1] do
    let a := tooHigh - max + a
    -- let a := a * searchStartDigit + magicSuffixNat
    if a % (max/100) == 0 then
      dbg_trace s!"{a}"
    let init := ComputerState.mk a initB initC 0 []
    if matchesOutput init program desiredOutput then
      return a
  none

#eval fromBinary magic_suffix.toList
#eval fromBinary "101110000000110100111110100100000010101000101010".toList -- too high
#eval fromBinary "101110000000110100111110100100000010101000101010".toList -- Also the first found with shorter suffix
#eval fromBinary "101110000000110100100110111100000010101000101010".toList -- First found with prefix (correct answer)


def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let parsed := parseInput contents
  -- for i in [:17] do
  --   let result := findProgramRepro parsed.regA parsed.regB parsed.regC parsed.program (parsed.program.take i)
  --   match result with
  --   | none => IO.println s!"i: {i} : none"
  --   | some result => IO.println s!"i: {i} : {binaryToString (toBinary result)}"
  let result := findProgramRepro parsed.regA parsed.regB parsed.regC parsed.program parsed.program
  match result with
  | none => IO.println s!"none"
  | some result => IO.println s!"{binaryToString (toBinary result)}"


-- Program is:
-- Register A: 59397658
-- Register B: 0
-- Register C: 0
--
-- Program: 2,4,1,1,7,5,4,6,1,4,0,3,5,5,3,0

-- bst 4
-- bxl 1
-- cdv 5
-- bxc 6
-- bxl 4
-- adv 3
-- out 5
-- jnz 0

-- Program is:
-- while a != 0
--   b := a % 8
--   b := b ^^^ 1
--   c := a / 32
--   b := b ^^^ c
--   b := b ^^^ 4
--   a := a / 8
--   output b
