import Init.System.FilePath
import Init.System.IO
import AocDay24

partial def collectInputs (gateName: String) (gates : List Gate) : List String :=
  if gateName.startsWith "x" || gateName.startsWith "y" then
    [gateName]
  else
    let gate := (gates.find? (λ g => g.output == gateName)).get!
    let inputs := collectInputs gate.input1 gates ++ collectInputs gate.input2 gates
    inputs.mergeSort

partial def getGraphLeadingTo (gateName: String) (gates : List Gate) : List Gate :=
  let gate := (gates.find? (λ g => g.output == gateName)).get!
  let left := if gate.input1.startsWith "x" || gate.input1.startsWith "y" then [] else getGraphLeadingTo gate.input1 gates
  let right := if gate.input2.startsWith "x" || gate.input2.startsWith "y" then [] else getGraphLeadingTo gate.input2 gates
  [gate] ++ left ++ right

partial def getOutputsAffectedBy (gateName: String) (gates : List Gate) : Id (List String) := do
  let mut frontier := (gates.filter (λ g => g.input1 == gateName || g.input2 == gateName)).map (λ g => g.output)
  let mut visited := []
  while !frontier.isEmpty do
    let current := frontier.head!
    frontier := frontier.tail!
    if visited.contains current then
      continue
    visited := current :: visited
    for gate in gates do
      if gate.input1 == current || gate.input2 == current then
        if !visited.contains gate.output then
          frontier := gate.output :: frontier
  (visited.filter (λ v => v.startsWith "z")).mergeSort

def problem := initCircuit (problemStr.splitOn "\n")

def toTwoDigitString (n: Nat) : String :=
  if n < 10 then "0" ++ toString n else toString (n % 100)

def inputNames := (List.range 45).map (λ i => "x" ++ toTwoDigitString i) ++ (List.range 45).map (λ i => "y" ++ toTwoDigitString i)
def outputNames := (List.range 46).map (λ i => "z" ++ toTwoDigitString i)

def renameGate (oldName: String) (newName: String) (g: Gate)  : Gate :=
  if g.input1 == oldName ∧ g.input2 == oldName then Gate.make g.type newName newName g.output else
  if g.input1 == oldName then Gate.make g.type newName g.input2 g.output else
  if g.input2 == oldName then Gate.make g.type g.input1 newName g.output else
  g

def getCarryZInputs (n: Nat) : (String × String) :=
  if n == 1 then
    ("AND_x00_y00", "XOR_x01_y01")
  else
    let nStr := toTwoDigitString n
    let predNStr := toTwoDigitString (n-1)
    ("CARRY_"++predNStr, "XOR_x"++nStr++"_y"++nStr)

def getCarryInputs (n: Nat) : (String × String) :=
  let nStr := toTwoDigitString n
  ("AND_x"++nStr++"_y"++nStr, "CARRYZ_"++nStr)

def getNewName (g: Gate) (currCarry : Nat) : String :=
  if g.type == GateType.AND && g.input1 == (getCarryZInputs currCarry).fst && g.input2 == (getCarryZInputs currCarry).snd then
    "CARRYZ_"++toTwoDigitString currCarry
  else if g.type == GateType.OR && g.input1 == (getCarryInputs currCarry).fst && g.input2 == (getCarryInputs currCarry).snd then
    "CARRY_"++toTwoDigitString currCarry
  else
    if g.input1 < g.input2 then
        g.type.toString ++"_"++g.input1++"_"++g.input2
      else
        g.type.toString ++"_"++g.input2++"_"++g.input1

def renamePuzzle (c : CircuitState) : Id (CircuitState × Std.HashMap String String) := do
  let mut finalNames := inputNames
  let mut remainingGates := c.gates
  let mut newGates := []
  let mut count := 0
  let mut currCarry := 1

  let mut mapping : Std.HashMap String String := Std.HashMap.empty

  while !remainingGates.isEmpty do
    count := count + 1
    let gate := remainingGates.head!
    remainingGates := remainingGates.tail!
    if finalNames.contains gate.input1 ∧ finalNames.contains gate.input2 then
      let oldName := gate.output
      let mut newName := getNewName gate currCarry
      newName := if oldName.startsWith "z" then oldName else newName
      let newGate := Gate.make gate.type gate.input1 gate.input2 newName
      if newName.startsWith "CARRY_" then
        currCarry := currCarry + 1
      mapping := mapping.insert newName oldName
      newGates := newGate :: newGates
      remainingGates := remainingGates.map (renameGate oldName newName)
      finalNames := newName :: finalNames
      count := 0
    else
      remainingGates := remainingGates ++ [gate]
    if count > remainingGates.length then
      break
  ({c with gates := newGates}, mapping)

def carryGateInputStr (i : Nat) : String :=
  if i == 1 then
    "AND_x00_y00 AND XOR_x01_y01"
  else
    let nStr := toTwoDigitString i
    let predNStr := toTwoDigitString (i-1)
    "OR_CARRY_"++predNStr++"_AND_x"++predNStr++"_y"++predNStr++" AND XOR_x"++nStr++"_y"++nStr

partial def decimalToBinary (n : Nat) (numDigits : Nat) : List Bool :=
  let rec aux (n : Nat) : List Bool :=
    if n == 0 then [false]
    else if n == 1 then [true]
    else (n % 2 == 1) :: aux (n / 2)
  let tail := aux n
  (List.replicate (numDigits - tail.length) false) ++ tail

def binaryToInputs (bs : List Bool) (inputPrefix: String) : Id (List (String × Bool)) := do
  let bs := bs.reverse
  let mut res := []
  for i in [0:45] do
    res := ((inputPrefix ++ toTwoDigitString i), (bs.get! i)) :: res
  res

def countBits (bs : List Bool) : Nat :=
  bs.foldl (λ acc b => if b then acc + 1 else acc) 0

def swapOutputs (c: CircuitState) (o1 o2: String) : CircuitState :=
  let newGates := c.gates.map (λ g => if g.output == o1 then {g with output := o2} else if g.output == o2 then {g with output := o1} else g)
  {c with gates := newGates}

def getCanonicalPuzzleString (c: CircuitState) : Option String := do
  let renamedPuzzle := renamePuzzle c
  let allOutpts := renamedPuzzle.fst.gates.map (λ g => g.output)
  let mut haveAll := true
  for o in outputNames do
    if !allOutpts.contains o then
      haveAll := false
      break
  if haveAll then
    -- Sorting isn't needed for scoring, but it's nice for visualizing
    let sortedGates := (renamedPuzzle.fst.gates.mergeSort (λ g1 g2 => g1.toString < g2.toString)).map Gate.toString
    let mut res := ""
    res := res ++ s!"Renamed puzzle:\n"
    for gate in sortedGates do
      res := res ++ s!"{gate}\n"
    res := res ++ s!"Mapping:\n"
    for (k,v) in renamedPuzzle.snd do
      res := res ++ s!"{k} --> {v}\n"
    res
  else
    none

-- Higher is better
def testCircuit (c: CircuitState) : Id Nat := do
  match (← getCanonicalPuzzleString c) with
  | none => pure (0)
  | some s =>
    let mut highestCarry := 0
    for i in [1:45] do
      if s.containsStr ("CARRYZ_"++(toTwoDigitString i)) then
        highestCarry := i
      else
        break
    highestCarry

def findBestSwap (c: CircuitState) : Id (String × String) := do
  let allOutputs := c.gates.map (λ g => g.output)
  let uniqueOutputs := allOutputs.foldl (λ acc o => if acc.contains o then acc else o :: acc) []

  let mut bestSwap := ((0,1), 0)
  for i in [0:uniqueOutputs.length] do
    for j in [0:uniqueOutputs.length] do
      if i < j then
        let swapped := swapOutputs c (uniqueOutputs.get! i) (uniqueOutputs.get! j)
        let score ← testCircuit swapped
        if score > bestSwap.snd then
          bestSwap := ((i,j), score)
  (uniqueOutputs.get! (bestSwap.fst.fst), uniqueOutputs.get! (bestSwap.fst.snd))

-- #eval testCircuit problem
-- #eval testCircuit (swapOutputs problem "gvw" "qjb")
-- #eval testCircuit (swapOutputs (swapOutputs problem "gvw" "qjb") "jgc" "z15")
-- #eval testCircuit (swapOutputs (swapOutputs (swapOutputs problem "gvw" "qjb") "jgc" "z15") "drg" "z22")
-- #eval testCircuit (swapOutputs (swapOutputs (swapOutputs (swapOutputs problem "gvw" "qjb") "jgc" "z15") "drg" "z22") "z35" "jbp")

-- -- Solved by printing a canonical version of the puzzle and visually finding the issue
-- def solvePuzzle (input : String) : IO Unit := do
--   let lines := input.trim.splitOn "\n"
--   let initialState := initCircuit lines
--   -- let canonicalString := getCanonicalPuzzleString initialState
--   let canonicalString := getCanonicalPuzzleString (swapOutputs initialState "gvw" "qjb")
--   IO.print canonicalString.get!

-- Greedy search for best swaps
def solvePuzzle (input : String) : IO Unit := do
  let lines := input.trim.splitOn "\n"
  let initialState := initCircuit lines
  let mut swapCount := 0
  let mut currCircuit := initialState
  while swapCount < 4 do
    let res := findBestSwap currCircuit
    IO.println s!"{res}"
    currCircuit := swapOutputs currCircuit res.fst res.snd
    swapCount := swapCount + 1

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let res ← solvePuzzle contents
  IO.println s!"{res}"
