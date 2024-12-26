import Lean.Data.HashMap
import Lean.Data.HashSet

/-- Represents a boolean gate type -/
inductive GateType
  | AND
  | OR
  | XOR
deriving Repr, BEq, Hashable

def GateType.toString (t : GateType) : String :=
  match t with
  | GateType.AND => "AND"
  | GateType.OR => "OR"
  | GateType.XOR => "XOR"


/-- Represents a gate with its inputs and output -/
structure Gate where
  type : GateType
  input1 : String
  input2 : String
  output : String
deriving Repr, BEq, Hashable

def Gate.make (type: GateType) (input1 input2 output: String) : Gate :=
  if input1 <= input2 then
    { type, input1, input2, output }
  else
    { type := type, input1 := input2, input2 := input1, output := output }


instance : Inhabited Gate := ⟨{ type := GateType.AND, input1 := "", input2 := "", output := "" }⟩


/-- The state of our circuit simulation -/
structure CircuitState where
  wires : Std.HashMap String Bool  -- Current wire values
  gates : List Gate                -- All gates in the system
  pending : Std.HashSet Gate       -- Gates waiting to be evaluated
deriving Repr

def Gate.toString (g : Gate) : String :=
  if g.input1 < g.input2 then
    s!"{g.input1} {g.type.toString} {g.input2} -> {g.output}"
  else
    s!"{g.input2} {g.type.toString} {g.input1} -> {g.output}"

/-- Parse a gate type from a string -/
def parseGateType (s : String) : Option GateType :=
  match s with
  | "AND" => some GateType.AND
  | "OR" => some GateType.OR
  | "XOR" => some GateType.XOR
  | _ => none

/-- Parse a gate definition line into a Gate -/
def parseLine (line : String) : Option Gate := do
  let parts := line.splitOn " "
  guard (parts.length == 5)
  guard (parts[3]! == "->")
  let type ← parseGateType parts[1]!
  pure (Gate.make type parts[0]! parts[2]! parts[4]!)

def String.containsStr (s : String) (sub : String) : Bool :=
  match sub.toList with
  | [] => true
  | head :: tail =>
    let splits := s.split (λ c => c == head)
    splits.any (λ s => s.startsWith (String.mk tail))

/-- Initialize circuit state from input lines -/
def initCircuit (input : List String) : Id CircuitState := do
  -- Parse initial wire values
  let mut wires := Std.HashMap.empty
  for line in input do
    if line.contains ':' then
      let parts := line.splitOn ":"
      let wire := parts[0]!.trim
      let value := parts[1]!.trim == "1"
      wires := wires.insert wire value

  -- Parse gates
  let mut gates := []
  for line in input do
    if line.containsStr "->" && !line.contains ':' then
      match parseLine line.trim with
      | some gate => gates := gate :: gates
      | none => pure ()

  { wires := wires
  , gates := gates
  , pending := Std.HashSet.ofList gates}

/-- Apply a gate operation to two boolean values -/
def applyGate (type : GateType) (a b : Bool) : Bool :=
  match type with
  | GateType.AND => a && b
  | GateType.OR => a || b
  | GateType.XOR => a != b

/-- Try to evaluate a gate given the current wire values -/
def tryEvalGate (gate : Gate) (wires : Std.HashMap String Bool) : Option Bool := do
  let input1 ← wires.get? gate.input1
  let input2 ← wires.get? gate.input2
  pure (applyGate gate.type input1 input2)


/-- Simulate the circuit until no more changes occur -/
partial def simulateCircuit (state : CircuitState) : Id CircuitState := do
  if state.pending.isEmpty then
    state
  else
    let mut newState := state
    let mut evaluated := false

    for gate in state.gates do
      if state.pending.contains gate then
        match tryEvalGate gate state.wires with
        | some value =>
            newState := { newState with
              wires := newState.wires.insert gate.output value,
              pending := newState.pending.erase gate }
            evaluated := true
        | none => pure ()

    if evaluated then
      simulateCircuit newState
    else
      state

/-- Convert a binary number represented as a list of bools to decimal -/
def binaryToDecimal (bits : List Bool) : Nat :=
  bits.foldl (fun acc bit => 2 * acc + if bit then 1 else 0) 0

/-- Get all wire values starting with the given prefix in order -/
def getWireValues (pref : String) (wires : Std.HashMap String Bool) : List Bool :=
  let wireNames := wires.toList.map (·.1)
  let zWires := wireNames.filter (·.startsWith pref)
  let sortedWires := zWires.mergeSort
  sortedWires.map (fun name => wires.get! name)
