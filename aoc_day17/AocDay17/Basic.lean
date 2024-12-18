
structure ComputerState where
  regA : Nat
  regB : Nat
  regC : Nat
  ip : Nat
  outputs : List Nat
  deriving Repr

structure ParsedInput where
  regA : Nat
  regB : Nat
  regC : Nat
  program : List Nat
  deriving Repr

def parseNumber (s : String) : Nat :=
  s.trim.toNat!

def parseProgram (s : String) : List Nat :=
  let numbers := s.trim.split (·.toString == ",")
  numbers.map (fun n => n.trim.toNat!)

def parseInput (input : String) : ParsedInput :=
  let lines := input.splitOn "\n"
  let regA := parseNumber ((lines[0]!.dropWhile (· != ':')).drop 1)
  let regB := parseNumber ((lines[1]!.dropWhile (· != ':')).drop 1)
  let regC := parseNumber ((lines[2]!.dropWhile (· != ':')).drop 1)
  let program := parseProgram ((lines[4]!.dropWhile (· != ':')).drop 1)
  { regA := regA, regB := regB, regC := regC, program := program }

def getComboValue (state : ComputerState) (operand : Nat) : Nat :=
  match operand with
  | 0 => 0
  | 1 => 1
  | 2 => 2
  | 3 => 3
  | 4 => state.regA
  | 5 => state.regB
  | 6 => state.regC
  | _ => 0 -- operand 7 is reserved

def pow (base : Nat) (exp : Nat) : Nat :=
  Nat.pow base exp

def handleAdv (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    regA := state.regA / (pow 2 (getComboValue state operand)),
    ip := state.ip + 2
  }

def xor (a : Nat) (b : Nat) : Nat :=
  a ^^^ b

def handleBxl (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    regB := xor state.regB operand,
    ip := state.ip + 2
  }

def handleBst (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    regB := getComboValue state operand % 8,
    ip := state.ip + 2
  }

def handleJnz (state : ComputerState) (operand : Nat) : ComputerState :=
  if state.regA ≠ 0 then
    { state with ip := operand }
  else
    { state with ip := state.ip + 2 }

def handleBxc (state : ComputerState) (_ : Nat) : ComputerState :=
  { state with
    regB := xor state.regB state.regC,
    ip := state.ip + 2
  }

def handleOut (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    outputs := state.outputs ++ [(getComboValue state operand) % 8],
    ip := state.ip + 2
  }

def handleBdv (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    regB := state.regA / (2 ^ getComboValue state operand),
    ip := state.ip + 2
  }

def handleCdv (state : ComputerState) (operand : Nat) : ComputerState :=
  { state with
    regC := state.regA / (2 ^ getComboValue state operand),
    ip := state.ip + 2
  }

def executeInstruction (state : ComputerState) (program : List Nat) : ComputerState :=
  if state.ip + 1 >= program.length then
    state -- halt if we can't read both opcode and operand
  else
    let opcode := program[state.ip]!
    let operand := program[state.ip + 1]!

    match opcode with
    | 0 => handleAdv state operand
    | 1 => handleBxl state operand
    | 2 => handleBst state operand
    | 3 => handleJnz state operand
    | 4 => handleBxc state operand
    | 5 => handleOut state operand
    | 6 => handleBdv state operand
    | 7 => handleCdv state operand
    | _ => state -- invalid opcode
