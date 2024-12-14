import Lean.Data.HashMap
import Std.Internal.Parsec

structure ButtonMove where
  x : Int
  y : Int
deriving Inhabited, Repr

structure Prize where
  x : Int
  y : Int
deriving Inhabited, Repr

structure ClawMachine where
  buttonA : ButtonMove
  buttonB : ButtonMove
  prize : Prize
deriving Inhabited, Repr

def parseNumber (input : String) : Option Int :=
  let nums := input.toList.filter (·.isDigit)
  String.toInt? (String.mk nums)

def parseButtonLine (input : String) : Option ButtonMove := do
  let parts := input.split (· == ',')
  if parts.length != 2 then none
  let xPart := parts[0]!.trim
  let yPart := parts[1]!.trim

  let xNum ← parseNumber xPart
  let yNum ← parseNumber yPart

  some { x := xNum, y := yNum }

def parsePrizeLine (input : String) : Option Prize := do
  let parts := input.split (· == ',')
  if parts.length != 2 then none
  let xPart := parts[0]!.trim
  let yPart := parts[1]!.trim

  let xNum ← parseNumber xPart
  let yNum ← parseNumber yPart

  some { x := xNum, y := yNum }

def parseClawMachine (lines : Array String) : Option ClawMachine := do
  if lines.size != 3 then none

  let buttonA ← parseButtonLine lines[0]!
  let buttonB ← parseButtonLine lines[1]!
  let prize ← parsePrizeLine lines[2]!

  some {
    buttonA := buttonA,
    buttonB := buttonB,
    prize := prize
  }

def readProblem (input : String) : Array ClawMachine := Id.run do
  let mut machines := #[]
  let lines := input.splitOn "\n"
  let mut currentMachine := #[]

  for line in lines do
    if line.trim.isEmpty then
      if !currentMachine.isEmpty then
        if let some machine := parseClawMachine currentMachine then
          machines := machines.push machine
        currentMachine := #[]
    else
      currentMachine := currentMachine.push line.trim

  -- Handle last machine if exists
  if !currentMachine.isEmpty then
    if let some machine := parseClawMachine currentMachine then
      machines := machines.push machine

  machines

def getSolnCost (soln: Nat × Nat) : Nat :=
  soln.1 * 3 + soln.2

def getAllSolutions (p: ClawMachine) : Id (Array (Nat × Nat)) := do
  let mut solns := #[]
  for x in [0:100] do
    for y in [0:100] do
      if x * p.buttonA.x + y * p.buttonB.x == p.prize.x &&
         x * p.buttonA.y + y * p.buttonB.y == p.prize.y then
        solns := solns.push (x,y)
  solns

def getCheapestSolution (p: ClawMachine) : Option (Nat × Nat) :=
  let allSolutions := getAllSolutions p
  if allSolutions.isEmpty then none
  else
    let costs := allSolutions.map getSolnCost
    let minCost := costs.foldl Nat.min (costs[0]!)
    let minIndex := costs.findIdx? (· == minCost)
    allSolutions.get? minIndex.get!
