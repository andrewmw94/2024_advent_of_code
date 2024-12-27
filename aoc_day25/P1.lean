import Init.System.FilePath
import Init.System.IO

import Lake
open Lake DSL System
open Lean

def maxHeight := 5

/-- Represents a sequence of heights -/
def Heights := List Nat
deriving Repr

/-- Parse a single schematic line into a list of bools representing # (true) and . (false) -/
def parseLine (line : String) : List Bool :=
  line.toList.map (· == '#')

/-- Convert a list of lines into a list of column heights by counting from top or bottom -/
def getHeights (lines : List String) (fromTop : Bool) : Heights := Id.run do
  -- Convert lines to bool grid
  let grid := lines.map parseLine
  let width := grid.head!.length

  -- For each column
  let mut heights := []
  for col in [0:width] do
    let mut height := 0
    -- Count # symbols from top or bottom
    for row in [0:lines.length] do
      let rowIdx := if fromTop then row else (lines.length - 1 - row)
      if grid.get! rowIdx |>.get! col then
        height := height + 1
    heights := heights.append [height]
  heights

/-- Check if lock and key heights are compatible (no overlaps) -/
def isCompatible (lock key : Heights) : Bool :=
  -- Must be same length
  if lock.length ≠ key.length then
    false
  else
    -- For each column, sum of heights must not exceed available space
    List.zip lock key |>.all (fun (l, k) => l + k ≤ maxHeight)

def getLockHeights (lines : List (List Char)): Id Heights := do
  let width := lines.head!.length
  let mut heights := (List.replicate width 0).toArray
  for col in [0:width] do
    let mut height := 0
    for row in [0:lines.length] do
      height := height + 1
      if ((lines.get! row).get! col) == '.' then
        break
    heights := heights.set! col (height-2)
  heights.toList

def getKeyHeights (lines : List (List Char)): Id Heights := do
  let width := lines.head!.length
  let mut heights := (List.replicate width 0).toArray
  for col in [0:width] do
    let mut height := lines.length
    for row in [0:lines.length] do
      height := height - 1
      if ((lines.get! row).get! col) == '#' then
        break
    heights := heights.set! col height
  heights.toList

/-- Parse input into locks and keys -/
def parseInput (input : String) : Id (List Heights × List Heights) := do
  let groups := input.splitOn "\n\n"
  let mut locks := []
  let mut keys := []

  for group in groups do
    let lines := group.splitOn "\n"
    if lines.head!.all (· == '#') then
      locks := locks.append [getLockHeights (lines.map (·.toList))]
    else
      keys := keys.append [getKeyHeights (lines.map (·.toList))]

  pure (locks, keys)

/-- Count compatible lock/key pairs -/
def countCompatiblePairs (locks keys : List Heights) : Id Nat := do
  let mut count := 0
  for lock in locks do
    for key in keys do
      if isCompatible lock key then
        count := Nat.add count 1
  count

/-- Main solution function -/
def solve (input : String) : Id Nat := do
  let (locks, keys) ← parseInput input
  pure $ countCompatiblePairs locks keys


def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let res ← solve contents
  IO.println s!"{res}"
