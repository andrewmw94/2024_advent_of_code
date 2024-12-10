open Lean

structure FileDesc where
  id : Nat
  size : Nat
deriving Repr, BEq

structure EmptySpace where
  size : Nat
deriving Repr, BEq

inductive FileOrEmpty where
  | file : FileDesc → FileOrEmpty
  | empty : EmptySpace → FileOrEmpty
deriving Repr, BEq

def FileOrEmpty.size : FileOrEmpty → Nat
  | FileOrEmpty.file desc => desc.size
  | FileOrEmpty.empty l => l.size

instance: Inhabited FileOrEmpty where
  default := FileOrEmpty.empty { size := 0 }

def lengthsToFileOrEmpty (lengths: List Nat) : Id (List FileOrEmpty) := do
  let mut result : List FileOrEmpty := []
  let mut fileId := 0
  let mut isFile := true

  for len in lengths do
    if isFile then
      result := result ++ [FileOrEmpty.file { id := fileId, size := len }]
    else
      result := result ++ [FileOrEmpty.empty { size := len }]
    if isFile then fileId := fileId + 1
    isFile := !isFile
  result

def toOptNat (a : Array FileOrEmpty): Array (Option Nat):=
  let l : List (List (Option Nat)) := a.toList.map (fun c => match c with
                  | FileOrEmpty.file desc => List.replicate desc.size (some desc.id)
                  | FileOrEmpty.empty l => List.replicate l.size none)
  l.flatten.toArray

/-- Parse a string of digits into alternating lengths of files and free space -/
def parseDiskMap (input : String) : List Nat :=
  input.toList.map (fun c => c.toString.toNat!)

/-- Main function to process disk map string into blocks string -/
def readProblem (input : String) : List (Option Nat) :=
  --(lengthsToBlocks (parseDiskMap input)).map blockToOptNat
  let a := (lengthsToFileOrEmpty (parseDiskMap input)).toArray
  (toOptNat a).toList

def readProblem2 (input : String) : List FileOrEmpty :=
  (lengthsToFileOrEmpty (parseDiskMap input))
