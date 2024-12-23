import Init.System.FilePath
import Init.System.IO
import AocDay22

def getLastDigit (n : Nat) : Nat :=
  n % 10

def generateNSecrets (initial : Nat) (n : Nat) : List Nat :=
  let rec helper (secret : Nat) (remaining : Nat) (acc : List Nat) : List Nat :=
    match remaining with
    | 0 => acc.reverse
    | m + 1 =>
      let nextSecret := generateNextSecret secret
      helper nextSecret m (getLastDigit nextSecret :: acc)
  helper initial n [getLastDigit initial]

partial def getMatchIdx (diffs : Array Int) (pattern : Array Int) : Option Nat := do
  for idx in [3:diffs.size] do
    if diffs.get! (idx - 3) == pattern[0]! ∧ diffs.get! (idx - 2) == pattern[1]! ∧ diffs.get! (idx - 1) == pattern[2]! ∧ diffs.get! idx == pattern[3]! then
      return idx
  none

def processPattern (secrets : Array (List Nat)) (diffs : Array (Array Int)) (pattern : Array Int) : Id Nat := do
  let mut totalSold := 0
  for seqIdx in [:secrets.size] do
    let diffSeq := diffs[seqIdx]!
    match getMatchIdx diffSeq pattern with
    | none => ()
    | some idx => totalSold := totalSold + (secrets.get! seqIdx).get! (idx+1) -- idx+1 because we have no diff for first secret
  totalSold

def solve (inputs : List Nat) : Id Nat := do
  let secrets := inputs.map (λ seed => generateNSecrets seed 1999)
  let zipped := secrets.map (λ seq => seq.zip (seq.drop 1))
  let diffs := (zipped.map (λ seq => (seq.map (fun p => (p.snd : Int) - (p.fst: Int))).toArray))
  let secrets := secrets.toArray

  let mut tasks := #[]
  -- Create tasks for each outer loop iteration
  for i0 in List.range 19 do
    let task := Task.spawn (fun _ => do
      let mut localBest := 0
      for i1 in List.range 19 do
        for i2 in List.range 19 do
          for i3 in List.range 19 do
            let pattern := [(i0 : Int) - 9, (i1 : Int) - 9, (i2 : Int) - 9, (i3 : Int) - 9].toArray
            let totalSold ← processPattern secrets diffs.toArray pattern
            if totalSold > localBest then
              localBest := totalSold
      localBest)
    tasks := tasks.push task

  -- Wait for all tasks and find the maximum result
  let mut bestTotal : Nat := 0
  for task in tasks do
    let result ← task.get
    if result > bestTotal then
      bestTotal := result
  bestTotal

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem contents
  let result := solve s
  IO.println s!"{result}"
