import Init.System.FilePath
import Init.System.IO
import AocDay19

def solve (input : String) : Id Nat := do
  let puzzle := parse_input input
  let pieces := puzzle.patterns.toArray
  let mut memo := Std.HashMap.empty
  let mut count := 0
  for pattern in puzzle.designs do
    let res ← countBuilds pattern pieces memo true
    memo := res.snd
    count := count + res.fst
  count

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := solve contents
  IO.println s!"{s}"
