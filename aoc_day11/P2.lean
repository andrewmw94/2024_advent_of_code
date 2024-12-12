import Init.System.FilePath
import Init.System.IO
import Std.Data.HashMap
import AocDay11

structure Memo where
  cache : Std.HashMap (Nat × Nat) Nat := {}

partial def nStepsCount (n k : Nat) : Nat := Id.run do
  let mut memo : Memo := {}

  let rec helper (n k : Nat) : StateM Memo Nat := do
    -- Base case: 0 steps means 1 pebble
    if k = 0 then return 1

    -- Check cache
    let key := (n, k)
    match (← get).cache[key]? with
    | some result => return result
    | none => do
      -- Calculate result based on transformation
      let result ← match n with
      | 0 => helper 1 (k-1)  -- 0 becomes 1
      | n =>
        if countDigits n % 2 = 0 then
          -- Even number of digits: split and recurse on both parts
          let (left, right) := splitNumber n
          let leftCount ← helper left (k-1)
          let rightCount ← helper right (k-1)
          pure (leftCount + rightCount)
        else
          -- Multiply by 2024
          helper (n * 2024) (k-1)

      -- Cache result
      modify fun m => { cache := m.cache.insert key result }
      return result

  helper n k |>.run' memo

#eval nStepsCount 125 6 + nStepsCount 17 6 -- test with example from puzzle

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  let l := parseNats input
  let counts := l.map (fun n => nStepsCount n 75)
  let result := counts.foldl Nat.add 0
  IO.println s!"Number of stones after 75 blinks: {result}"
