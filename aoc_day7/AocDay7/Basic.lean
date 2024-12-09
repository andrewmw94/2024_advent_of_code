def parseLine (line : String) : Option (Nat × List Nat) := do
  let parts := line.splitOn ":"
  if parts.length != 2 then
    none
  else
    let idStr := parts[0]!.trim
    let numbersStr := parts[1]!.trim
    let id ← idStr.toNat?
    let numbers ← numbersStr.splitOn " "
      |>.filter (·.length > 0)  -- Handle multiple spaces
      |>.mapM String.toNat?
    some (id, numbers)

def parseFile (contents : String) : Option (List (Nat × List Nat)) := do
  let lines := contents.splitOn "\n"
    |>.filter (·.length > 0)  -- Skip empty lines
  lines.mapM parseLine
