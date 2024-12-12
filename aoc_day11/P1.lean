import Init.System.FilePath
import Init.System.IO
import AocDay11

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  let l := parseNats input
  let result := nSteps 25 l
  IO.println s!"Number of stones after 25 blinks: {result.length}"
