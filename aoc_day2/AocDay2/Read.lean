import Init.System.FilePath
import Init.System.IO

def parseIntRows (filename : System.FilePath) : IO (List (List Int)) := do
  let contents ← IO.FS.readFile filename
  let lines := contents.splitOn "\n" |>.filter (·.length > 0)

  -- Process each line to get list of numbers
  lines.mapM (fun line => do
    let numbers := line.splitOn " " |>.filter (·.length > 0)

    -- Parse each number in the line
    numbers.mapM (fun numStr => do
      match numStr.trim.toInt? with
      | some n => pure n
      | none => throw (IO.Error.userError s!"Invalid number: {numStr}"))
  )
