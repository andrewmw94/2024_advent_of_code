import Init.System.FilePath
import Init.System.IO

def readTwoColumnFile (filename : System.FilePath) : IO (List Int × List Int) := do
  let contents ← IO.FS.readFile filename
  let lines := contents.splitOn "\n" |>.filter (·.length > 0)

  -- Process each line to get pairs of numbers
  let pairs ← lines.mapM (fun line => do
    let numbers := line.splitOn " " |>.filter (·.length > 0)
    if numbers.length != 2 then
      throw (IO.Error.userError s!"Invalid line format: {line}")

    -- Parse each number
    let n1 ← match numbers[0]!.trim.toInt? with
      | some n => pure n
      | none => throw (IO.Error.userError s!"Invalid number: {numbers[0]!}")

    let n2 ← match numbers[1]!.trim.toInt? with
      | some n => pure n
      | none => throw (IO.Error.userError s!"Invalid number: {numbers[1]!}")

    pure (n1, n2))

  -- Split pairs into two lists
  let col1 := pairs.map (·.1)
  let col2 := pairs.map (·.2)

  pure (col1, col2)

