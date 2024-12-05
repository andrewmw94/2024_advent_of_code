def parseGrid (input : String) : Array (Array Char) :=
  -- Split input into lines
  let lines := input.splitOn "\n"

  -- Check if there are any lines
  if lines.isEmpty then
    #[]
  else
    -- Get the length of the first line as reference
    let expectedLen := lines[0]!.length
    -- Check if all lines have the same length
    if !lines.all (·.length == expectedLen) then
      #[]
    else
      -- Convert each line to array of chars
      let grid := lines.map (·.data.toArray)
      grid.toArray

def getChar (grid : Array (Array Char)) (x y : Int) : Option Char :=
  if x < 0 || y < 0 then
    none
  else
    let x := x.toNat
    let y := y.toNat
    if h : x < grid.size then
      let row := grid.get ⟨x, h⟩
      if h : y < row.size then
        some <| row.get ⟨y, h⟩
      else
        none
    else
      none
