import AocDay4

def checkMatch (grid: Array (Array Char)) (x y: Int) (x_step y_step: Int) (l: List Char) : Bool :=
  match l with
  | [] => true
  | h::tail =>
    match getChar grid x y with
    | some c =>
      if c == h then
        -- Only do bounds check if tail is nonempty
        if tail == [] then
          true
        else
          checkMatch grid (x + x_step) (y + y_step) x_step y_step tail
      else
        false
    | none => false

def countMatches (grid: Array (Array Char)) (l: List Char) : Id Nat := do
  let height := grid.size
  if height == 0 then 0
  else
    let width := grid[0]!.size
    let directions := [
      (0, 1),
      (0, -1),
      (1, 0),
      (-1, 0),
      (1, 1),
      (1, -1),
      (-1, 1),
      (-1, -1)
    ]
    let mut count := 0
    for i in [:height] do
      for j in [:width] do
        for (dx, dy) in directions do
          if checkMatch grid i j dx dy l then
            count := count + 1
    pure count


def main : IO Unit := do
  let filename := "input.txt"
  let contents ← IO.FS.readFile filename
  let grid := parseGrid contents
  let s := "XMAS"
  let count := countMatches grid s.toList
  IO.print s!"Num matches: {count}\n"
