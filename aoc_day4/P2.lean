import AocDay4

def checkX (grid: Array (Array Char)) (x y: Int) : Bool :=
  match getChar grid x y with
    | some 'A' =>
      match getChar grid (x-1) (y+1), getChar grid (x-1) (y-1), getChar grid (x+1) (y+1), getChar grid (x+1) (y-1) with
        | some ne, some nw, some se, some sw =>
          (ne == 'M' && sw == 'S' || ne == 'S' && sw == 'M') && (nw == 'M' && se == 'S' || nw == 'S' && se == 'M')
        | _,_,_,_ => false
    | _ => false

def countXs (grid: Array (Array Char)) : Id Nat := do
  let height := grid.size
  if height == 0 then 0
  else
    let width := grid[0]!.size
    let mut count := 0
    for i in [:height] do
      for j in [:width] do
        if checkX grid i j then
            count := count + 1
    pure count


def main : IO Unit := do
  let filename := "input.txt"
  let contents ← IO.FS.readFile filename
  let grid := parseGrid contents
  let count := countXs grid
  IO.print s!"Num matches: {count}\n"
