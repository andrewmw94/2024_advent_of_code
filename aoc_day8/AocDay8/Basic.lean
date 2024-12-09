structure Grid where
  data : Array (Array Char)

def Grid.getCell (g : Grid) (x : Int) (y : Int) : Option Char := do
  if y < 0 ∨ y >= g.data.size then
    none
  else
  let row := g.data.get! y.toNat
  if x < 0 ∨ x >= row.size then
    none
  else
    some (row.get! x.toNat)


def readProblem (input : String) : Id Grid := do
  let rows := input.splitOn "\n"
  -- Convert each row into an Array of characters
  let mut grid := #[]

  for row in rows do
    let chars := row.toList
    grid := grid.push chars.toArray

  return {data:=grid.reverse}

def findAlphas (g : Grid) : Id (Array (Char × Array (Int × Int))) := do
  let mut alphas : Array (Char × Array (Int × Int)) := #[]
  for i in [:g.data.size] do
    for j in [:(g.data[i]!).size] do
      let c := g.data[i]![j]!
      if c.isAlphanum then
        match alphas.findIdx? (fun (char, _) => char == c) with
        | some idx => alphas :=
          alphas.modify idx (λ p => (p.fst, p.snd.push (i,j)))
        | none => alphas := alphas.push (c, #[(i,j)])
  return alphas
