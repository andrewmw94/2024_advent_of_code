structure Grid where
  data : Array (Array Nat)
deriving Repr

def Grid.getCell (g : Grid) (x : Int) (y : Int) : Option Nat := do
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
    let nats := row.toList.map (λ c => c.toString.toNat!)
    grid := grid.push nats.toArray

  return {data:=grid.reverse}



def getZeros (g: Grid) (r c: Int) (curr: Nat): List (Int × Int):=
  if curr > 0 then
    match g.getCell r c with
    | some n =>
      let neighbors := [(r-1, c), (r+1,c), (r, c-1), (r, c+1)]
      let pred_coords := (neighbors.filter (λ (r, c) => match (g.getCell r c) with
      | none => false
      | some m => m == curr-1))
      (pred_coords.map (λ (r, c) => getZeros g r c (curr-1))).flatten
    | none => []
  else
    [(r, c)]

def getUniqueZeros (g: Grid) (r c : Int) : List (Int × Int) :=
  let allZeros := getZeros g r c 9
  allZeros.foldl (λ acc (r, c) => if acc.contains (r, c) then acc else (r, c) :: acc) []

def getNines (g: Grid): Id (List (Int × Int)) := do
  let mut res := #[]
  for r_idx in [:g.data.size] do
    for c_idx in [:(g.data.get! 0).size] do
      match g.getCell r_idx c_idx with
      | some 9 => res := res.push ((r_idx: Int), (c_idx: Int))
      | _ => ()
  res.toList
