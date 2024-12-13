import AocDay12.Graph

structure Grid where
  data : Array (Array Char)
  width: Nat
deriving Repr

def Grid.getCell (g : Grid) (x : Int) (y : Int) : Option Char := do
  if y < 0 ∨ y >= g.data.size then
    none
  else
  let row := g.data.get! y.toNat
  if x < 0 ∨ x >= row.size then
    none
  else
    some (row.get! x.toNat)

def Grid.getConnectedNeighbors (g: Grid) (r c: Int) : List (Int × Int) :=
  [(r, (c + 1)), (r, (c - 1)), ((r + 1), c), ((r - 1), c)].filter (λ (r2, c2) => (g.getCell r2 c2).isSome ∧ g.getCell r2 c2 == g.getCell r c)

def readProblem (input : String) : Id Grid := do
  let rows := input.splitOn "\n"
  -- Convert each row into an Array of characters
  let mut grid := #[]

  for row in rows do
    grid := grid.push row.toList.toArray

  let width := match grid.get? 0 with
    | none => 0
    | some row => row.size

  return {width, data:=grid.reverse}

def getNodeId (width: Int) (r: Int) (c: Int) : Nat :=
  (r * width + c).toNat

def nodeIdToRC (width: Int) (id: Nat) : (Int × Int) :=
  ((id: Int) / width, (id: Int) % width)

-- Helper to check if two cells should be connected
def shouldConnect (grid: Grid) (r1 c1 r2 c2: Int) : Bool :=
  match grid.getCell r1 c1 with
  | none => false
  | some char =>
    match grid.getCell r2 c2 with
    | none => false
    | some char2 => char == char2

def gridToGraph (grid: Grid) : Id Graph := do
  let height := grid.data.size
  if height == 0 then
    return { edges := #[] }
  else
    let width := grid.width
    -- Initialize empty adjacency lists for each node
    let totalNodes := width * height
    let mut edges := mkArray totalNodes []

    -- Add edges for each cell
    for y in [0:height] do
      for x in [0:width] do
        let nodeId := getNodeId width x y
        let node := { id := nodeId }

        -- Check all four directions
        -- Right
        if shouldConnect grid x y (x + 1) y then
          let destId := getNodeId width (x + 1) y
          edges := edges.modify nodeId fun list =>
            { src := node, dest := { id := destId }, weight := 1 } :: list

        -- Left
        if shouldConnect grid x y (x - 1) y then
          let destId := getNodeId width (x - 1) y
          edges := edges.modify nodeId fun list =>
            { src := node, dest := { id := destId }, weight := 1 } :: list

        -- Up
        if shouldConnect grid x y x (y + 1) then
          let destId := getNodeId width x (y + 1)
          edges := edges.modify nodeId fun list =>
            { src := node, dest := { id := destId }, weight := 1 } :: list

        -- Down
        if shouldConnect grid x y x (y - 1) then
          let destId := getNodeId width x (y - 1)
          edges := edges.modify nodeId fun list =>
            { src := node, dest := { id := destId }, weight := 1 } :: list

    return { edges := edges }


def getConnectedComponents (graph: Graph): Id (Array (Array Node)) := do
  let mut visited := []
  let mut components := #[]
  for nodeId in [:graph.edges.size] do
    if ¬(visited.contains nodeId) then
      let component := Graph.dfs graph {id := nodeId}
      components := components.push component.toArray
      for node in component do
        visited := node.id :: visited
  return components

def getComponentArea (component: Array Node) : Nat :=
  component.size
