import Init.System.FilePath
import Init.System.IO
import AocDay12

def addsCornerNE (north northEast east: Bool) : Bool :=
  (¬(north ∨ east)) ∨ (north ∧ east ∧ ¬northEast)

def countCorners (grid: Grid) (r c: Int): Nat :=
  let north := grid.getCell (r+1) c == grid.getCell r c
  let south := grid.getCell (r-1) c == grid.getCell r c
  let east := grid.getCell r (c+1) == grid.getCell r c
  let west := grid.getCell r (c-1) == grid.getCell r c
  let northEast := grid.getCell (r+1) (c+1) == grid.getCell r c
  let southEast := grid.getCell (r-1) (c+1) == grid.getCell r c
  let northWest := grid.getCell (r+1) (c-1) == grid.getCell r c
  let southWest := grid.getCell (r-1) (c-1) == grid.getCell r c
  ([addsCornerNE north northEast east, addsCornerNE east southEast south, addsCornerNE south southWest west, addsCornerNE west northWest north].map (λ b => if b then 1 else 0)).foldl Nat.add 0

def getComponentSideCount (grid: Grid) (component: Array Node) : Nat :=
  let cornerCounts := component.map (λ n => countCorners grid (nodeIdToRC grid.width n.id).fst (nodeIdToRC grid.width n.id).snd)
  cornerCounts.foldl Nat.add 0

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let grid := readProblem contents
  let graph := gridToGraph grid
  let components := getConnectedComponents graph
  let componentPrices := components.map (λ c => (getComponentArea c) * (getComponentSideCount grid c))
  let sum := componentPrices.foldl Nat.add 0
  IO.println s!"{sum}"
