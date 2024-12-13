import Init.System.FilePath
import Init.System.IO
import AocDay12


def getComponentPerimeter (grid: Grid) (component: Array Node) : Nat :=
  let connections := component.map (λ n => (grid.getConnectedNeighbors (nodeIdToRC grid.width n.id).fst (nodeIdToRC grid.width n.id).snd).length)
  4 * component.size - connections.foldl Nat.add 0

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let grid := readProblem contents
  let graph := gridToGraph grid
  let components := getConnectedComponents graph
  let componentPrices := components.map (λ c => (getComponentArea c) * (getComponentPerimeter grid c))
  let sum := componentPrices.foldl Nat.add 0
  IO.println s!"{sum}"
