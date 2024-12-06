import Init.System.FilePath
import Init.System.IO
import AocDay5

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let p := parseFileString contents
  let constraints := p.fst
  let data := p.snd

  -- Not stated, but on our input, the constraints are already closed

  let validLists := data.filter (λ l => checkListValidForwards constraints l ∧ checkListValidBackwards constraints l.reverse)
  let middles := validLists.map getMiddleElement
  let sum := middles.foldl (λ acc x => acc + x) 0
  IO.print s!"Sum: {sum}\n"
