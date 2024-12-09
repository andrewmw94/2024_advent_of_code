import Init.System.FilePath
import Init.System.IO
import AocDay7

def checkLine (target: Nat) (numbers: List Nat) : Bool :=
  match numbers with
  | [] => false
  | n :: [] => n == target
  | n :: ns =>
    if n > target then
      false
    else
      if target.mod n == 0 then
        checkLine (target / n) ns ∨ checkLine (target - n) ns
      else
        checkLine (target - n) ns


def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let lines := parseFile contents
  match lines with
  | none => IO.println "parse error"
  | some lines =>
    let legalLines := lines.filter (λ l => checkLine l.fst l.snd.reverse)
    let sum := legalLines.foldl (λ acc l => acc + l.fst) 0
    IO.println s!"Sum: {sum}"
