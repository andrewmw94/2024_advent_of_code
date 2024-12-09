import Init.System.FilePath
import Init.System.IO
import AocDay7

def concatNats (n1 n2: Nat) : Nat :=
  (toString n1 ++ toString n2).toNat!

def checkLine (target: Nat) (numbers: List Nat) (acc: Nat): Bool :=
  match numbers with
  | [] => acc == target
  | n :: ns =>
    if acc > target then
      false
    else
      checkLine target ns (n*acc) ∨ checkLine target ns (n+acc) ∨ checkLine target ns (concatNats acc n)
termination_by numbers.length


def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let lines := parseFile contents
  match lines with
  | none => IO.println "parse error"
  | some lines =>
    let legalLines := lines.filter (λ l => checkLine l.fst l.snd.tail! l.snd.head!)
    let sum := legalLines.foldl (λ acc l => acc + l.fst) 0
    IO.println s!"Sum: {sum}"
