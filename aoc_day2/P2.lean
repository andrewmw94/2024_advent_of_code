import AocDay2

def removeAt (xs : List α) (i : Nat) : List α :=
  (xs.take i) ++ (xs.drop (i + 1))

def checkRelaxedValid (list : List Int) : Bool :=
  if list.length ≤ 2 then
    true
  else
    List.range list.length
      |>.any (fun i => checkSafe (removeAt list i))

def main : IO Unit := do
  let l  ← parseIntRows "numbers.txt"
  IO.println s!"Num Safe with one deletion: {List.length (l.filter checkRelaxedValid)}"
