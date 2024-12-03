import AocDay2

def main : IO Unit := do
  let l  ← parseIntRows "numbers.txt"
  IO.println s!"Num Safe: {List.length (l.filter checkSafe)}"
