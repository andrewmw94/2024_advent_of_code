import Init.System.FilePath
import Init.System.IO

-- The problem statement says "numbers" instead of Nats or Ints. We assume Nats and don't handle `-`.
def isDigit (c : Char) : Bool :=
  '0' ≤ c && c ≤ '9'

def List.startsWith : List Char → List Char → Bool
  | _, [] => true
  | [], _ => false
  | x::xs, y::ys => x == y && List.startsWith xs ys

#eval List.startsWith "mul(2,4)".toList "mul(".toList

def List.posOf : List Char → Char → Nat
  | [], _ => 0
  | x::xs, c => if x == c then 0 else 1 + List.posOf xs c

#eval List.posOf "mul(2,4)".toList '('

def List.extract (xs : List Char) (start : Nat) (end_idx : Nat) : List Char :=
  (xs.drop start).take (end_idx - start)

#eval List.extract "mul(2,4)".toList 0 3

def parseMulExprs (s: List Char) : List (Nat × Nat) :=
  if s.length > 0 then
    if s.startsWith "mul(".toList then
      let lparen_idx := s.posOf '('
      let comma_idx := s.posOf ','
      let rparen_idx := s.posOf ')'
      if comma_idx > rparen_idx then
        parseMulExprs (s.drop 1)
      else
        let n1 := s.extract (lparen_idx + 1) comma_idx
        let n2 := s.extract (comma_idx + 1) rparen_idx
        let all_digits := n1.all isDigit && n2.all isDigit
        if all_digits then
          ((String.mk n1).toNat!, (String.mk n2).toNat!) :: parseMulExprs (s.drop 1)
        else
          parseMulExprs (s.drop 1)
    else
      parseMulExprs (s.drop 1)
    else
      []

#eval parseMulExprs "mul(2,4)".toList

def parseMulExprsFromString (s: String) : List (Nat × Nat) :=
  parseMulExprs s.toList


def parseMulFile (filename : System.FilePath) : IO (List (Nat × Nat)) := do
  let contents ← IO.FS.readFile filename
  pure (parseMulExprsFromString contents)

-- Test cases
#eval parseMulExprsFromString "mul(2,4)"                      -- [(2,4)]
#eval parseMulExprsFromString "xmul(2,4)%&mul[3,7]"           -- [(2,4)]
#eval parseMulExprsFromString "mul(1,2)mul( 2,4)"             -- [(1,2)]
#eval parseMulExprsFromString "mul(1,2) mul(3,4) mul(5,6)"    -- [(1,2), (3,4), (5,6)]
-- The problem statement says "numbers" instead of Nats or Ints. We assume Nats, so this is empty.
#eval parseMulExprsFromString "mul(-1,2) mul(3,-4)"           -- []
#eval parseMulExprsFromString "nomul here"                    -- []


def main : IO Unit := do
  let pairs ← parseMulFile "input.txt"
  let terms := pairs.map (fun p => p.fst * p.snd)
  let sum := terms.foldl (fun acc t => acc + t) 0
  IO.print s!"Sum: {sum}\n"