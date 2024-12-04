import Init.System.FilePath
import Init.System.IO

-- The problem statement says "numbers" instead of Nats or Ints. We assume Nats and don't handle `-`.
def isDigit (c : Char) : Bool :=
  '0' ≤ c && c ≤ '9'

theorem dropOneDecreasesLen : ∀ (s : String), s.length > 0 → (s.drop 1).length < s.length := by
  intro s h
  simp only [String.length, String.drop]
  simp [String.toSubstring, Substring.drop]
  sorry

def parseMulExprs (s: String) : List (Nat × Nat) :=
  if h: s.length > 0 then
    if s.startsWith "mul(" then
      let lparen_idx := s.posOf '('
      let comma_idx := s.posOf ','
      let rparen_idx := s.posOf ')'
      if comma_idx > rparen_idx then
        parseMulExprs (s.drop 1)
      else
        let n1 := s.extract (String.next s lparen_idx) comma_idx
        let n2 := s.extract (String.next s comma_idx) rparen_idx
        let all_digits := n1.all isDigit && n2.all isDigit
        if all_digits then
          (n1.toNat!, n2.toNat!) :: parseMulExprs (s.drop 1)
        else
          parseMulExprs (s.drop 1)
    else
      parseMulExprs (s.drop 1)
    else
      []
termination_by s.length
decreasing_by
  -- Case 1: s.drop 1
  exact dropOneDecreasesLen s h
  -- Case 2: s.drop rparen_idx.byteIdx
  exact dropOneDecreasesLen s h


def parseMulFile (filename : System.FilePath) : IO (List (Nat × Nat)) := do
  let contents ← IO.FS.readFile filename
  pure (parseMulExprs contents)

-- -- Test cases
-- #eval parseMulExprs "mul(2,4)"                      -- [(2,4)]
-- #eval parseMulExprs "xmul(2,4)%&mul[3,7]"           -- [(2,4)]
-- #eval parseMulExprs "mul(1,2)mul( 2,4)"             -- [(1,2)]
-- #eval parseMulExprs "mul(1,2) mul(3,4) mul(5,6)"    -- [(1,2), (3,4), (5,6)]
-- -- The problem statement says "numbers" instead of Nats or Ints. We assume Nats, so this is empty.
-- #eval parseMulExprs "mul(-1,2) mul(3,-4)"           -- []
-- #eval parseMulExprs "nomul here"                    -- []
