def readProblem (s: String) : List Nat :=
  s.splitOn "\n"
  |> List.filter (· ≠ "")  -- Remove empty lines
  |> List.map String.trim  -- Remove whitespace
  |> List.filterMap String.toNat?  -- Convert to Nat, skip invalid


def mix (secret num : Nat) : Nat :=
  secret.xor num

def prune (secret : Nat) : Nat :=
  secret % 16777216

def generateNextSecret (secret : Nat) : Nat :=
  let step1 := prune (mix secret (secret * 64))
  let step2 := prune (mix step1 (step1 / 32))
  prune (mix step2 (step2 * 2048))

