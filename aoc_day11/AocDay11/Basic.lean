def parseNats (input : String) : List Nat :=
  (input.splitOn " ").map String.toNat!


def Nat.toString (n: Nat) : String :=
  s!"{n}"

def countDigits (n : Nat) : Nat :=
  n.toString.length

/-- Split a number into two parts based on digit count -/
def splitNumber (n : Nat) : (Nat × Nat) :=
  let half := n.toString.length/2
  ((n.toString.toList.take half).asString.toNat!, (n.toString.toList.drop half).asString.toNat!)

/-- Apply transformation rules to a single number -/
def transform (n : Nat) : List Nat :=
  if n = 0 then
    [1]
  else if countDigits n % 2 = 0 then
    let (left, right) := splitNumber n
    [left, right]
  else
    [n * 2024]

/-- Apply one step of transformation to the entire list -/
def step (nums : List Nat) : List Nat :=
  nums.flatMap transform

/-- Apply n steps of transformation -/
def nSteps (n : Nat) (nums : List Nat) : List Nat :=
  match n with
  | 0 => nums
  | n+1 => nSteps n (step nums)
