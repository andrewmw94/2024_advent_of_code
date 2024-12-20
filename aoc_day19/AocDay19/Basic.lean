import Init.Data.String.Basic
import Init.Data.List.Basic
import Std.Data.HashMap

def String.split_on_char (c : Char) (s : String) : List String :=
  (s.split (fun x => x == c)).filter (fun x => x ≠ "")

structure TowelPuzzle where
  patterns : List String
  designs : List String

def parse_input (input : String) : TowelPuzzle :=
  let lines := input.split (fun c => c == '\n')
  let patterns := (lines.head!).split_on_char ',' |>.map String.trim
  let designs := lines.drop 2 |>.filter (fun s => s ≠ "")
  ⟨patterns, designs⟩

partial def countBuilds (pattern: String) (pieces : Array String) (memo: Std.HashMap String Nat) (front: Bool) : Id (Nat × (Std.HashMap String Nat)) := do
  match pattern with
  | "" => return (1, memo)
  | _ =>
    match memo.get? pattern with
    | some count => return (count, memo)
    | none =>
      let mut count := 0
      let mut newMemo := memo
      if front then
        for piece in pieces do
          if pattern.startsWith piece then
            let res ← countBuilds (pattern.drop piece.length) pieces newMemo !front
            newMemo := res.snd
            count := count + res.fst
        newMemo := newMemo.insert pattern count
        return (count, newMemo)
      else
        for piece in pieces do
          if pattern.endsWith piece then
            let res ← countBuilds (pattern.dropRight piece.length) pieces newMemo !front
            newMemo := res.snd
            count := count + res.fst
        newMemo := newMemo.insert pattern count
        return (count, newMemo)
