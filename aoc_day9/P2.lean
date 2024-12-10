import Init.System.FilePath
import Init.System.IO
import AocDay9

def nextEmpty (input: Array (Option Nat)) (idx: Nat) : Nat :=
  if h:idx < input.size then
    if (input.get ⟨idx, h⟩).isNone then
      idx
    else
      nextEmpty input (idx + 1)
  else
    idx
termination_by input.size - idx

def findFirstSlotForFile (a : Array FileOrEmpty) (f: FileDesc) (back_idx : Nat) : Option Nat := do
  for idx in [:back_idx] do
    let c := a.get! idx
    match c with
    | FileOrEmpty.empty l =>
      if l.size >= f.size then
        return idx
      else
        continue
    | _ => continue
  none

def defragment (a : Array FileOrEmpty) (back_idx : Nat) (orig : Array FileOrEmpty) (orig_back_idx : Nat) : Array FileOrEmpty :=
  if orig_back_idx > 1 then
    match orig.get? orig_back_idx with
    | some (FileOrEmpty.file f) =>
      let file_idx := (a.findIdx? (fun c => c == FileOrEmpty.file f)).get!
      match findFirstSlotForFile a f file_idx with
      | some new_idx =>
        let space := a.get! new_idx
        if f.size == space.size then
          defragment ((a.set! file_idx (FileOrEmpty.empty {size:=(f.size)})).set! new_idx (FileOrEmpty.file f)) (back_idx - 1) (orig) (orig_back_idx-1)
        else
          let front := (((a.take new_idx).push (FileOrEmpty.file f)).push (FileOrEmpty.empty {size:=(space.size - f.size)}))
          let back := ((a.set! file_idx (FileOrEmpty.empty {size:=(f.size)})).toList.drop (new_idx+1)).toArray
          defragment (front.append back) (back_idx) (orig) (orig_back_idx-1) -- back_idx unchanged since we added another element
      | _ => defragment a (back_idx - 1) (orig) (orig_back_idx-1)
    | _ => defragment a (back_idx - 1) (orig) (orig_back_idx-1)
  else
    a
termination_by orig_back_idx

partial def unproven_defragment (a : Array FileOrEmpty) (back_idx : Nat) : Array FileOrEmpty :=
  if back_idx > 1 then
    match a.get? back_idx with
    | some (FileOrEmpty.file f) =>
      match findFirstSlotForFile a f back_idx with
      | some new_idx =>
        let space := a.get! new_idx
        if f.size == space.size then
          unproven_defragment ((a.set! back_idx (FileOrEmpty.empty {size:=(f.size)})).set! new_idx (FileOrEmpty.file f)) (back_idx - 1)
        else
          let front := (((a.take new_idx).push (FileOrEmpty.file f)).push (FileOrEmpty.empty {size:=(space.size - f.size)}))
          let back := ((a.set! back_idx (FileOrEmpty.empty {size:=(f.size)})).toList.drop (new_idx+1)).toArray
          unproven_defragment (front.append back) (back_idx) -- back_idx unchanged since we added another element
      | _ => unproven_defragment a (back_idx - 1)
    | _ => unproven_defragment a (back_idx - 1)
  else
    a

def checksum (input: Array (Option Nat)) : Id Nat := do
  let mut sum := 0
  for idx in [:input.size] do
    let c := input.get! idx
    match c with
    | none => continue
    | some n =>
      sum := sum + (n * idx)
  sum

-- Should be 6448168620520
def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readProblem2 contents
  let a := s.toArray
  -- IO.println s!"Before: {toOptNat a}"
  let defragmented := defragment a (a.size-1) a (a.size-1)
  -- IO.println s!"After: {toOptNat defragmented}"
  IO.println s!"{checksum (toOptNat defragmented)}"
