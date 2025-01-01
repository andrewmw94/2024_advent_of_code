import AocDay6.Grid
import Std.Data.HashSet.Lemmas

-- To help prove termination:

theorem contains_implies_non_empty (s : Std.HashSet Nat) (x : Nat) :
    s.contains x → ¬s.isEmpty := by
  intro h
  false_or_by_contra
  rename_i h'
  have my_h : ¬x ∈ s := Std.HashSet.not_mem_of_isEmpty h'
  contradiction

theorem non_empty_implies_size_non_zero (s : Std.HashSet Nat) :
    ¬s.isEmpty → s.size ≠ 0 := by
  simp [Std.HashSet.isEmpty_eq_size_eq_zero]

theorem erase_size_decreases (s : Std.HashSet Nat) (x : Nat) :
    s.contains x → (s.erase x).size < s.size := by
  intro h_contains
  simp [Std.HashSet.size_erase]
  have h: (if x ∈ s then s.size - 1 else s.size) = s.size - 1 := by
    split
    rfl
    contradiction
  rw [h]
  have h_non_empty : ¬s.isEmpty := contains_implies_non_empty s x h_contains
  have h_pos : s.size ≠ 0 := non_empty_implies_size_non_zero s h_non_empty
  exact Nat.pred_lt h_pos

-- Toy example function where the proof is analagous to the proof for our function.
def f1 (set: Std.HashSet Nat) (n: Nat) : Option (Std.HashSet Nat) :=
  if h: set.contains n then
    have decrease : (set.erase n).size < set.size := erase_size_decreases set n h
    f1 (set.erase n) (n+1)
  else
    none
termination_by set.size

-- For now we assume this since we proved it for Nat and it shouldn't really depend on the type.
-- Adding it requires writing instances of LawfulBEq and LawfulHashable for Pose (and more proofs).
theorem erase_size_decreases_pose (s : Std.HashSet Pose) (x : Pose) :
    s.contains x → (s.erase x).size < s.size := by sorry

theorem next_state_decrease_unvisited (g: Grid) (s: State) : getNextState g s = moveResults.newState s' → s'.unvisitedSet.size < s.unvisitedSet.size := by
  intro h
  unfold getNextState at h
  split at h
  simp at h
  split at h
  split at h
  rename_i h_contains
  rename_i p _
  have h_s'_unvisited : s'.unvisitedSet = s.unvisitedSet.erase p :=
    have h_s' : s' = {pose := p, history := p :: s.history, unvisitedSet := s.unvisitedSet.erase p} := by
      injection h
      rename_i h
      rw [← h]
    by simp [h_s']
  rw [h_s'_unvisited]
  exact erase_size_decreases_pose s.unvisitedSet p h_contains
  contradiction
  contradiction
