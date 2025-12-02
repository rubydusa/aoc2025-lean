def NatNotMulOf (m : Nat) := { n : Nat // ¬ m ∣ n }

inductive Instruction (m: Nat)
| L (n: NatNotMulOf m)
| R (n: NatNotMulOf m)

structure State (m: Nat) where
  dial: Int
  times: Nat

def start_state: State 100 := {
  dial := 50,
  times := 0
}

def step (s: State m) (i: Instruction m) : State m :=
  let newDial: Int := match i with
  | Instruction.L n => s.dial - n.1
  | Instruction.R n => s.dial + n.1
  let newTimes: Nat := if newDial % m = 0 then s.times + 1 else s.times
  {
    dial := newDial,
    times := newTimes
  }

def run (is: List (Instruction m)) (s: State m): State m :=
  is.foldl step s

-------------------------------------
-- Theorems                        --
-------------------------------------

theorem step_dial_L (s : State m) (n : NatNotMulOf m) :
  (step s (Instruction.L n)).dial = s.dial - n.1 := by
  simp [step]

theorem step_dial_R (s : State m) (n : NatNotMulOf m) :
  (step s (Instruction.R n)).dial = s.dial + n.1 := by
  simp [step]

theorem nat_eq_zero_of_zero_eq_zero_sub (n : Nat)
    (h : (0 : Int) = 0 - (n : Int)) :
    n = 0 := by
  have h' := congrArg (fun z : Int => z + (n : Int)) h
  have h'' : (n : Int) = 0 := by
    simpa [Int.sub_eq_add_neg, Int.add_comm, Int.add_right_neg] using h'
  exact Int.ofNat_eq_zero.mp h''

theorem does_not_return_zero
  (s0: State m) (hp: s0.dial = 0) :
  ∀ (i : Instruction m), let s1 := step s0 i; s1.dial ≠ 0 :=
  fun hi s1_dial_zero =>
    match hi with
    | Instruction.L n =>
      let sdl := step_dial_L s0 n
      let h₁ := calc
        (step s0 (Instruction.L n)).dial = s0.dial - n.1 := sdl
        _                                = 0 - n.1 := by rw [hp]
      let h₂ := s1_dial_zero.symm.trans h₁
      -- this looks dirty
      let n_eq0 : n.1 = 0 := nat_eq_zero_of_zero_eq_zero_sub n.1 h₂
      let mDividesZero : m ∣ 0 := ⟨0, by simp⟩
      let contradiction : m ∣ n.1 := by simpa [n_eq0] using mDividesZero
      absurd contradiction n.2
    | Instruction.R n =>
      let stepDialR := step_dial_R s0 n
      let h₁ := calc
        (step s0 (Instruction.R n)).dial = s0.dial + n.1 := stepDialR
        _                                = 0 + n.1 := by rw [hp]
        _                                = n.1     := by simp
      -- feeling kind of iffy about to congrArg Int.toNat
      let n_eq0 : n.1 = 0 := congrArg Int.toNat (s1_dial_zero.symm.trans h₁).symm
      let mDividesZero : m ∣ 0 := ⟨0, by simp⟩
      let contradiction : m ∣ n.1 := by simpa [n_eq0] using mDividesZero
      absurd contradiction n.2

-- eh fuck it the definitions I wrote are bad, not going to finish proving this
-- theorem run_empty_list_no_change
--   (s0: State m) : (run [] s0).times = s0.times := by
--   simp [run]

-- theorem run_times_upper_bound_half_empty_list
--   (s0: State m) (hp: s0.times = 0) :
--   let sn := run [] s0; sn.times ≤ (([] : List (Instruction m)).length / 2) :=
--     let sn := run [] s0
--     let sn_times := calc
--       sn.times = s0.times := run_empty_list_no_change s0
--       _        = 0        := hp
--     let sn_times_le_zero := Nat.le_zero.mpr sn_times
--     by
--       rw [List.length_nil, Nat.zero_div]
--       exact sn_times_le_zero

-- theorem run_times_upper_bound_half_dial_zero
--   (s0 : State m) (hp₁ : s0.times = 0) (hp₂ : s0.dial = 0):
--   ∀ (steps: List (Instruction m)), let sn := run steps s0; sn.times ≤ (steps.length / 2) :=
--   fun steps =>
--     match steps with
--     | [] => run_times_upper_bound_half_empty_list s0 hp₁
--     | h₁ :: t₁ =>
--       let s1_zero_times := does_not_return_zero s0 hp₂ h₁
--       match t₁ with
--       | [] =>
--         sorry
--       | h₂ :: t₂ =>
--         sorry
