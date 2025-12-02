-- IO
import Std
open Std

structure IDRange where
  start : Nat
  stop : Nat
  deriving Repr

def validate_id_range (s: String) : Except String IDRange := do
  let parts := s.splitToList (· == '-')
  if _h : parts.length = 2 then
    let x₁ :: xs := parts
    let x₂ :: [] := xs
    let x₁_nat ←
      match x₁.toNat? with
      | none     => throw s!"could not parse number in: {x₁}"
      | some n   => pure n
    let x₂_nat ←
      match x₂.toNat? with
      | none     => throw s!"could not parse number in: {x₂}"
      | some n   => pure n

    return {
      start := x₁_nat,
      stop := x₂_nat
    }
  else
    throw s!"bad parts: {parts}"

def parse_id_ranges (s : String) : Except String (List IDRange) := do
  let terms := s.splitToList (· == ',')
  terms.mapM validate_id_range

def load_id_ranges (path : System.FilePath) :
    IO (Except String (List IDRange)) := do
  let contents ← IO.FS.readFile path
  pure (parse_id_ranges contents)

-- PURE

def nat_digits (n : Nat) : List Nat := ((Nat.toDigits 10 n).map Char.toNat).map (· - 48)

def digits_nat_aux (xs: List Nat) (p : Nat) : Nat :=
  match xs with
  | [] => 0
  | x :: xs' => x * (10 ^ p) + digits_nat_aux xs' (p + 1)

def digits_nat (xs: List Nat) : Nat :=
  digits_nat_aux (xs.reverse) 0

-- I fucked up reading the question
-- I'm giving up
def invalid_ids_in_range (id_range: IDRange) :=
  let start_digits := nat_digits id_range.start
  let stop_digits  := nat_digits id_range.stop
  let start_digits_div_2  := start_digits.length / 2
  let stop_digits_div_2   := stop_digits.length / 2

  let total_invalid_id_range := (10 ^ stop_digits_div_2) - 1

  let lower_cutoff := if start_digits.length % 2 = 1 then
    (10 ^ start_digits_div_2) - 1
  else
    let first_half := List.take (start_digits.length / 2) start_digits
    let second_half := List.drop (start_digits.length / 2) start_digits
    let first_half_nat := digits_nat first_half
    let second_half_nat := digits_nat second_half
    first_half_nat - Bool.toNat (second_half_nat ≤ first_half_nat)

  let higher_cutoff := if stop_digits.length % 2 = 1 then
    0
  else
    let first_half := List.take (stop_digits.length / 2) stop_digits
    let second_half := List.drop (stop_digits.length / 2) stop_digits
    let first_half_nat := digits_nat first_half
    let second_half_nat := digits_nat second_half

    let higher_bound := 10 ^ stop_digits_div_2

    (higher_bound - first_half_nat) - Bool.toNat (second_half_nat ≥ first_half_nat)

  dbg_trace s!"total_invalid_id_range: {total_invalid_id_range}"
  dbg_trace s!"lower_cutoff: {lower_cutoff}"
  dbg_trace s!"higher_cutoff: {higher_cutoff}"

  total_invalid_id_range - lower_cutoff - higher_cutoff

def invalid_ids_in_ranges (id_ranges: List IDRange) : Nat :=
  (id_ranges.map invalid_ids_in_range).sum
