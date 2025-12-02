import Aoc2025.Day1
import Aoc2025.Day1IO

def instruction_io_to_regular (m: Nat) (i: IOInstruction) : Except String (Instruction m) := do
  match i with
  | IOInstruction.L n =>
    if h: ¬ m ∣ n then
      return Instruction.L ⟨n, h⟩
    throw s!"{n} divides {m}"
  | IOInstruction.R n =>
    if h: ¬ m ∣ n then
      return Instruction.R ⟨n, h⟩
    throw s!"{n} divides {m}"

def instruction_io_to_regular2 (i: IOInstruction) : Except String Instruction2 := do
  match i with
  | IOInstruction.L n =>
    if h: n ≠ 0 then
      let _ : NeZero n := ⟨h⟩
      return Instruction2.L n
    throw "L zero instruction"
  | IOInstruction.R n =>
    if h: n ≠ 0 then
      let _ : NeZero n := ⟨h⟩
      return Instruction2.R n
    throw "R zero instruction"

def sequence_except {ε α} : List (Except ε α) → Except ε (List α)
  | [] => Except.ok []
  | e :: es =>
    match e with
    | Except.error err => Except.error err
    | Except.ok x =>
      match sequence_except es with
      | Except.ok xs   => Except.ok (x :: xs)
      | Except.error err => Except.error err

-- def main : IO Unit := do
--   let path : System.FilePath := "Aoc2025/day1-input.txt"
--   let result ← loadInstructions path
--   match result with
--   | .ok arr =>
--       IO.println s!"Parsed instructions:\n{repr arr}"
--       match sequence_except (arr.toList.map (instruction_io_to_regular 100)) with
--       | .ok list =>
--         let result := run list start_state
--         IO.println s!"Result: {result.times}"
--       | .error msg =>
--         IO.eprintln s!"Error processing IO instructions list: {msg}"
--   | .error msg =>
--       IO.eprintln s!"Error parsing file: {msg}"

def main : IO Unit := do
  let path : System.FilePath := "Aoc2025/day1-input.txt"
  let result ← loadInstructions path
  match result with
  | .ok arr =>
      IO.println s!"Parsed instructions:\n{repr arr}"
      match sequence_except (arr.toList.map instruction_io_to_regular2) with
      | .ok list =>
        let result := run2 list start_state
        IO.println s!"Result: {result.times}"
      | .error msg =>
        IO.eprintln s!"Error processing IO instructions list: {msg}"
  | .error msg =>
      IO.eprintln s!"Error parsing file: {msg}"
