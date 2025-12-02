import Aoc2025.Day1
import Aoc2025.Day1IO
import Aoc2025.Day2

def main : IO Unit := do
  let path : System.FilePath := "inputs/day2-input-small.txt"
  let result ← load_id_ranges path
  match result with
  | .ok list =>
      IO.println s!"Parsed id ranges:\n{repr list}"
      let result := invalid_ids_in_ranges list
      IO.println s!"Result: {result}"
  | .error msg =>
      IO.eprintln s!"Error parsing file: {msg}"

-- DAY 1
--

-- First Half
-- ----------
-- def main : IO Unit := do
--   let path : System.FilePath := "inputs/day1-input.txt"
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

-- Second Half
-- ----------
-- def main : IO Unit := do
--   let path : System.FilePath := "inputs/day1-input.txt"
--   let result ← loadInstructions path
--   match result with
--   | .ok arr =>
--       IO.println s!"Parsed instructions:\n{repr arr}"
--       match sequence_except (arr.toList.map instruction_io_to_regular2) with
--       | .ok list =>
--         let result := run2 list start_state
--         IO.println s!"Result: {result.times}"
--       | .error msg =>
--         IO.eprintln s!"Error processing IO instructions list: {msg}"
--   | .error msg =>
--       IO.eprintln s!"Error parsing file: {msg}"
