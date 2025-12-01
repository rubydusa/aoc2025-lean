import Aoc2025.Day1IO

def main : IO Unit := do
  let path : System.FilePath := "Aoc2025/day1-input.txt"
  let result ← loadInstructions path
  match result with
  | .ok arr =>
      IO.println s!"Parsed instructions:\n{repr arr}"
  | .error msg =>
      IO.eprintln s!"Error parsing file: {msg}"
