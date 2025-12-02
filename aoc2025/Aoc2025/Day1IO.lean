-- LLM slop parser because couldn't bother to write a parser on Day 1 of using Lean

import Aoc2025.Day1
import Std
open Std

inductive IOInstruction where
  | L (n : Nat)
  | R (n : Nat)
  deriving Repr, BEq

def parseLine (s : String) : Except String IOInstruction := do
  let s := s.trim
  if s.isEmpty then
    throw "empty line"

  -- Get first character, but keep everything in the Except monad
  let c ←
    match s.data with
    | []       => throw "unexpected empty string after trim"
    | c :: _   => pure c

  let rest := (s.drop 1).trim

  -- Parse the number, again staying in the monad
  let n ←
    match rest.toNat? with
    | none     => throw s!"could not parse number in: {rest}"
    | some n   => pure n

  -- Finally, decide L/R
  match c with
  | 'L' => return IOInstruction.L n
  | 'R' => return IOInstruction.R n
  | _   => throw s!"unknown instruction kind: '{c}'"

def parseFileContents (contents : String) :
    Except String (Array IOInstruction) :=
  let lines := contents.splitOn "\n"
  lines.foldl
    (fun acc line =>
      match acc with
      | Except.error e => Except.error e   -- propagate first error
      | Except.ok arr =>
          let line := line.trim
          if line.isEmpty then
            Except.ok arr                  -- skip blank lines
          else
            match parseLine line with
            | Except.error e   => Except.error e
            | Except.ok instr  => Except.ok (arr.push instr)
    )
    (Except.ok #[])

def loadInstructions (path : System.FilePath) :
    IO (Except String (Array IOInstruction)) := do
  let contents ← IO.FS.readFile path
  pure (parseFileContents contents)

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
