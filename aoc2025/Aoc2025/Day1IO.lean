-- LLM slop parser because couldn't bother to write a parser on Day 1 of using Lean

import Std
open Std

inductive Instruction where
  | L (n : Nat)
  | R (n : Nat)
  deriving Repr, BEq

def parseLine (s : String) : Except String Instruction := do
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
  | 'L' => return Instruction.L n
  | 'R' => return Instruction.R n
  | _   => throw s!"unknown instruction kind: '{c}'"

def parseFileContents (contents : String) :
    Except String (Array Instruction) :=
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
    IO (Except String (Array Instruction)) := do
  let contents ← IO.FS.readFile path
  pure (parseFileContents contents)
