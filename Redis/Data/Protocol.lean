import Soda.Grape
import Soda.Grape.Text
import Redis.Data.DataType

open Grape
open Function

def replicateM {α} (n : Nat) (parser : Grape α) : Grape (List α) :=
  List.foldrM (λ _ acc => List.cons <$> parser <*> (Grape.pure acc)) List.nil (List.replicate n ())

def Grape.Text.Char.eol := Grape.string "\r\n"

def Grape.Text.int : Grape Int := do
  let s ← Grape.takeWhile (λchr => chr == 43 || chr == 45)
  let d ← Grape.takeWhile1 (λchr => Char.isDigit $ Char.ofNat chr.toNat)
  match s.toASCIIString with
  | "+" => Grape.pure $ String.toInt! d.toASCIIString
  | "-" => Grape.pure $ -String.toInt! d.toASCIIString
  | s   =>
    if s.isEmpty
      then Grape.pure $ String.toInt! d.toASCIIString
      else Grape.fail "Should be a '+' or '-' symbol"

def simpleStringParse : Grape DataType := do
  let s ← Grape.takeWhile (λchr => chr ≠ 13)
  let _ ← Grape.Text.Char.eol
  Grape.pure $ DataType.SimpleString $ s.toASCIIString

def simpleErrorParse : Grape DataType := do
  let e ← Grape.takeWhile (λchr => chr ≠ 13)
  let _ ← Grape.Text.Char.eol
  Grape.pure $ DataType.SimpleError $ e.toASCIIString

def integerParse : Grape DataType := do
  let n ← Grape.Text.int
  let _ ← Grape.Text.Char.eol
  Grape.pure $ DataType.Integer n

def bulkStringParse : Grape DataType := do
  let n ← Grape.Text.int
  if n >= 0
    then
      let _ ← Grape.Text.Char.eol
      let e ← Grape.takeWhile (λchr => chr ≠ 13)
      let _ ← Grape.Text.Char.eol
      Grape.pure $ DataType.BulkString $ e.toASCIIString
    else
      let _ ← Grape.Text.Char.eol
      Grape.pure DataType.Null

mutual
partial def arrayParse : Grape DataType := do
  let n ← Grape.Text.int
  if n >= 0
    then
      let _  ← Grape.Text.Char.eol
      let ds ← replicateM n.toNat dataTypeParse
      Grape.pure $ DataType.Array $ List.toArray (List.reverse ds)
    else
      let _ ← Grape.Text.Char.eol
      Grape.pure DataType.Null

partial def dataTypeParse : Grape DataType := do
  let c ← Grape.takeN 1
  match c.toASCIIString with
  | "+" => simpleStringParse
  | "-" => simpleErrorParse
  | ":" => integerParse
  | "$" => bulkStringParse
  | "*" => arrayParse
  | _   => Grape.fail "Unknown data type"
end
