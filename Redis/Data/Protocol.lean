import Soda.Grape
import Soda.Grape.Text
import Redis.Data.DataType

namespace Grape.text

def eol := Grape.string "\r\n"

def int : Grape Int := do
  let s ← Grape.takeWhile (λchr => chr == 43 || chr == 45)
  let d ← Grape.takeWhile1 (λchr => Char.isDigit $ Char.ofNat chr.toNat)
  match s.toASCIIString with
  | "+" => Grape.pure $ String.toInt! d.toASCIIString
  | "-" => Grape.pure $ -String.toInt! d.toASCIIString
  | s   =>
    if s.isEmpty
      then Grape.pure $ String.toInt! d.toASCIIString
      else Grape.fail "Should be a '+' or '-' symbol"

end Grape.text

namespace Redis
namespace Data
namespace Protocol
open Grape
open Function
open Redis.Data.DataType

def replicateM {α} (n : Nat) (parser : Grape α) : Grape (List α) :=
  List.foldrM (λ _ acc => List.cons <$> parser <*> (Grape.pure acc)) List.nil (List.replicate n ())

def simpleStringParse : Grape DataType := do
  let s ← Grape.takeWhile (λchr => chr ≠ 13)
  discard <| Grape.text.eol
  Grape.pure $ DataType.simpleString $ s.toASCIIString

def simpleErrorParse : Grape DataType := do
  let e ← Grape.takeWhile (λchr => chr ≠ 13)
  discard <| Grape.text.eol
  Grape.pure $ DataType.simpleError $ e.toASCIIString

def integerParse : Grape DataType := do
  let n ← Grape.text.int
  discard <| Grape.text.eol
  Grape.pure $ DataType.integer n

def bulkStringParse : Grape DataType := do
  let n ← Grape.text.int
  if n >= 0
    then
      discard <| Grape.text.eol
      let e ← Grape.takeWhile (λchr => chr ≠ 13)
      discard <| Grape.text.eol
      Grape.pure $ DataType.bulkString $ e.toASCIIString
    else
      discard <| Grape.text.eol
      Grape.pure DataType.null

mutual
partial def arrayParse : Grape DataType := do
  let n ← Grape.text.int
  if n >= 0
    then
      discard <| Grape.text.eol
      let ds ← replicateM n.toNat dataTypeParse
      Grape.pure $ DataType.array $ List.toArray (List.reverse ds)
    else
      discard <| Grape.text.eol
      Grape.pure DataType.null

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
