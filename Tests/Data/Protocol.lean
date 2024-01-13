import Specs
import Specs.Macro

import «Redis».Data.DataType
import «Redis».Data.Protocol

open Specs
open Specs.Matchers
open Specs.Macro

instance : Repr ByteSlice where
  reprPrec b _ := b.toASCIIString

instance [BEq α] : BEq (Result α) where
  beq
    | Result.done i is, Result.done j js => i == j && is.toASCIIString == js.toASCIIString
    | Result.error i is, Result.error j js => i == j && is == js
    | _, _ => false

def runParser (s : String) := (Grape.run dataTypeParse (s.toSlice))

def protocolTests := do
  it "should parse simple string" do
    isEqual
      (runParser "+OK\r\n")
      (Result.done (DataType.SimpleString "OK") default)

  it "should parse simple error" do
    isEqual
      (runParser "-ERR unknown command 'foobar'\r\n")
      (Result.done (DataType.SimpleError "ERR unknown command 'foobar'") default)

  it "should parse integer" do
    isEqual
      (runParser ":0\r\n")
      (Result.done (DataType.Integer 0) default)

  it "should parse integer with the plus signer" do
    isEqual
      (runParser ":+1000\r\n")
      (Result.done (DataType.Integer 1000) default)

  it "should parse integer with the minus signer" do
    isEqual
      (runParser ":-1000\r\n")
      (Result.done (DataType.Integer $ -1000) default)

  it "should parse bulk string" do
    isEqual
      (runParser "$5\r\nhello\r\n")
      (Result.done (DataType.BulkString "hello") default)

  it "should parse empty bulk string" do
    isEqual
      (runParser "$0\r\n\r\n")
      (Result.done (DataType.BulkString "") default)

  it "should parse array" do
    isEqual
      (runParser "*3\r\n:1\r\n:2\r\n:3\r\n")
      (Result.done
        (DataType.Array #[
          DataType.Integer 1,
          DataType.Integer 2,
          DataType.Integer 3])
        default)

  it "should parse array" do
    isEqual
      (runParser "*3\r\n$3\r\nfoo\r\n:1\r\n:2\r\n")
      (Result.done
        (DataType.Array #[
          DataType.BulkString "foo",
          DataType.Integer 1,
          DataType.Integer 2])
        default)

  it "should parse empty array" do
    isEqual
      (runParser "*0\r\n")
      (Result.done (DataType.Array #[]) default)

  it "should parse array with different data types" do
    isEqual
      (runParser "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Foo\r\n-Bar\r\n")
      (Result.done
        (DataType.Array #[
          DataType.Array #[
            DataType.Integer 1,
            DataType.Integer 2,
            DataType.Integer 3],
          DataType.Array #[
            DataType.SimpleString "Foo",
            DataType.SimpleError "Bar"]])
        default)

  it "should parse array with null data type" do
    isEqual
      (runParser "*3\r\n$3\r\nfoo\r\n$-1\r\n$3\r\nbar\r\n")
      (Result.done (DataType.Array #[
        DataType.BulkString "foo",
        DataType.Null,
        DataType.BulkString "bar"])
      default)
