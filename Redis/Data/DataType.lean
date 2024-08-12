namespace Redis
namespace Data

/--
Data type definition for Redis.
-/
inductive DataType where
  /--
  Simple strings are used to transmit non-binary
  safe strings.
  -/
  | simpleString : String → DataType

  /--
  Simple errors are similar to simple strings,
  but they are used to transmit errors as exceptions.

  Example: `-ERR unknown command 'hamood'`
  -/
  | simpleError : String → DataType

  /--
  Represents an integer in Redis.

  Example: `:100\r\n`
  -/
  | integer : Int → DataType

  /--
  Bulk strings are used to transmit binary-safe strings
  with a maximum size of 512 MB.

  Example: `$5\r\nhello\r\n`

  Note: You need to specify the number of bytes of
  the string you want to represent.
  -/
  | bulkString : String → DataType

  /--
  Arrays can hold multiple `DataType` values.

  Examples: `*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n`
            `*0\r\n`

  Note: As with bulk strings, you need to specify
  the length of your array.
  -/
  | array : Array DataType → DataType

  /--
  Represents null bulk strings and null arrays in Redis.
  -/
  | null : DataType
  deriving Inhabited, Repr, BEq
