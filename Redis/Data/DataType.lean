inductive DataType where
  -- Simple strings are used to transmit non binary
  -- safe string.

  -- Example: `+OK\r\n`
  | SimpleString : String → DataType

  -- Simple error is similar to the simple string,
  -- but it's used to transmit error as exceptions.

  -- Example: `-ERR unknown command 'hamood'`
  | SimpleError : String → DataType

  -- Example: `:100\r\n`
  | Integer : Int → DataType

  -- Bulk strings are used to transmit binary safe string
  -- with max size of 512 MB.

  -- Example: `$5\r\nhello\r\n`

  -- Note that you need to specify the number of bytes of
  -- string that you want to represent.
  | BulkString : String → DataType

  -- Examples: `*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n`
  --           `*0\r\n`

  -- Note that in the same way you need to specify
  -- the number of bytes of a bulk string, you need to
  -- specify the length of your array.
  | Array : (Array DataType) → DataType

  -- This is utils to represent null bulk strings
  -- and null arrays.
  | Null
  deriving Inhabited, Repr
