import «Redis».IO.Client

open «Redis».IO

def main : IO Unit := do
  let client ← Client.new 127 0 0 1 6379
  let recv ← client.send "*1\r\n$4\r\nPING\r\n"

  IO.println recv
