import Socket

namespace Redis
namespace IO
open Socket

structure Client where
  private mk ::
  socket : Socket

namespace Client

def new (addr : String) (port : String) : IO Client := do
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  let remoteAddr ← SockAddr.mk addr port AddressFamily.inet SockType.stream

  socket.connect remoteAddr

  pure { socket }

def send (client : Client) (command : String) : IO String := do
  let _ ← client.socket.send command.toUTF8

  let bytesRecv ← client.socket.recv 1024
  match bytesRecv with
  | Option.some bytes => pure $ String.fromUTF8Unchecked bytes
  | Option.none       => pure "(nil)"
