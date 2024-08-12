import Socket

namespace Redis
namespace IO

structure Client where
  private mk ::
  socket : Socket

namespace Client

def new (o1 o2 o3 o4 : UInt8) (port : UInt16) : IO Client := do
  let socket ← Socket.mk .inet .stream

  socket.connect (Socket.SockAddr4.v4 (.mk o1 o2 o3 o4) port)

  pure { socket }

def send (client : Client) (command : String) : IO String := do
  discard <| client.socket.send command.toUTF8

  let bytes ← client.socket.recv 1024
  pure (String.fromUTF8! bytes)
