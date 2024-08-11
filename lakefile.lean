import Lake
open Lake DSL

package «Redis» where
  -- add package configuration options here

lean_lib «Redis» where
  -- add library configuration options here

lean_lib «Tests» where
  -- add library configuration options here

lean_exe «redis» where
  root := `Main
  supportInterpreter := true

lean_exe «test» where
  root := `Tests
  supportInterpreter := true

require soda from git "https://github.com/algebraic-sofia/soda.git"
require Specs from git "https://github.com/axiomed/Specs.lean.git"
require Socket from git "https://github.com/KislyjKisel/Socket.lean.git"
