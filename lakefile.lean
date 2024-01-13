import Lake
open Lake DSL

package «Redis» where
  -- add package configuration options here

lean_lib «Redis» where
  -- add library configuration options here

@[default_target]
lean_exe «redis» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true

require soda from git "https://github.com/algebraic-sofia/soda.git"
