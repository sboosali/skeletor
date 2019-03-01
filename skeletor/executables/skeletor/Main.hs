import qualified Program.Skeletor.Haskell.CLI as CLI
import           Prelude
main :: IO ()
main = do
  command <- CLI.cli
  print command

-- import qualified Program.Skeletor.Haskell.Main as Program
-- import           Prelude
-- main :: IO ()
-- main = Program.main
