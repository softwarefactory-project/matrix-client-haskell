-- | The matrix-bot entrypoint
module MatrixBot.Main where

import qualified Network.Matrix.Client as Matrix

main :: IO ()
main = do
  putStrLn "MatrixBot starting..."
