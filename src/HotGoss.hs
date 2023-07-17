module HotGoss (main) where

import qualified Data.Text.IO as Text

main :: IO ()
main = do
  getArgs >>= \case
    _ -> do
      Text.hPutStrLn stderr "usage: hotgoss CHALLENGE [ARG...]"
      exitFailure
