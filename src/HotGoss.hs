module HotGoss (main) where

import qualified Data.Text.IO as Text
import qualified HotGoss.Challenge1
import qualified UnliftIO.Environment as Environment

main :: IO ()
main = do
  getArgs >>= \case
    "1" : args ->
      Environment.withArgs args HotGoss.Challenge1.main
    _ -> do
      Text.hPutStrLn stderr "usage: hotgoss CHALLENGE [ARG...]"
      exitFailure
