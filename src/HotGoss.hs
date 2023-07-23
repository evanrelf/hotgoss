module HotGoss (main) where

import qualified Data.Text.IO as Text
import qualified HotGoss.Challenge1
import qualified HotGoss.Challenge2
import qualified HotGoss.Challenge3a
import qualified UnliftIO.Environment as Environment

main :: IO ()
main = do
  Environment.getProgName >>= \case
    "hotgoss-1" ->
      HotGoss.Challenge1.main
    "hotgoss-2" ->
      HotGoss.Challenge2.main
    "hotgoss-3a" ->
      HotGoss.Challenge3a.main
    program -> do
      Text.hPutStrLn stderr $ "wtf does '" <> toText program <> "' mean"
      exitFailure
