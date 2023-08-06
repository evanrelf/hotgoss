module HotGoss (main) where

import Data.Text.IO qualified as Text
import HotGoss.Challenge1 qualified
import HotGoss.Challenge2 qualified
import HotGoss.Challenge3a qualified
import HotGoss.Challenge3b qualified
import UnliftIO.Environment qualified as Environment

main :: IO ()
main = do
  Environment.getProgName >>= \case
    "hotgoss-1" ->
      HotGoss.Challenge1.main
    "hotgoss-2" ->
      HotGoss.Challenge2.main
    "hotgoss-3a" ->
      HotGoss.Challenge3a.main
    "hotgoss-3b" ->
      HotGoss.Challenge3b.main
    program -> do
      Text.hPutStrLn stderr $ "wtf does '" <> toText program <> "' mean"
      exitFailure
