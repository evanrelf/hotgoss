module HotGoss (main) where

import qualified Data.Text.IO as Text
import qualified HotGoss.Challenge1
import qualified UnliftIO.Environment as Environment

main :: IO ()
main = do
  Environment.getProgName >>= \case
    "hotgoss-1" ->
      HotGoss.Challenge1.main
    program -> do
      Text.hPutStrLn stderr $ "wtf does '" <> toText program <> "' mean"
      exitFailure
