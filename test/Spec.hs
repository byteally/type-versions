{-# LANGUAGE DataKinds #-}
import Type.Versions

import Control.Exception.Base

main :: IO ()
main = do
  assert (compareVersion (SemVar :: SemVar '(1,1,1)) (SemVar :: SemVar '(1,1,1))
         == EQ) (pure ())
  assert (compareVersion (SemVar :: SemVar '(1,1,2)) (SemVar :: SemVar '(1,1,1))
         == GT) (pure ())
  assert (compareVersion (SemVar :: SemVar '(1,1,2)) (SemVar :: SemVar '(1,2,1))
          == LT) (pure ())
  assert (compareVersion (SemVar :: SemVar '(2,1,2)) (SemVar :: SemVar '(1,2,1))
           == GT) (pure ())
