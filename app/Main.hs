{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Data.Monoid    ((<>))
import           Lib
import           Lubeck.App     (runAppReactive)


main :: IO ()
main = do
  (lat, v1) <- pipeline1
  (lon, v2) <- pipeline2

  (v3, latlon) <- coordComponent $ (,) <$> lat <*> lon

  mv <- mapComponent latlon

  runAppReactive $ vlayout <$> v1 <*> v2 <*> v3 <*> mv

  where
    pipeline1 = do
      (v1, outp1) <- counterComponent 0
      (v2, outp2) <- opComponent outp1
      (v3, outp3) <- opComponent outp2
      v4          <- resComponent outp3

      pure (outp3, hlayout <$> v1 <*> v2 <*> v3 <*> v4)

    pipeline2 = do
      (v1, outp1) <- counterComponent 0
      (v2, outp2) <- opComponent outp1
      (v3, outp3) <- opComponent outp2
      v4          <- resComponent outp3

      pure (outp3, hlayout <$> v1 <*> v2 <*> v3 <*> v4)

    
