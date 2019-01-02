{-# LANGUAGE OverloadedStrings #-}

-- | Simple test suite.

module Main where

import qualified Data.HashMap.Strict as HM
import           Data.Ini
import           Test.Hspec

main :: IO ()
main =
  hspec
    (do describe
          "Regular files"
          (do it
                "Multi-section file with comments"
                (shouldBe
                   (parseIni
                      "# Some comment.\n\
                      \[SERVER]\n\
                      \port=6667\n\
                      \hostname=localhost\n\
                      \[AUTH]\n\
                      \user=hello\n\
                      \pass=world\n\
                      \# Salt can be an empty string.\n\
                      \salt=")
                   (Right
                      (Ini
                         { iniSections =
                             HM.fromList
                               [ ( "AUTH"
                                 , HM.fromList
                                     [ ("user", "hello")
                                     , ("salt", "")
                                     , ("pass", "world")
                                     ])
                               , ( "SERVER"
                                 , HM.fromList
                                     [ ("hostname", "localhost")
                                     , ("port", "6667")
                                     ])
                               ]
                         })))))
