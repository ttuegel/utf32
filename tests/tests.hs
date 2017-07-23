module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.UTF32

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence
    [ Test.UTF32.tests
    ]

  unless (and results) exitFailure
