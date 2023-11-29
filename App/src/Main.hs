import HBooks.Prelude
module Main (main) where

import qualified HBooks.Cli (main)

-- a = variable that is being wrapped by the new type
-- newtype App a

main :: IO ()
main = HBooks.Cli.main
