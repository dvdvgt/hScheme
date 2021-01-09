module Main where

import System.Environment
import HScheme
import Control.Monad

main :: IO ()
main = do
    --putStr "\ESC[1J"
    putStr ">: "
    forever $ do 
        inpt <- getLine
        let evaled = show <$> (readExpr inpt >>= eval)
        putStrLn $ extractValue $ trapErr evaled
        putStr ">: "
