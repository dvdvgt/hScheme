module Main where

import System.Environment
import HScheme
import Control.Monad
import System.IO

main :: IO ()
main = do
    putStr "\ESC[1J"
    putStrLn banner
    forever $ do 
        inpt <- putStr ">>: " >> hFlush stdout >> getLine
        let evaled = show <$> (readExpr inpt >>= eval)
        putStrLn $ extractValue $ trapErr evaled


banner :: String
banner = "██╗  ██╗███████╗ ██████╗██╗  ██╗███████╗███╗   ███╗███████╗\n\
        \██║  ██║██╔════╝██╔════╝██║  ██║██╔════╝████╗ ████║██╔════╝\n\
        \███████║███████╗██║     ███████║█████╗  ██╔████╔██║█████╗\n\
        \██╔══██║╚════██║██║     ██╔══██║██╔══╝  ██║╚██╔╝██║██╔══╝\n\
        \██║  ██║███████║╚██████╗██║  ██║███████╗██║ ╚═╝ ██║███████╗\n\
        \╚═╝  ╚═╝╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝     ╚═╝╚══════╝\n\n\
        \Author: David Voigt\tLicense: MIT\thttps://github.com/dvdvgt/hscheme\n"
