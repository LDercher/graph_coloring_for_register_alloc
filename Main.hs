module Main where

import Data.Function
import Data.List
import System.Environment

import LL.Language
import LL.Parser
import LL.Lexer

import Lab6
import Lab7
import Lab8


main = mapM_ doFile =<< getArgs

doFile fn = do s <- readFile fn
               let prog  = parse (alexScanTokens s)
               mapM_ doFun (functions prog)

doFun (funName, (_, _, cfg@(_, blocks))) =
    do putStrLn ("=== " ++ funName ++ " ===")
       mapM_ doBlock ("^" : map fst blocks)
       putStrLn $ unlines ("Registers:" : ["    " ++ intercalate "   " [pad 16 (tempName ++ ":") ++ ' ' :  pad 4 (show register) | (tempName, register) <- four] | four <- byFours registers])
    where preds = predecessors cfg
          uds   = useDefs cfg
          (liveIns, liveOuts) = liveness cfg preds uds
          registers = sortBy (compare `on` fst) (registerColoring (interferenceGraph cfg liveIns))

          doBlock blockName = putStr (unlines ["--- " ++ blockName ++ " ---", blockPreds, blockUseDefs, blockLiveness])
              where blockPreds = case lookup blockName preds of
                                   Nothing -> "    Something's wrong! No predecessors for " ++ blockName
                                   Just bs -> "    Predecessors: " ++ intercalate ", " bs
                    blockUseDefs = case lookup blockName uds of
                                     Nothing           -> "    Something's wrong! No uses for " ++ blockName
                                     Just (used, defs) -> "    Used:" ++ intercalate ", " used ++
                                                          "\n    Defined: " ++  intercalate ", " defs
                    blockLiveness = case (lookup blockName liveIns, lookup blockName liveOuts) of
                                      (Just vs, Just ws) -> "    Live in: " ++ intercalate ", " vs ++ "\n    Live out: " ++ intercalate ", " ws
                                      _                  -> "    Something's wrong! No liveness for " ++ blockName

          pad n s | m >= n = s
                  | otherwise = s ++ replicate (n - m) ' '
              where m = length s

          byFours [] = []
          byFours xs = take 4 xs : byFours (drop 4 xs)
