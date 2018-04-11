module Lab8 where

import LL.Language

import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple
import Lab6

{--------------------------------------------------------------------------------

LAB 8: Register allocation

DUE: Thursday, April 12, 11:59 PM

This lab

FILES

  LL/Language.hs - the definition of LLVMlite.  You should familiarize yourself
    with the definitions in this file, and how they map to your understanding of
    LLVM from the slides and previous labs.

  LL/Parser.hs, LL/Lexer.hs - parsing support for LLVMlite.  These files are
    tool-generated, and so are not for human consumption.

  Lab6.hs - the sample solutions to Lab 6.

  Lab7.hs - last week's assignment.  You should start with your version of last
    week's assignment.  However, once Lab 7 is due, we will post sample
    solutions and you may use those if you prefer.

  Lab8.hs - this assignment.

  Main.hs - driver code.

  samples/ - a collection of LLVMlite code samples.

TESTING: The distribution includes a file "Main.hs".  You should be able to load
this file into GHCi (or compile it with GHC) and then use it to analyze the
sample LLVM file (or any others you might want to create).  Here is a sample
interaction:

    λ> :l Main.hs
    [1 of 7] Compiling LL.Language      ( LL\Language.hs, interpreted )
    [2 of 7] Compiling LL.Lexer         ( LL\Lexer.hs, interpreted )
    [3 of 7] Compiling LL.Parser        ( LL\Parser.hs, interpreted )
    [4 of 7] Compiling Lab6             ( Lab6.hs, interpreted )
    [5 of 7] Compiling Lab7             ( Lab7.hs, interpreted )
    [6 of 7] Compiling Lab8             ( Lab8.hs, interpreted )
    [7 of 7] Compiling Main             ( Main.hs, interpreted )
    Ok, modules loaded: LL.Language, LL.Lexer, LL.Parser, Lab6, Lab7, Lab8, Main.

    λ> :main "samples\\factrec.ll"
    === function ===
    --- ^ ---
        Predecessors:
        Used:arg
        Defined: slota, slotb, valb, valble0
        Live in: arg
        Live out: slota, slotb
    --- then ---
        Predecessors: ^
        Used:slota
        Defined:
        Live in: slota
        Live out: slota
    --- else ---
        Predecessors: ^
        Used:slotb, slota
        Defined: valb2, valbminus1, recursive, multiplied
        Live in: slotb, slota
        Live out: slota
    --- done ---
        Predecessors: then, else
        Used:slota
        Defined: vala2
        Live in: slota
        Live out:
    Registers:
        multiplied:      4      recursive:       3      slota:           6      slotb:           5
        vala2:           1      valb:            1      valb2:           1      valble0:         2
        valbminus1:      2


Note that the LLVM format supported by the tests code is our LLVMlite, and so
is significantly less than full LLVM.  In addition to limiting the range of
types that can be used, the parser also insists on explicit names for
everything, and does not support many of the additional options to various
commands (including the "align" option on loads, the "nsw" option on arithmetic,
and the various annotations that come with functions).

SUBMISSION INSTRUCTIONS: Submit *only* Lab8.hs.  Do not submit any of the other
distributed files, any of your test data, &c.

-------------------------------------------------------------------------------}


type Liveness = [(String, [String])]

definedIn :: Instruction -> [String]
definedIn (Bin result _ _ _ _)   = [result]
definedIn (Alloca result _)      = [result]
definedIn (Load result _ _)      = [result]
definedIn (Store _ _ _)          = []
definedIn (Icmp result _ _ _ _)  = [result]
definedIn (Call result _ _ _)    = [result]
definedIn (Bitcast result _ _ _) = [result]
definedIn (Gep result _ _ _)     = [result]

{-------------------------------------------------------------------------------

We represent graphs as ADJACENCY LISTS---that is, a mapping from each vertex to
those other vertices adjacent to it.  For example, suppose we have a
three-clique with nodes "a", "b", and "c".  We would represent this with the
following adjacency list:

  [("a", ["b", "c"]), ("b", ["a", "c"]), ("c", ["a", "b"])]

As interference graphs are undirected, each edge in the graph should be
represented twice in the adjacency list, once for each endpoint.  For example,
the edge from "b" to "c" is represented both by the "c" entry in "b"s adjacency
list and by the "b" entry in "c"s adjacency list.

-------------------------------------------------------------------------------}


type Graph t = [(t, [t])]

{--------------------------------------------------------------------------------

PROBLEM 1: Computing interference graphs

In this problem, you will compute the interference graph corresponding to a
given control-flow graph.  In addition to the Cfg, you are also provided with
the live-in temporaries at each node of the Cfg, as computed in Lab 7.  You
should return a graph with one node for each temporary in the Cfg, and edges
between them should they interfere.  For example, samples/factrec.ll contains
the following block:

    else:
      %valb2 = load i64, i64* %slotb
      %valbminus1 = sub i64 %valb2, 1
      %recursive = call i64 @function(i64 %valbminus1)
      %multiplied = mul i64 %valb2, %recursive
      store i64 %multiplied, i64* %slota
      br label %done

The live-in temporaries to this block are %slota and %slotb.  From this block
alone, you should compute the following interference graph:

    [("slota", ["slotb","valb2","valbminus1","recursive","multiplied"]),
     ("slotb",["valb2","valbminus1","recursive","multiplied",""slota"]),
     ("valb2",["valbminus1","recursive","multiplied","slotb","slota"]),
     ("valbminus1",["recursive","multiplied","slotb","slota","valb2"]),
     ("recursive",["multiplied","slotb","slota","valb2","valbminus1"]),
     ("multiplied",["slotb","slota","valb2","valbminus1","recursive"])]

(The entries in the interference graph for these vertices computed from the
whole function will include additional entries; shown here are just the entries
arising from the %else block.)  You may assume that all temporaries defined or
used in a given block interference (even if they might not).

--------------------------------------------------------------------------------}

interferenceGraph :: Cfg -> Liveness -> Graph String
interferenceGraph (first,rest) lns = map (\n -> (n, map snd (filter (\(a,b) -> a == n) crossp))) vars
                            where blocks = ("^",first) : rest
                                  ints = map (\(n, (instrs, t)) -> union (fromJust( lookup n lns)) (concatMap definedIn instrs)) blocks                                 
                                  crossp = filter (\(a,b) -> a /= b)(nub (concatMap cartProd ints))
                                  vars = nub (map fst crossp)


cartProd :: [String] -> [(String,String)]
cartProd xs = [(x,y) | x <- xs, y <- xs]

-- every var interference = livin U defs
-- get every var from livin and defs and make then adjacent to one another
-- usedefs cfg = [("blockname",([uses],[defs]))]
-- livein = [("blockname",[liveins])]
-- create tuple for every elem in [livins] ++ [defs] first elem being one elem in
-- list followed by the rest


{-------------------------------------------------------------------------------

PROBLEM 2: Coloring registers

For this problem, you will build an assignment of temporaries to registers,
given an interference graph (as computed in the previous problem).  We will
represent registers by integers starting from 1, rather than worrying about the
register names in x86.  You should follow Chaitin's algorithm, as discussed in
class.  However, you may assume an infinite set of registers, and so do not need
to worry about register spilling.  This eliminates the need for the iteration in
Chaitin's algorithm---you should able to assign registers to temporaries in one
pass.

An example is given at the top of the file.  The particular choices of registers
are not important; however, your solution should use the minimum number of
registers for a given interference graph.

-------------------------------------------------------------------------------}

registerColoring :: Graph String -> [(String, Int)]
registerColoring graph = foldr (\(node,neigh) colors -> (node, checkColor colors neigh) : colors) [] graph
    


checkColor :: [(String,Int)] -> [String] -> Int
checkColor col neigh = fst (head compCols)
                        where interSet i = (i, map fst (filter (\(n,c)-> c == i) col ))
                              matchNodes = map interSet [1..]
                              compCols = filter (\(c,n)-> null (intersect n neigh)) matchNodes

shortest :: [[a]] -> [a]
shortest [] = []
shortest [y] = y
shortest (x:y:list)
    | length x > length y = shortest (y:list)
    | otherwise = shortest (x:list)



--each var that is adjaecnt to var1 needs a different color
-- pick node with lowest degree 1st

{-------------------------------------------------------------------------------

DISCUSSION: Compare the register assignment for factrec.ll and factrec2.ll.
What does this say about the combination of statements into basic blocks in
factrec.ll?  Would you expect this to be true in general?

-------------------------------------------------------------------------------}

{--The combination of statements into basic blocks in factrec.ll was less efficient. The factrec2 implmentation needed one less register. 
    It seems the spreading of statements into more blocks allow for better register usage. Yes, it is more advantagous as far as register usage
    to spread the statements into more blocks --}
