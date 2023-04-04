// This file is new and hides the implementation details of the parser.

module Parser
    
    open ScrabbleUtil
    open Eval
    open StateMonad
    type word   = (char * int) list
    type square = Map<int, squareFun>
    type boardFun = coord -> Result<square option, Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
        isInfinite    : bool
    }
    
    val parseBoardProg : boardProg -> board