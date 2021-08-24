-- vim: filetype=haskell
{
{-# OPTIONS_GHC -dshow-passes #-}
module Parse (parse) where

import Control.Monad.State.Strict
}

%monad { ParseState }
%lexer { positionKeep } { (-1) }
%name parseMain
%tokentype { Token }
%error { parseErrorTok }

%expect 0

%token

T0 { 0 }
T1 { 1 }
T2 { 2 }
T3 { 3 }
T4 { 4 }
T5 { 5 }
T6 { 6 }
T7 { 7 }
T8 { 8 }
T9 { 9 }

%%

R :: { [Node] }
    :         { [] }
    | gen(R0) { $1 }
    | gen(R1) { $1 }
    | gen(R2) { $1 }
    | gen(R3) { $1 }
    | gen(R4) { $1 }
    | gen(R5) { $1 }
    | gen(R6) { $1 }
    | gen(R7) { $1 }
    | gen(R8) { $1 }
    | gen(R9) { $1 }

gen(base) :: { [Node] }
    : base R0 R0 R0 R0 R0 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R0 R0 R0 R0 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R0 R0 R0 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R0 R0 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R0 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R5 R0 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R5 R6 R0 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R5 R6 R7 R0 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R5 R6 R7 R8 R0 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }
    | base R1 R2 R3 R4 R5 R6 R7 R8 R9 { [$1, $2, $3, $4, $5, $6, $7, $8, $9, $10] }

R0 :: { Node } : T0 { show $1 }
R1 :: { Node } : T1 { show $1 }
R2 :: { Node } : T2 { show $1 }
R3 :: { Node } : T3 { show $1 }
R4 :: { Node } : T4 { show $1 }
R5 :: { Node } : T5 { show $1 }
R6 :: { Node } : T6 { show $1 }
R7 :: { Node } : T7 { show $1 }
R8 :: { Node } : T8 { show $1 }
R9 :: { Node } : T9 { show $1 }

unused00 : { () }
unused01 : { () }
unused02 : { () }
unused03 : { () }
unused04 : { () }
unused05 : { () }
unused06 : { () }
unused07 : { () }
unused08 : { () }
unused09 : { () }
unused10 : { () }
unused11 : { () }
unused12 : { () }
unused13 : { () }
unused14 : { () }
unused15 : { () }
unused16 : { () }
unused17 : { () }
unused18 : { () }
unused19 : { () }

{

type Token = Int
type Node = String

type ParseState = StateT [Token] IO

parse :: [Token] -> IO [Node]
parse = evalStateT parseMain

positionKeep :: (Token -> ParseState a) -> ParseState a
positionKeep cont = do
    tokens <- get
    case tokens of
        [] -> cont (-1)
        tok : toks -> put toks >> cont tok

parseErrorTok :: Token -> ParseState a
parseErrorTok = error . ("unexpected token " ++) . show

}
