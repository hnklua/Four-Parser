import Data.List
import Data.Char
import Data.Maybe () 
import Debug.Trace

-- or it can be an operation, which has a string name.
-- the program "2 2 +" would have tokens [ I 2, I 2, Op "+" ] 
data Token = 
        Val Float 
    |   Word String
    deriving ( Eq, Show )
-- Deriving "Eq" means that we can use == and /= between tokens. E.g., I 2 == I 2
-- Deriving "Show" means that we can use "print" on tokens. 

-- An abstract syntax tree
data AstNode =
        -- a single token 
        Terminal Token 

        -- an if node. contains two branches: one for true and one for false. 
    |   If { ifTrue :: AstNode, ifFalse :: AstNode }

        -- a while node. contains only a child node for the body of the loop. 
    |   While AstNode

        -- a list of nodes. Represents a sequence of instructions like "1 1 + 2 *"
    |   Expression [ AstNode ]
    deriving ( Show )

-- This is the state of the interpreter. 
-- Currently it stores the stack, which is where all of the data lives. 
newtype ForthState = ForthState { stack :: [Float] } 
    deriving ( Show )

doAdd :: ForthState -> ForthState
doAdd state = 
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a + b ) state' 
-- we need to pop 2 values so we can add them.
-- we will pop 2 values in all the below operations. 
-- you can streamline this by defining a helper function "binary_op" if you want.
-- it can take a function with type Int -> Int -> Int and apply it to the top 
-- two values on the stack, pushing the result. 

-- apply the - operation: pop 2 values, subtract them, push the result. 1 2 - -> -1
doSub :: ForthState -> ForthState
doSub state =
    let (state',b, a ) = fsPop2 state
    in fsPush( a - b ) state' 
-- doSub state = error "your code here"

-- apply the * operation: pop 2 values, multiply them, push the result. 3 4 * -> 12
doMul :: ForthState -> ForthState
doMul state = 
    let (state',b, a) = fsPop2 state
    in fsPush( a * b ) state'
--doMul state = error "your code here"

-- apply the / operation: pop 2 values, divide them, push the result. 4 2 / -> 2
doDiv :: ForthState -> ForthState
doDiv state = 
    let (state',b, a) = fsPop2 state
    in fsPush( a / b ) state' 
-- doDiv state = error "your code here"

-- apply the swap operation. pop 2 values, re-push them in reverse order. 1 2 swap -> 2 1 
doSwap :: ForthState -> ForthState 
doSwap state = 
    let (state', b, a) = fsPop2 state
    in fsPush b (fsPush a state')
--doSwap state = error "your code here"

-- apply the drop operation. pop 1 value. 1 2 3 -> 1 2 
-- does nothing if stack is empty 
doDrop :: ForthState -> ForthState 
doDrop state = 
    let (state' , _ ) = fsPop state 
    in state'
--doDrop state = error "your code here" 

-- apply the rot operation. rotates the top three right: 1 2 3 -> 3 1 2 
-- does nothing if stack is empty or size 1
-- same as swap if stack has size 2 
doRot :: ForthState -> ForthState 
doRot state = 
    let (state', c, b, a) = fsPop3 state 
    in fsPush a (fsPush c (fsPush b state'))
-- doRot state = error "your code here"

-- duplicate the top value on the stack. 1 -> 1 1 
doDup :: ForthState -> ForthState
doDup state = 
    let (state', a) = fsPop state 
    in fsPush a (fsPush a state')
--doDup state = error "your code here"

-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> ForthState -> ForthState
-- here's how we turn the strings into their corresponding operation. 
doOp "+" = doAdd 
doOp "-" = doSub
doOp "*" = doMul
doOp "/" = doDiv 
doOp "swap" = doSwap 
doOp "drop" = doDrop 
doOp "rot" = doRot 
doOp "dup" = doDup 

-- if we go through all the definitions without finding our operation, 
-- it's not supported 
doOp op = error $ "unrecognized word: " ++ op 

-- execute an AstNode
doNode :: AstNode -> ForthState -> ForthState

-- if we execute a terminal that's an if-statement, we need to determine whether
-- the top of the stack is "true" (/= 0.0)
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
    let (state' , cond) = fsPop state 
    in if cond /= 0.0 then doNode trueBranch state' else doNode falseBranch state' 

doNode ( While loopBody ) state = 
    let (state', cond) = fsPop state
    in if cond /= 0.0 then doNode (Expression [loopBody, While loopBody]) (doNode loopBody state') else state' 

-- doing a terminal changes depending on whether it's a word or a number. 
-- if it's a number, push it...
doNode ( Terminal ( Val v ) ) state = 
  fsPush v state

-- ...if it's a word, execute the operation
doNode ( Terminal ( Word o ) ) state = doOp o state 

-- "doing" an empty expression does nothing
doNode ( Expression [] ) state = state 

-- "doing" a non-empty expression tries to execute every node in the expression
doNode ( Expression ( first:rest ) ) state = 
    let newState = doNode first state 
    in doNode (Expression rest) newState

-- arguments:
--  alreadyParsed :: [AstNode]: a list of nodes parsed so far. Starts empty.
--  tokens :: [Token]: a list of tokens remaining to be parsed
--  terminators :: [String]: a list of words that will stop parsing. 
-- How it works: 
--  * if the next token is a terminator, we're done parsing. This happens when we're in an 
--    if statement and we see a ';' or 'else' for example. 
--  * if we see the word "if", call parseIf, which reads the if branch and else branch. 
--    afterwards, we parse the remainder of the program and paste the result onto what we got 
--    when we parsed the if. 
--  * if we see the word "while" call parseWhile. This works in much the same way as parseIf
--  * if none of the above, we found a random operation or number. just append whatever we found 
--    to the alreadyParsed list and keep going. 
parseExpression' :: [AstNode] -> [Token] -> [String] -> ( [AstNode], [Token], Maybe Token )

-- if there are no more tokens, we need to check if we have terminators.
-- if we were expecting a terminator and there isn't one, that's an error. 
parseExpression' tokens [] terminators = 
    -- this is the base case: nothing to parse
    if null terminators then (tokens, [], Nothing)
    else error ( "ended expression without finding one of: " ++ intercalate ", " terminators )

-- if tokens remain, keep parsing
parseExpression' alreadyParsed ( token:tokens ) terminators 
    -- found a terminator: stop parsing and return. 
    | token `elem` map Word terminators = ( alreadyParsed, tokens, Just token )

    -- found an if-statement: remove the "if" token, parse the true and false branches, and 
    -- then parse whatever is after the if-statement.
    | token == Word "if" =
        let (ifTrue, ifFalse, remainingTokens) = parseIf tokens
        in parseExpression' (alreadyParsed ++ [If ifTrue ifFalse]) remainingTokens terminators

    -- found a while-statement: remove the "while", parse the body, then parse whatever is after
    | token == Word "while" =
        let (loopBody, afterLoop, _) = parseExpression' [] tokens [";"]
            whileNode = While (Expression loopBody)
        in parseExpression' (alreadyParsed ++ [whileNode]) afterLoop terminators
        
    -- no special word found. We are parsing a list of operations. Keep doing this until 
    -- there aren't any. 
    | otherwise = parseExpression' (alreadyParsed ++ [Terminal token]) tokens terminators

-- takes the result of parseExpression' and wraps it in an Expression constructor
parseExpression :: [Token] -> AstNode
parseExpression tokens = 
    let (parsedNodes, remainingTokens, _) = parseExpression' [] tokens []  
    in Expression parsedNodes

-- we just saw an "if". now we have to build an "If" AstNode.
-- returns the two branches and the remaining to kens. 
-- ( ifTrue, ifFalse, remainingTokens ). 
parseIf :: [Token] -> ( AstNode, AstNode, [Token] ) 
parseIf tokens =
    let (trueBranch, tokensAfterIf, _) = parseExpression' [] tokens ["else", ";"]
        (Expression falseBranch, remaingTokens) = parseElse tokensAfterIf
    in (If trueBranch, falseBranch, remaingTokens)


-- we just saw an "else". now finish the ifFalse part of the If node. This one only needs to 
-- return the "false" branch of the if statement, which is why there is only one [AstNode] in 
-- the return value. 
parseElse :: [Token] -> ( AstNode, [Token] )
parseElse tokens = 
    let (falseBranch, remaingTokens, _) = parseExpression' [] tokens [";"]
    in (Expression falseBranch, remaingTokens)
-- parsing a while loop is similar to parsing an if statement. 
parseWhile :: [Token] -> ( AstNode, [Token] )
-- if we reach the end of our tokens without closing the loop, that's an error 
parseWhile [] = error "while without closing semicolon."
-- otherwise, parse the loop body until reaching the ";"
parseWhile tokens = 
    let (loopBody, tokensAfterWhile, _) = parseExpression' [] tokens [";"]
    in (While (Expression loopBody), tokensAfterWhile)
    
-- create a new interpreter
fsNew :: ForthState
fsNew = ForthState { stack = [] }

-- push a new value onto the stack
fsPush :: Float -> ForthState -> ForthState
fsPush i state = ForthState { stack = i : stack state }

-- remove a value from the stack, or print an error if nothing is there.
-- returns the value removed and the new state 
fsPop :: ForthState -> ( ForthState, Float )
fsPop state = 
    let top = head $ stack state 
        new_stack = tail $ stack state  
    in  
        ( ForthState { stack = new_stack }, top )

-- remove two values from the stack. return the new stack and the two items.
fsPop2 :: ForthState -> ( ForthState, Float, Float )
fsPop2 state = 
    case stack state of 
        (b:a:tail) -> (state {stack=tail}, a, b)

-- remove three values from the stack. return the new stack and the three items. 
fsPop3 :: ForthState -> ( ForthState, Float, Float, Float )
fsPop3 state = 
    case stack state of 
        (c:b:a:tail) -> (state {stack=tail}, a, b, c)

-- return the value on top of the stack 
fsTop :: ForthState -> Float 
fsTop state = head $ stack state 

-- Takes a single word and turns it into a token. So "2" becomes "I 2" and 
-- "+" becomes "Op +"
lexToken :: String -> Token
lexToken t = 
    let firstChar = ord . head in 
    if firstChar t >= ord '0' && firstChar t <= ord '9' then 
        Val $ read t 
    else 
        Word t 

-- Takes a whole program and turns it into a list of tokens. Calls "lexToken"
tokenize :: String -> [Token]
tokenize code = map lexToken $ words code 

-- removes comments from a token stream. comments are between /' and '/
-- arguments:
--  * the first bool tells us whether we are in a comment or not. starts false.
--  * the first token list is the tokens that are not inside of comments. starts empty.
--  * the last list are the remaining tokens 
removeComments :: Bool -> [Token] -> [Token] -> [Token]
removeComments _ nonComments [] = reverse nonComments  
removeComments isFirstToken nonComments (Word token : tokens)
    | isFirstToken && token == "/'" = removeComments False nonComments tokens
    | token == "'/" = removeComments True nonComments tokens
    | token == "/'" = removeComments False nonComments tokens
    | otherwise = removeComments isFirstToken (Word token : nonComments) tokens
removeComments isFirstToken nonComments (_ : tokens) = removeComments isFirstToken nonComments tokens


main :: IO ()
main = do
    -- get all the code passed to STDIN as a giant string 
    code <- getContents

    -- convert it into a list of tokens
    let tokens = removeComments False [] ( tokenize code ) 

    -- parse the ast 
    let ast = parseExpression tokens

    -- if tokens are left after we are done parsing, there's a problem
    print ast 

    putStrLn ""

    print $ reverse $ stack $ doNode ast fsNew 
