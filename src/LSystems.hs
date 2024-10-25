module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour

type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command] deriving Show
type ColouredLine = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem a _ _) = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ a _) = a

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ a) = a 

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar [] _ = []
lookupChar (r:rs) c
    | a == c = b
    | otherwise = lookupChar rs c
    where (a, b) = r

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne _ [] = ""
expandOne rs (c:cs) = lookupChar rs c ++ expandOne rs cs

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand cs 0 rs = cs
expand cs n rs = expand (expandOne rs cs) (n-1) rs

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move cmd theta state = case cmd of
   F -> ((x + cos angleRad, y + sin angleRad), angle)
   L -> (coord,  angle + theta)
   R -> (coord,  angle - theta)
   where (coord, angle) = state
         (x, y) = coord
         angleRad = degreesToRadians angle

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

-- Parse function
parse :: Rules Command -> [Char] -> [Command]
parse rs "" = []
parse rs (']':cs) = []
parse rs (c:cs)
   | c == '[' = let (inside, rest) = splitBrackets cs
                in  B (parse rs inside) : parse rs rest
   | otherwise = lookupChar rs c ++ parse rs cs 

-- Helper function to get inside and outside of brackets
splitBrackets :: [Char] -> ([Char], [Char])
splitBrackets = go 0 []
  where
    go _ acc [] = (reverse acc, [])
    go 0 acc (']':cs) = (reverse acc, cs)
    go nestLevel acc (']':cs) = go (nestLevel-1) (']':acc) cs
    go nestLevel acc ('[':cs) = go (nestLevel+1) ('[':acc) cs
    go nestLevel acc (c:cs) = go nestLevel (c:acc) cs

-- Starting point for drawing
initialState :: TurtleState
initialState = ((0, 0), 90)

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 cmds angle col = t1 cmds initialState
  where
    t1 :: [Command] -> TurtleState -> [ColouredLine]
    t1 [] _ = []
    t1 (B cmds : cmds') state = t1 cmds state ++ t1 cmds' state
    t1 (cmd : cmds) state@((x, y), theta)
      | F <- cmd = let ((newX, newY), newTheta) = updateState
                       newLine = ((x, y), (newX, newY), col)
                    in newLine : t1 cmds ((newX, newY), newTheta)
      | otherwise = t1 cmds updateState
      where updateState = move cmd angle state

-- Define a stack to track command list and state
type Stack = [([Command], TurtleState)]

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 cmds angle col = t2 cmds initialState []
  where
    t2 :: [Command] -> TurtleState -> Stack -> [ColouredLine]
    t2 [] _ [] = []
    t2 [] _ ((cmds', state) : stack) = t2 cmds' state stack
    t2 (B cmds : cmds') state stack = t2 cmds state ((cmds', state) : stack)
    t2 (cmd : cmds) state@((x, y), theta) stack
      | F <- cmd = let ((newX, newY), newTheta) = updateState
                       newLine = ((x, y), (newX, newY), col)
                    in newLine : t2 cmds ((newX, newY), newTheta) stack
      | otherwise = t2 cmds updateState stack
      where updateState = move cmd angle state

-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
