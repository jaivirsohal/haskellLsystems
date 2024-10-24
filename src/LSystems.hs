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

parse :: Rules Command -> [Char] -> [Command]
parse = undefined

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 = undefined

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 = undefined

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
