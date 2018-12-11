--Name: Catalin Mares
--Reg. Num. : 160151912
--

type Input  = Int
type Output = Int


-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)



-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int}
  | INC {box :: Int}
  | JEQ {box1   :: Int,
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)

type Program = [Instruction]



-- PROBLEM 1. 
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?
{-
Explanation:
-- Function listMaxBoxNum creates a list of all the numbers of the boxes used within the program
-- maxBoxNum function will then return the maximum number from that list

Test:
-maxBoxNum [CLR 4, INC 2, JEQ 3 4 2, INC 8]  -> 8
-maxBoxNum [CLR 16, INC 2, JEQ 3 4 2, INC 8] -> 16
-}
maxBoxNum :: Program -> Int
maxBoxNum x = maximum (listMaxBoxNum x)


listMaxBoxNum :: Program -> [Int]
listMaxBoxNum [] = [0]
listMaxBoxNum (h:t)
  | (h == CLR (box h)) || (h == INC (box h)) = box h:(listMaxBoxNum t)
  | otherwise = [(box1 h),(box2 h)]++listMaxBoxNum t


-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)


-- PROBLEM 2. 
-- --------------------------
{-
Explanation:
-- This overrides the default show and provides my own way of showing the BATConfig

Test:
-show(BATConfig [0,2,5,8,6,1] 3)  -> "boxes = [0,2,5,8,6,1]; counter = 3"
-}
instance Show BATConfig where
  show (BATConfig x y) = "boxes = " ++ show(x) ++ "; counter = " ++ show(y)


-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
    {-
    Explanation:
    -- Concatinate a 0 in front of the input to represent box 0 which is used for computation
    -- Fill in all boxes up the the maximum box number with 0's
    -- Set the counter to 0

    Test:
    -initialise [CLR 3,INC 5, JEQ 2 3 3] [3,2,4] :: BATConfig  -> boxes = [0,3,2,4,0,0]; counter = 0
    -}
    initialise x y = BATConfig ((0:y)++(replicate ((maxBoxNum x)-(length y)) 0)) 0
    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
    --
    {-
    Explanation:
    -- If the length of the list containing the instructions is the same as the counter return true
    -- This will mean that the counter is pointing to an instruction that does not exist
    -- Which shows that the program has halted so it has finished running

    Test:
    -acceptState [CLR 3,INC 5,JEQ 2 3 3] (BATConfig [0,3,2,4,0,0] 0)  -> False
    -acceptState [CLR 3,INC 5,JEQ 2 3 3] (BATConfig [0,3,2,4,0,0] 2)  -> False
    -acceptState [CLR 3,INC 5,JEQ 2 3 3] (BATConfig [0,3,2,4,0,0] 3)  -> True
    -}
    acceptState x y@(BATConfig a b)
      | b == (length x) = True  --use the counter
      | otherwise = False
    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
    {-
    Explanation:
    -- This will update the configuration as a result of the instruction being executed at the current state
    -- newBoxValues takes the list of numers in each box, the list of instructions and an int
    -- newBoxValues will then change only the number of the box mentioned in the Instruction given
    -- If the instruction is CLR x then 0 will be overwritten in box x
    -- If the instruction is INC x then x will be incremented by 1
    -- If the insturction is JEQ x y z then if box x = box y you move to instruction z
    -- Otherwise return the same configuration and just increment the counter by 1

    Test: This is the add B1 and B2 program
    -doNextMove [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [0,3,2,4,0,0] 0)  -> boxes = [0,3,2,4,0,0]; counter = 1
    -doNextMove [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [0,3,2,4,0,0] 1)  -> boxes = [1,3,2,4,0,0]; counter = 2
    -doNextMove [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [1,3,2,4,0,0] 2)  -> boxes = [1,4,2,4,0,0]; counter = 3
    -doNextMove [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [1,4,2,4,0,0] 3)  -> boxes = [1,4,2,4,0,0]; counter = 4
    -doNextMove [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [1,4,2,4,0,0] 4)  -> boxes = [1,4,2,4,0,0]; counter = 1
    -}
    doNextMove prg (BATConfig y z)
      | (prg!!z == CLR (box (prg!!z))) = BATConfig (newBoxValues y (box (prg!!z)) 0) (z+1)
      | (prg!!z == INC (box (prg!!z))) = BATConfig (newBoxValues y (box (prg!!z)) ((y!!(box (prg!!z)))+1)) (z+1)
      | otherwise = if ((y!!(box1 (prg!!z)))==(y!!(box2 (prg!!z)))) then BATConfig y (target (prg!!z)) else BATConfig y (z+1)
         where newBoxValues inp prg n = take prg inp ++ [n] ++ drop (prg + 1) inp
    -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg
    {-
    Explanation:
    -- If the program is empty this function will initialise a default configuration
    -- If the current configuration is an accept state then the same configuration will be returned
    -- If the configuration is not an accept state the next configuration will be ran to attept and get an accept state

    Test:
    --This is not an accept state as we have not added box 1 and 2
    -runFrom [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1] (BATConfig [0,4,2,4,0,0] 0)  -> boxes = [2,6,2,4,0,0]; counter = 5
    --This is what happens when you give an empty program
    --runFrom [] (BATConfig [2,6,7] 0)  ->  boxes = [0,0]; counter = 0
    -}
    runFrom x y
      | null x = initialise x [0]
      | acceptState x y = y
      | otherwise = runFrom x (doNextMove x y)
    -- PROBLEM 7: getOutput    :: cfg -> Output
    {-
    Explanation:
    -- Printing out the contents of box 1

    Test:
    --getOutput (BATConfig [0,2,6,7] 4)  ->  2
    --getOutput (BATConfig [0,12,6,7] 4)  ->  12
    -}
    getOutput (BATConfig x y) = (x!!1)


-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs
execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)


-- PROBLEM 8. 
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.
{-
Explanation:
-- Changing the target of every JEQ command by incrementing it by a given integer value

Test:
--transpose 5 [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1]  ->  [JEQ {box1 = 0, box2 = 2, target = 10},INC {box = 0},INC {box = 1},JEQ {box1 = 0, box2 = 2, target = 10},JEQ {box1 = 2, box2 = 2, target = 6}]
--transpose 1 [JEQ 0 2 5, INC 0, INC 1, JEQ 0 2 5, JEQ 2 2 1]  ->  [JEQ {box1 = 0, box2 = 2, target = 6},INC {box = 0},INC {box = 1},JEQ {box1 = 0, box2 = 2, target = 6},JEQ {box1 = 2, box2 = 2, target = 2}]
-}
transpose :: Int -> Program -> Program
transpose x [] = []
transpose x (h:t)
  | (h == JEQ (box1 h) (box2 h) (target h)) = (JEQ (box1 h) (box2 h) ((target h)+x)):(transpose x t)
  | otherwise = h:(transpose x t)



-- PROBLEM 9. 
-- ---------------------------
-- join two programs together, so as to run one
-- after the other
{-
Explanation:
-- Joining two programs together, changing every JEQ command of the second program using the transpose function which was done above
-- Every JEQ comand will be incremented by x, which represents the length of the list of instructions in the first program

Test:
--Input = (*->*) [JEQ 0 2 3, CLR 3, INC 4, JEQ 2 1 4] [JEQ 2 3 4, CLR 2, JEQ 4 5 6]
--Output = [JEQ {box1 = 0, box2 = 2, target = 3},CLR {box = 3},INC {box = 4},JEQ {box1 = 2, box2 = 1, target = 4},JEQ {box1 = 2, box2 = 3, target = 8},CLR {box = 2},JEQ {box1 = 4, box2 = 5, target = 10}]
-}
(*->*) :: Program -> Program -> Program
p1 *->* p2 = p1++(transpose (length p1) p2)


-- PROBLEM 10. 
-- ---------------------------
-- program to compute B1 = B1 + B2
{-
Explanation:
-- If box 2 is 0 then we do not need to add anything to box 1 so we finish the program
-- Otherwise we increment box 0 and box 1 by 1 until box 0 and box 2 are equal
-- This means that we would have added the number from box 2 to box 1
-- JEQ 2 2 1 is used because the value of box 2 will not change so we can use it to jump back to any instruction we like

Test:
--execute (adder) [5,2]  ->  7
--execute (adder) [8,20,6,8]  ->  28
-}
adder :: Program
adder = [CLR 0,JEQ 0 2 6, INC 0, INC 1, JEQ 0 2 6, JEQ 2 2 2]


-- PROBLEM 11. 
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)
{-
Explanation:
-- If the boxes m and n are equal we jump so that we finish the program
-- Otherwise we clear box n and check again if m and n are the same (0) so that we can finish the program
-- If this is not the case we increment n until box n = box m

Test:
--execute (copyBox 2 1) [8,20,6,8]  ->  20
--execute (copyBox 3 1) [8,20,6,8]  ->  6
-}
copyBox :: Int -> Int -> Program
copyBox m n = [JEQ m n 6,CLR n,JEQ m n 6, INC n, JEQ m n 6, JEQ m m 3]


-- PROBLEM 12. 
-- ---------------------------
-- program to compute B1 = Bx + By
{-
Explanation:
-- If you try to add box 1 to box 1 then you overwrite box 2 to copy the contents of box1 in it
-- After that you simply use the adder to add box 1 to box 2
-- This means that if you try to chain more programs together and the second program uses box 2 it will not work as you have overwritten it
-- Otherwise you will simply add the two boxes together and copy the result into box 1 to display the result of the calculation

Test:
--execute (addXY 1 1) [8,20,6,8]  ->  16
--execute (addXY 1 2) [8,20,6,8]  ->  28
--execute (addXY 2 3) [8,20,6,8]  ->  26
-}
addXY :: Int -> Int -> Program
addXY x y
  | (x==1) && (y==1) = (copyBox 1 2) *->* (adder)
  | x == 1 = [JEQ 0 y 5, INC 0, INC x, JEQ 0 y 5, JEQ y y 1]
  | otherwise = [JEQ 0 y 5, INC 0, INC x, JEQ 0 y 5, JEQ y y 1,CLR 1,JEQ x 1 10,INC 1, JEQ x 1 10,JEQ x x 7]
