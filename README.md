# BAT-Computer

## Project Description
In this programming assignment I have implemented a model of computation called the BoxAnd-Token computer (BAT-Computer).

For the purposes of this assignment, a computer is something that moves from one configuration to another as a result of following instructions in a program. It can be initialised by loading one or more inputs, and typically provides a single output if/whenthe program stops running. For this purpose all input and output values are of type Int .

A BAT-computer is a device comprising a finite list of boxes, (Box0, Box1, . . .), each of which can contain finitely many tokens. The current configuration of a BAT-computer is given by saying how many tokens are in each box, and which instruction should be executed next. There are only three types of instruction. Two of these adjust the number of tokens in a box. The notation for each of these instructions, and its intended meaning, is as follows:
• CLR x (“clear x”)
Remove all of the tokens from Box x;

• INC x (“increment x”)
Add one token to Box x;

To run a program, you first load the user’s input(s) into boxes Box1, Box2, Box3, . . .(Box0 is reserved for use by the computer itself). Then you run the program, one instruction after another, unless instructed to do otherwise by a jump instruction.Jump instructions look like this:
• JEQ x y n (“jump-on-equal”)
If Box x and Box y contain the same number of tokens, run instruction n next;
