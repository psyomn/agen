with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

--@description Provides a specialized argument stack
--@remarks Ada.Containers doesn't provide a stack, but stack parsing the arguments is highly beneficial, so this implements what we need out of a stack. This stack specializes in Unbounded_String operations and can't be used for anything else
package Argument_Stack with Preelaborate is

	----------
	-- Node --
	----------

	type Node is private;

	type Node_Access is access all Node;

	-----------
	-- Stack --
	-----------
	--This is implemented as an "abstract state machine" or singleton because when would you actually have multiple arguments to parse?

	procedure Load_Arguments; --Load the arguments from the command line into the stack

	function Is_Empty return Boolean with Inline; --Whether the argument stack is empty

	function Length return Natural; --Length of the stack, the amount of arguments in it

	procedure Push(Value : in Unbounded_String); --Push the string onto the stack

	function Pop return Unbounded_String; --Pop the value off the stack

	function Peek return Unbounded_String; --Peek at the value on the stack top

private
	type Node is record
		Value : Unbounded_String;
		Next : Node_Access;
	end record;

	Length : Natural := 0; --The length of the stack

	Top : Node_Access; --The top node of the stack

end Argument_Stack;