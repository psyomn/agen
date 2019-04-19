--@description Provides argument stack semantics
--@remarks Doesn't actually implement a stack, but exposes Ada.Command_Line as a stack, because those semantics are useful
package Argument_Stack with Preelaborate is

	-----------
	-- Stack --
	-----------
	--This is implemented as an "abstract state machine" or singleton because when would you actually have multiple arguments to parse?

	function Is_Empty return Boolean with Inline; --Whether the argument stack is empty.

	function Length return Natural; --Length of the stack, the amount of arguments in it.

	procedure Push_Back; --"Push" back onto the stack. This doesn't need a value because technically it just adjusts an index and the value was always there.

	function Pop return String; --Pop the value off the stack.

private
	Current : Positive := 1;

end Argument_Stack;