with Ada.Command_Line; use Ada.Command_Line;

package body Argument_Stack is

	function Is_Empty return Boolean is (Argument_Count = 0);

	function Length return Natural is (Argument_Count);

	procedure Push_Back is
	begin
		Current := Current - 1;
	end Push_Back;

	function Pop return Unbounded_String Is_Empty
	Result : String := Argument(Current);
	begin
		Current := Current + 1;
		return Result;
	end Pop;

end Argument_Stack;