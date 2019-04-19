with Ada.Command_Line; use Ada.Command_Line;

package body Argument_Stack is

	function Is_Empty return Boolean is (Current > Argument_Count);

	function Length return Natural is (Argument_Count);

	procedure Reset is
	begin
		Current := 1;
	end Reset;

	procedure Push_Back is
	begin
		Current := Current - 1;
	end Push_Back;

	function Pop return String is
	begin
		Current := Current + 1;
		return Argument(Current - 1);
	end Pop;

end Argument_Stack;