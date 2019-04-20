with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Argument_Stack is

	function Is_Empty return Boolean is (Current > Argument_Count);

	function Length return Natural is ((Argument_Count + 1) - Current);

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

	function Pop_Remaining return String is
		Result : Unbounded_String;
	begin
		while not Is_Empty loop
			Append(Result, Pop & " ");
		end loop;
		return To_String(Result);
	end Pop_Remaining;

end Argument_Stack;