with Ada.Command_Line, Ada.Unchecked_Deallocation;
use Ada.Command_Line;

package body Argument_Stack is

	----------
	-- Node --
	----------

	procedure Deallocate is new Ada.Unchecked_Deallocation(Node, Node_Access);

	-----------
	-- Stack --
	-----------

	procedure Load_Arguments is
	begin
		for i in 1..Argument_Count;
	end Load_Arguments;

	function Is_Empty return Boolean is (Top = null);

	function Length return Natural is (Length);

	procedure Push(Value : in Unbounded_String) is
	begin
		Top := new Node'(Value, Top);
		Length := Length + 1;
	end Push;

	function Pop return Unbounded_String is
		Old_Top : Node_Access := Top;
		Result : Unbounded_String := Top.Value;
	begin
		Top := Top.Next;
		Deallocate(Old_Top);
		Length := Length - 1;
		return Result;
	end Pop;

	function Peek return Unbounded_String is (Top.Value);

end Argument_Stack;