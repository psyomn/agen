with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GnatGen; use GnatGen;
with Argument_Stack;

package body Actions.Comment is

	procedure Help is
	begin
		Put_Line("  (cmm|comment)");
		Put_Line("    (desc|description) <message> - Print a description doc comment");
		Put_Line("    (ex|exception) <name> <mesage> - Print an exception doc comment");
		Put_Line("    field <name> <message> - Print a field doc comment");
		Put_Line("    param <name> <message> - Print a param doc comment");
		Put_Line("    (ret|return) <message> - Prints a return doc comment");
		Put_Line("    (summ|summary) <message> - Print a summary doc comment");
		Put_Line("    value <name> <message> - Print a value doc comment");
	end Help;

	function Try_Act return Boolean is
	begin
		if Argument_Stack.Is_Empty then goto Fail; end if;
		declare
			Action : constant String := Argument_Stack.Pop;
		begin
			if To_Upper(Action) /= "CMM" and To_Upper(Action) /= "COMMENT" then
				goto Fail;
			end if;
		end;
		-- We don't need to verify a target exists, because no target is a normal comment. As a result of this, we don't actually need any arguments in the stack, because we can always create an empty comment. So instead of failing, goto the creation of a basic comment
		if Argument_Stack.Is_Empty then goto Basic_Comment; end if;
		declare
			Target : constant String := Argument_Stack.Pop;
		begin
			if To_Upper(Target) = "DESC" or To_Upper(Target) = "DESCRIPTION" then
				Print_Description_Comment(Argument_Stack.Pop_Remaining);
				return True;
			elsif To_Upper(Target) = "EX" or To_Upper(Target) = "EXCEPTION" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No exception name was specified");
					goto Fail;
				end if;
				declare
					Name : constant String := Argument_Stack.Pop;
				begin
					Print_Exception_Comment(Name, Argument_Stack.Pop_Remaining);
					return True;
				end;
			elsif To_Upper(Target) = "FIELD" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No field name was specified");
					goto Fail;
				end if;
				declare
					Name : constant String := Argument_Stack.Pop;
				begin
					Print_Field_Comment(Name, Argument_Stack.Pop_Remaining);
					return True;
				end;
			elsif To_Upper(Target) = "PARAM" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No param name was specified");
					goto Fail;
				end if;
				declare
					Name : constant String := Argument_Stack.Pop;
				begin
					Print_Param_Comment(Name, Argument_Stack.Pop_Remaining);
					return True;
				end;
			elsif To_Upper(Target) = "RET" or To_Upper(Target) = "RETURN" then
				Print_Return_Comment(Argument_Stack.Pop_Remaining);
				return True;
			elsif To_Upper(Target) = "SUMM" or To_Upper(Target) = "SUMMARY" then
				Print_Summary_Comment(Argument_Stack.Pop_Remaining);
				return True;
			elsif To_Upper(Target) = "VALUE" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No value name was specified");
					goto Fail;
				end if;
				declare
					Name : constant String := Argument_Stack.Pop;
				begin
					Print_Value_Comment(Name, Argument_Stack.Pop_Remaining);
					return True;
				end;
			else
				--The "Target" was really the first word of the message, so put it back
				Argument_Stack.Push_Back;
				goto Basic_Comment;
			end if;
		end;
		<<Basic_Comment>>
		Print_Comment(Argument_Stack.Pop_Remaining);
		return True;
		<<Fail>>
		Argument_Stack.Reset;
		return False;
	end Try_Act;
end Actions.Comment;