with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GnatGen; use GnatGen;
with Argument_Stack;

package body Actions.Init is

	procedure Help is
	begin
		Put_Line("  (new|init)");
		Put_Line("    project <name> - create a project from a basic template");
		Put_Line("    gpr <name> - create a GPR from a basic template");
	end Help;

	function Try_Act return Boolean is
	begin
		if Argument_Stack.Is_Empty then goto Fail; end if;
		declare
			Action : constant String := Argument_Stack.Pop;
		begin
			if To_Upper(Action) /= "NEW" and To_Upper(Action) /= "INIT" then
			  goto Fail;
			end if;
		end;
		if Argument_Stack.Is_Empty then
			Put_Line(Standard_Error, "Error: No target was specified");
			goto Fail;
		end if;
		declare
			Target : constant String := Argument_Stack.Pop;
		begin
			if To_Upper(Target) = "PROJECT" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No name was specifed");
					goto Fail;
				end if;
				GnatGen.Create_Project(Argument_Stack.Pop);
				return True;
			elsif To_Upper(Target) = "GPR" then
				if Argument_Stack.Is_Empty then
					Put_Line(Standard_Error, "Error: No name was specified");
					goto Fail;
				end if;
				GnatGen.Create_GPR(Argument_Stack.Pop);
				return True;
			else
				Put_Line(Standard_Error, "Error: """ & Target & """ was not an understood target");
				goto Fail;
			end if;
		end;
		<<Fail>>
		Argument_Stack.Reset;
		return False;
	end Try_Act;
end Actions.Init;