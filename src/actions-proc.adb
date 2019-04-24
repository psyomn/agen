with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Agen; use Agen;
with Argument_Stack;

package body Actions.Proc is

  procedure Help is
  begin
    Put_Line("  (proc|procedure) [parameter:type]* - Print the generated procedure");
  end Help;

  function Try_Act return Boolean is
  begin
    if Argument_Stack.Is_Empty then goto Fail; end if;
    declare
      Action : constant String := Argument_Stack.Pop;
    begin
      if To_Upper(Action) /= "PROC" and To_Upper(Action) /= "PROCEDURE" then
        goto Fail;
      end if;
    end;
    if Argument_Stack.Is_Empty then
      Put_Line(Standard_Error, "Error: No name was specified");
      goto Fail;
    end if;
    declare
      Name : constant String := Argument_Stack.Pop;
      Params : Parameter_Array(1 .. Argument_Stack.Length);
    begin
      if Argument_Stack.Is_Empty then
        Print_Procedure(Name);
      else
        for I in 1 .. Argument_Stack.Length loop
          if not Try_Parse(Argument_Stack.Pop, Params(I)) then
            Argument_Stack.Push_Back;
            Put_Line(Standard_Error, "Error: The parameter signature """ & Argument_Stack.Pop & """ was invalid");
            goto Fail;
          end if;
        end loop;
        Print_Procedure(Name, Params);
      end if;
    end;
    return True;
    <<Fail>>
    Argument_Stack.Reset;
    return False;
  end Try_Act;

end Actions.Proc;
