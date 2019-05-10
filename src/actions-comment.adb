-- Copyright 2014-2019 Simon Symeonidis (psyomn)
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Agen; use Agen;
with Argument_Stack;

package body Actions.Comment is

   procedure Help is
   begin
      Put_Line("  (cmm|comment)");
      Put_Line("    (desc|description) <message> - Print a description doc comment");
      Put_Line("    (ex|exception) <name> <mesage> - Print an exception doc comment");
      Put_Line("    field <name> <message> - Print a field doc comment");
      Put_Line("    (function|fn) name:return_type [parameter:type]* - Prints a set of function doc comments");
      Put_Line("    param <name> <message> - Print a param doc comment");
      Put_Line("    (procedure|proc) name [parameter:type]* - Prints a set of procedure doc comments");
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
      elsif To_Upper(Target) = "FUNCTION" or To_Upper(Target) = "FN" then
         declare
            Func : Parameter;
         begin
            if Argument_Stack.Is_Empty then
               Put_Line(Standard_Error, "Error: no function signature was specified");
               goto Fail;
            end if;
            if not Try_Parse(Argument_Stack.Pop, Func) then
               Put_Line(Standard_Error, "Error: The function signature was invalid");
               goto Fail;
            end if;
            if Argument_Stack.Is_Empty then
               Print_Function_Comment(Func);
            else
               declare
                  Params : Parameter_Array(1 .. Argument_Stack.Length);
               begin
               for I in 1 .. Argument_Stack.Length loop
                  if not Try_Parse(Argument_Stack.Pop, Params(I)) then
                     Argument_Stack.Push_Back;
                     Put_Line(Standard_Error, "Error: The parameter signature """ & Argument_Stack.Pop & """ was invalid");
                     goto Fail;
                  end if;
               end loop;
               Print_Function_Comment(Func, Params);
               end;
            end if;
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
