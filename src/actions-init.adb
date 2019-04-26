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
         Agen.Create_Project(Argument_Stack.Pop);
         return True;
      elsif To_Upper(Target) = "GPR" then
         if Argument_Stack.Is_Empty then
            Put_Line(Standard_Error, "Error: No name was specified");
            goto Fail;
         end if;
         Agen.Create_GPR(Argument_Stack.Pop);
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
