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

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;

with GnatGen;

procedure Main is

   package US renames Ada.Strings.Unbounded;

   Parameters : GnatGen.String_Array(1..Argument_Count);

   procedure Print_Usage is
   begin
      Put_Line("Use:");
      Put_Line("gnatgen <action> [params]");
      Put_Line("  new project   - to create a template of a new project");
      Put_Line("  <throwaway|t> - to create a quick hello world program");
      Put_Line("  <print|p> <fn|proc|cmm> funcname:returntype [param:type]+");
      Put_Line("                - to print generated code for functions, procedures, or comments");
      Put_Line("  <help>        - print this info");
   end Print_Usage;

begin

   if Argument_Count < 1 then
      Print_Usage;
      return;
   end if;

   -- Copy parameters
   for ix in 1..Argument_Count loop
      Parameters(ix) := US.To_Unbounded_String(Argument(Number => ix));
   end loop;

   -- Add here any other possible commands
   if Argument(Number => 1) = "new" then
      GnatGen.handle_new(Parameters);

   elsif Argument (Number => 1) = "throwaway" Or
     Argument (Number => 1) = "t" then
      GnatGen.Handle_Throwaway;

   elsif Argument(Number => 1) = "print" Or
     Argument(Number => 1) = "p" then
      GnatGen.Handle_Print(Parameters);

   elsif Argument(Number => 1) = "help" Or
     Argument(Number => 1) = "h" Then
      Print_Usage;

   else
      Print_Usage;

   end if;
end Main;
