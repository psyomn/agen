-- Copyright 2019 Patrick Kelly (entomy)
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