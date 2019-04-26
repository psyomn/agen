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

with Ada.Text_IO, Ada.Wide_Wide_Text_IO, Testing;
use Ada.Wide_Wide_Text_IO, Testing;

package body Agen.Testing is

   ---------------
   -- Parameter --
   ---------------

   procedure Is_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String) is
   begin
      if To_String(Result.Name) = Expected_Name and then To_String(Result.Of_Type) = Expected_Type then
         Pass;
      else
         Fail;
      end if;
      Put(Statement & " → Name: ");
      Ada.Text_IO.Put(To_String(Result.Name));
      Put(" = """);
      Ada.Text_IO.Put(Expected_Name);
      Put(""" Type: ");
      Ada.Text_IO.Put(To_String(Result.Of_Type));
      Put(" = """);
      Ada.Text_IO.Put(Expected_Type);
      Put("""");
   end Is_Equal;

   procedure Is_Not_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String) is
   begin
      if To_String(Result.Name) /= Expected_Name and then To_String(Result.Of_Type) /= Expected_Type then
         Pass;
      else
         Fail;
      end if;
      Put(Statement & " → Name: ");
      Ada.Text_IO.Put(To_String(Result.Name));
      Put(" ≠ """);
      Ada.Text_IO.Put(Expected_Name);
      Put(""" Type: ");
      Ada.Text_IO.Put(To_String(Result.Of_Type));
      Put(" ≠ """);
      Ada.Text_IO.Put(Expected_Type);
      Put("""");
   end Is_Not_Equal;

end Agen.Testing;