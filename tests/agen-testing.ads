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

-- Certain types are rightfully private but need their fields tested. Those fields should not actually be exposed through functions. This allows us to still test those while keeping the API visibility what it should be.
package Agen.Testing is

   ---------------
   -- Parameter --
   ---------------

   procedure Is_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String);

   procedure Is_Not_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String);

end Agen.Testing;