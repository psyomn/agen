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

with Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;

with GnatGen;

package GnatGen.Code_Generator is

  package US renames Ada.Strings.Unbounded;

  function GPR(Name : String) return String;

  function Main return String;

  function Make_Func(Name : String;
    Params : GnatGen.String_Array) return String;

  function Make_Procedure(Name : String;
    Params : GnatGen.String_Array) return String;

  function Make_Body(Name : String) return String;

  function Make_Comments(Params : GnatGen.String_Array) return String;

private

  function Get_Attribute_Name(Attr : String) return String;
  function Get_Attribute_Type(Attr : String) return String;
  function Quick_Split(Attr : String; Choice : Slice_Number) return String;
  function Make_Type_Signature(Params : GnatGen.String_Array) return String;

end GnatGen.Code_Generator;
