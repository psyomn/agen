-- Copyright 2014-2019 Simon Symeonidis (psyomn), Patrick Kelly (entomy)
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


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Agen is

  type Parameter is private;

  type Parameter_Array is array(Positive range <>) of Parameter;

  function Try_Parse(Candidate : String; Result : out Parameter) return Boolean;

  ------------
  -- Create --
  ------------

  procedure Create_Project(Name : String);

  procedure Create_GPR(Name : String);

  procedure Create_Program(Name : String);

  -----------
  -- Print --
  -----------

  procedure Print_Comment(Message : String);

  procedure Print_Description_Comment(Message : String);

  procedure Print_Exception_Comment(Name : String; Message : String);

  procedure Print_Field_Comment(Name : String; Message : String);

  procedure Print_Param_Comment(Name : String; Message : String);

  procedure Print_Return_Comment(Message : String);

  procedure Print_Summary_Comment(Message : String);

  procedure Print_Value_Comment(Name : String; Message : String);

  procedure Print_Procedure(Name : String);

  procedure Print_Procedure(Name : String; Stub_Comments : Boolean);

  procedure Print_Procedure(Name : String; Param : Parameter);

  procedure Print_Procedure(Name : String; Param : Parameter; Stub_Comments : Boolean);

  procedure Print_Procedure(Name : String; Params : Parameter_Array);

  procedure Print_Procedure(Name : String; Params : Parameter_Array; Stub_Comments : Boolean);

  procedure Print_Function(Form : Parameter);

  procedure Print_Function(Form : Parameter; Stub_Comments : Boolean);

  procedure Print_Function(Name : String; Returns : String);

  procedure Print_Function(Name : String; Returns : String; Stub_Comments : Boolean);

  procedure Print_Function(Form : Parameter; Param : Parameter);

  procedure Print_Function(Form : Parameter; Param : Parameter; Stub_Comments : Boolean);

  procedure Print_Function(Name : String; Returns : String; Param : Parameter);

  procedure Print_Function(Name : String; Returns : String; Param : Parameter; Stub_Comments : Boolean);

  procedure Print_Function(Form : Parameter; Params : Parameter_Array);

  procedure Print_Function(Form : Parameter; Params : Parameter_Array; Stub_Comments : Boolean);

  procedure Print_Function(Name : String; Returns : String; Params : Parameter_Array);

  procedure Print_Function(Name : String; Returns : String; Params : Parameter_Array; Stub_Comments : Boolean);

private
  type Parameter is record
    Name : Unbounded_String;
    Of_Type : Unbounded_String;
  end record;

end Agen;
