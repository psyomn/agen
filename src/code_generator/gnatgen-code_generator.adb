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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;

with GnatGen.Project_Generator; use GnatGen.Project_Generator;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body GnatGen.Code_Generator is
  function GPR(Name : String) return String is
  begin
    return Project_Generator.Make_GPR_Contents(Name);
  end GPR;

  function Main return String is
  begin
    return Make_Simple_Main_Contents;
  end Main;

  function Make_Func(Name : String; Params : String_Array) return String is
    Contents : Unbounded_String;
  begin
    US.Append(Contents, Make_Comments(Params));
    US.Append(Contents, "function " & Get_Attribute_Name(Name));
    US.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, " return " & Get_Attribute_Type(Name));
    US.Append(Contents, Make_Body(Name));

    return US.To_String(Contents);
  end Make_Func;

  function Make_Procedure(Name : String; Params : String_Array) return String is
    Contents : Unbounded_String;
  begin
    US.Append(Contents, Make_Comments(Params));
    US.Append(Contents, "procedure " & Get_Attribute_Name(Name));
    Us.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, Make_Body(Name));
    return US.To_String(Contents);
  end Make_Procedure;

  function Make_Comments(Params : GnatGen.String_Array) return String is
    Now      : Constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Date     : Constant String := GNAT.Calendar.Time_IO.Image(Now, "%Y-%m-%d");
    Contents : Unbounded_String;
  begin
    if Ada.Environment_Variables.Exists(Name => "USER") then
      US.Append(Contents, "-- @author ");
      US.Append(Contents, Ada.Environment_Variables.Value(Name => "USER"));
      US.Append(Contents, Ascii.LF);
    end if;

    for ix in Params'First .. Params'Last loop
      US.Append(Contents, "-- @param ");
      US.Append(Contents, Get_Attribute_Name(US.To_String(Params(ix))));
      US.Append(Contents, Ascii.LF);
    end loop;

    US.Append(Contents, "-- @date ");
    US.Append(Contents, Date & " (iso)");
    US.Append(Contents, Ascii.LF);
    return US.To_String(Contents);
  end Make_Comments;

  function Make_Type_Signature(Params : String_Array) return String is
    Contents : Unbounded_String;
  begin
    US.Append(Contents, "(");
    through_params :
    for ix in Params'First..Params'Last loop
      Ada.Strings.Unbounded.Append(
        Source => Contents,
        New_Item =>
          -- paramname : type
          Get_Attribute_Name(US.To_String(Params(ix))) & " : " &
          Get_Attribute_Type(US.To_String(Params(ix))));

      if ix /= Params'Last then
        US.Append(Contents, " ; ");
      end if;
    end loop through_params;
    US.Append(Contents, ")");
    return US.To_String(Contents);
  end Make_Type_Signature;

  function Make_Body(Name : String) return String is
    use ASCII;
    Contents : Unbounded_String;
  begin
    Us.Append(Contents, " is " & LF);
    US.Append(Contents, "-- Enter your contents here..." & LF);
    US.Append(Contents, "begin" & LF);
    US.Append(Contents, "end " & Get_Attribute_Name(Name) & ";" & LF);
    return US.To_String(Contents);
  end Make_Body;

  function Get_Attribute_Name(Attr : String) return String is
  begin
    return Quick_Split(Attr, 1);
  end Get_Attribute_Name;

  function Get_Attribute_Type(Attr : String) return String is
  begin
    return Quick_Split(Attr, 2);
  end Get_Attribute_Type;

  function Quick_Split(Attr : String; Choice : Slice_Number) return String is
    Subs : GNAT.String_Split.Slice_Set;
    Seps : Constant String := ":";
  begin
    GNAT.String_Split.Create (Subs, Attr,
      Seps, Mode => GNAT.String_Split.Multiple);
    declare
      -- Choice takes either the first:second
      Ret : Constant String := GNAT.String_Split.Slice(Subs, Choice);
    begin
      return Ret;
    end;
  exception when others =>
    return "[param-error]";
  end Quick_Split;
end GnatGen.Code_Generator;
