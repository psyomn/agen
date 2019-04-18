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
    Append(Contents, Make_Comments(Params));
    Append(Contents, "function " & Get_Attribute_Name(Name));
    Append(Contents, Make_Type_Signature(Params));
    Append(Contents, " return " & Get_Attribute_Type(Name));
    Append(Contents, Make_Body(Name));

    return To_String(Contents);
  end Make_Func;

  function Make_Procedure(Name : String; Params : String_Array) return String is
    Contents : Unbounded_String;
  begin
    Append(Contents, Make_Comments(Params));
    Append(Contents, "procedure " & Get_Attribute_Name(Name));
    Append(Contents, Make_Type_Signature(Params));
    Append(Contents, Make_Body(Name));
    return To_String(Contents);
  end Make_Procedure;

  function Make_Comments(Params : GnatGen.String_Array) return String is
    Now      : Constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Date     : Constant String := GNAT.Calendar.Time_IO.Image(Now, "%Y-%m-%d");
    Contents : Unbounded_String;
  begin
    if Ada.Environment_Variables.Exists(Name => "USER") then
      Append(Contents, "-- @author ");
      Append(Contents, Ada.Environment_Variables.Value(Name => "USER"));
      Append(Contents, Ascii.LF);
    end if;

    for ix in Params'First .. Params'Last loop
      Append(Contents, "-- @param ");
      Append(Contents, Get_Attribute_Name(To_String(Params(ix))));
      Append(Contents, Ascii.LF);
    end loop;

    Append(Contents, "-- @date ");
    Append(Contents, Date & " (iso)");
    Append(Contents, Ascii.LF);
    return To_String(Contents);
  end Make_Comments;

  function Make_Type_Signature(Params : String_Array) return String is
    Contents : Unbounded_String;
  begin
    Append(Contents, "(");
    through_params :
    for ix in Params'First..Params'Last loop
      Ada.Strings.Unbounded.Append(
        Source => Contents,
        New_Item =>
          -- paramname : type
          Get_Attribute_Name(To_String(Params(ix))) & " : " &
          Get_Attribute_Type(To_String(Params(ix))));

      if ix /= Params'Last then
        Append(Contents, " ; ");
      end if;
    end loop through_params;
    Append(Contents, ")");
    return To_String(Contents);
  end Make_Type_Signature;

  function Make_Body(Name : String) return String is
    use ASCII;
    Contents : Unbounded_String;
  begin
    Append(Contents, " is " & LF);
    Append(Contents, "-- Enter your contents here..." & LF);
    Append(Contents, "begin" & LF);
    Append(Contents, "end " & Get_Attribute_Name(Name) & ";" & LF);
    return To_String(Contents);
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
