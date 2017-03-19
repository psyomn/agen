with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with GNAT.String_Split;     use GNAT.String_Split;

with GnatGen.Project_Generator; use GnatGen.Project_Generator;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;
with GNAT.IO;

-- @author psyomn
-- @date 2014-04-07 (iso)
package body GnatGen.Code_Generator is

  -- @author psyomn
  -- @date 2014-04-07 (iso)
  -- print the gpr file template given a name
  function GPR(Name : String) return String is
  begin
    return Project_Generator.Make_GPR_Contents(Name);
  end GPR;

  -- @author psyomn
  -- @date 2014-04-07 (iso)
  -- print a main file template
  function Main return String is
  begin return Make_Simple_Main_Contents;
  end Main;

  -- @author psyomn
  -- @param name
  -- @param params
  -- @date 2014-04-07 (iso)
  function Make_Func(Name : String; Params : String_Array) return String is
    use ASCII;
    Contents : Unbounded_String;
  begin
    US.Append(Contents, Make_Comments(Params));
    US.Append(Contents, "function " & Get_Attribute_Name(Name));
    US.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, " return " & Get_Attribute_Type(Name));
    US.Append(Contents, Make_Body(Name));

    return US.To_String(Contents);
  end Make_Func;

  -- @param Name is like hello:string, but because this is procedure we ignore
  --   the return type.
  -- @param Params are the parameters of the procedure (type sig)
  function Make_Procedure(Name : String; Params : String_Array) return String is
    use ASCII;
    Contents : Unbounded_String;
  begin
    US.Append(Contents, Make_Comments(Params));
    US.Append(Contents, "procedure " & Get_Attribute_Name(Name));
    Us.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, Make_Body(Name));
    return US.To_String(Contents);
  end Make_Procedure;


  -- @author psyomn
  -- @param params are the parameters for the type signature. This function
  --   will create a 'params' tag in order to document each one.
  -- @date 2014-04-07 (iso)
  function Make_Comments(Params : GnatGen.String_Array) return String is
    use ASCII;
    Now      : Ada.Calendar.Time := Ada.Calendar.Clock;
    Date     : String := GNAT.Calendar.Time_IO.Image(Now, "%Y-%m-%d");
    Contents : Unbounded_String;
  begin

    -- Add author name if exists
    if Ada.Environment_Variables.Exists(Name => "USER") then
      US.Append(Contents, "-- @author ");
      US.Append(Contents, Ada.Environment_Variables.Value(Name => "USER"));
      US.Append(Contents, LF);
    end if;

    -- gen comments for params
    for ix in Params'First .. Params'Last loop
      US.Append(Contents, "-- @param ");
      US.Append(Contents, Get_Attribute_Name(US.To_String(Params(ix))));
      US.Append(Contents, LF);
    end loop;

    -- Add Date
    US.Append(Contents, "-- @date ");
    US.Append(Contents, Date & " (iso)");
    US.Append(Contents, LF);
    return US.To_String(Contents);
  end Make_Comments;

-- Private

  -- @author psyomn
  -- @param params the parameters inside a type signature
  -- @return the type signature that is to be created
  -- @date 2014-04-07 (iso)
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

  -- @author psyomn
  -- @param name The name of the function/proc
  -- @return the body template
  -- @date 2014-04-07 (iso)
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

  -- @author psyomn
  -- @param Attr the attribute in form of attribname:attribtype
  -- @date 2014-04-07 (iso)
  function Get_Attribute_Name(Attr : String) return String is
  begin
    return Quick_Split(Attr, 1);
  end Get_Attribute_Name;

  -- @author psyomn
  -- @param Attr the attribute in form of attribname:attribtype
  -- @date 2014-04-07 (iso)
  function Get_Attribute_Type(Attr : String) return String is
  begin
    return Quick_Split(Attr, 2);
  end Get_Attribute_Type;

  -- @author psyomn
  -- @param Attr The attribute in form of attribname:attribtype
  -- @param Choice The choice, which is either 1, or 2 (attribname or
  --   attribtype respectively).
  -- @date 2014-04-07 (iso)
  function Quick_Split(Attr : String; Choice : Slice_Number) return String is
    Subs : GNAT.String_Split.Slice_Set;
    Seps : String := ":";

  begin
    GNAT.String_Split.Create (Subs, Attr,
      Seps, Mode => GNAT.String_Split.Multiple);

    declare
      -- Choice takes either the first:second
      Ret : String := GNAT.String_Split.Slice(Subs, Choice);
    begin
      return Ret;
    end;
  exception when Error: others =>
    return "[param-error]";
  end Quick_Split;

end GnatGen.Code_Generator;

