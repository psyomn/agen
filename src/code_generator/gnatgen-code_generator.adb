with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split;     use GNAT.String_Split;

with GnatGen.Project_Generator; use GnatGen.Project_Generator;

package body GnatGen.Code_Generator is 

  -- print the gpr file template given a name 
  function GPR(Name : String) return String is
  begin
    return Project_Generator.Make_GPR_Contents(Name);
  end GPR;

  -- print a main file template 
  function Main return String is 
  begin return Make_Simple_Main_Contents;
  end Main;

  function Make_Func(Name : String; Params : String_Array) return String is 
    use ASCII;
    Contents : Unbounded_String;
  begin
    US.Append(Contents, "function " & Get_Attribute_Name(Name));
    US.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, " return " & Get_Attribute_Type(Name));
    US.Append(Contents, Make_Body(Name));

    return US.To_String(Contents);
  end Make_Func;

  -- @param Name is like hello:string, but because this is procedure we ignore
  --   the return type.
  function Make_Procedure(Name : String; Params : String_Array) return String is
    use ASCII;
    Contents : Unbounded_String;
  begin
    US.Append(Contents, "procedure " & Get_Attribute_Name(Name));
    Us.Append(Contents, Make_Type_Signature(Params));
    US.Append(Contents, Make_Body(Name));
    return US.To_String(Contents);
  end Make_Procedure;


-- Private

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

