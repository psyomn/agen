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

private 

  function Get_Attribute_Name(Attr : String) return String;
  function Get_Attribute_Type(Attr : String) return String;
  function Quick_Split(Attr : String; Choice : Slice_Number) return String;
  function Make_Type_Signature(Params : GnatGen.String_Array) return String;

end GnatGen.Code_Generator;

