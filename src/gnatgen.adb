with Ada.Text_IO; use Ada.Text_IO;
with GnatGen.Project_Generator; use GnatGen.Project_Generator;
with GnatGen.Code_Generator; use GnatGen.Code_Generator;

package body GnatGen is
  procedure Handle_Throwaway is
  begin
    Project_Generator.Generate_Throwaway;
  end Handle_Throwaway;

  -- We know we want to handle something 'new'. Next argument hints us what.
  procedure Handle_New(Params : String_Array) is
    New_Type : Unbounded_String;
  begin
    if Params'Length < 3 then
      Put_Line("You need to provide a name.");
      return;
    end if;

    New_Type := Params(2);

    if New_Type = "project" then
      Put_Line("Creating project...");
      Project_Generator.Generate_Project(To_String(Params(3)));
      return;

    elsif New_Type = "submodule" then
      Put_Line("Going to create submodule here");

    end if;

  end Handle_New;

  -- Handle things that we want to print (eg: gpr files, function defs etc)
  procedure Handle_Print(Params : String_Array) is
    Wanted : Unbounded_String;
  begin
    if Params'Length < 2 then
      Put_Line("You need to provide a type to print (eg: gpr)");
      return;
    end if;

    Wanted := Params(2);

    if Wanted = "gpr" or Wanted = "GPR" then
      Put(Code_Generator.GPR(To_String(Params(3))));
      return;

    elsif Wanted = "main" then
      Put(Code_Generator.Main);
      return;

    elsif Wanted = "fn" or Wanted = "function" then
      Put(Code_Generator.Make_Func(
        Name => To_String(Params(3)),
        Params => Params(Params'First + 3.. Params'Last)
        ));
      return;

    elsif Wanted = "proc" or Wanted = "procedure" then
      Put(Code_Generator.Make_Procedure(
        Name => To_String(Params(3)),
        Params => Params(Params'First + 3 .. Params'Last)
        ));
      return;

    elsif Wanted = "cmm" or Wanted = "comment" then
      Put(Code_Generator.Make_Comments(
        Params => Params(Params'First + 2 .. Params'Last)
        ));
      return;

    end if;

  end Handle_Print;

end GnatGen;
