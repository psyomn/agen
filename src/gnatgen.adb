with Ada.Text_io;           use Ada.Text_io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GnatGen.Project_Generator;

package body GnatGen is 

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
      Project_Generator.Generate_Project(US.To_String(Params(3)));
      return;

    elsif New_Type = "submodule" then
      Put_Line("Going to create submodule here");
    
    end if;

  end Handle_New;

end GnatGen;
