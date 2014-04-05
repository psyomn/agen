with Ada.Text_io;      use Ada.Text_io;
with Ada.Command_Line; use Ada.Command_Line;

with GnatGen.Project_Generator;

package body GnatGen is 

  -- We know we want to handle something 'new'. Next argument hints us what.
  procedure Handle_New is
    New_Type : String := Argument(Number => 2);
  begin
    if Argument_Count < 3 then
      Put_Line("You need to provide a name.");
      return;
    end if;

    if New_Type = "project" then
      Put_Line("Creating project...");
      GnatGen.Project_Generator.Generate_Project(Argument(Number => 3));
      return;

    elsif New_Type = "submodule" then
      Put_Line("Going to create submodule here");
    
    end if;

  end Handle_New;

  procedure Handle_New_Project is 
  begin
    put_line("going to handle new project");
  end Handle_New_Project;
end GnatGen;
