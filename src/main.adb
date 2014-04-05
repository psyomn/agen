with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with GnatGen;
with GnatGen.Project_Generator; use GnatGen.Project_Generator;

-- Entry point for this command line application.
procedure Main is 

  procedure Print_Usage is
  begin
    Put_Line("Use:");
    Put_Line("gnatgen <action> [params]");
    Put_Line("  new project - to create a template of a new project");
  end Print_Usage;

begin
 
  if Argument_Count < 1 then
    Print_Usage;
    return;
  end if;

  -- Add here any other possible commands
  if Argument(Number => 1) = "new" then
    GnatGen.handle_new;
  else
    Put_Line("Did not understand that");
  end if;

end Main;
