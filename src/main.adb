with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;

with GnatGen;

-- Entry point for this command line application.
procedure Main is

  package US renames Ada.Strings.Unbounded;

  Parameters : GnatGen.String_Array(1..Argument_Count);

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

  -- Copy parameters
  for ix in 1..Argument_Count loop
    Parameters(ix) := US.To_Unbounded_String(Argument(Number => ix));
  end loop;

  -- Add here any other possible commands
  if Argument(Number => 1) = "new" then
    GnatGen.handle_new(Parameters);

  elsif Argument (Number => 1) = "throwaway" or
        Argument (Number => 1) = "t" then
    GnatGen.Handle_Throwaway;

  elsif Argument(Number => 1) = "print" or
        Argument(Number => 1) = "p" then
    GnatGen.Handle_Print(Parameters);

  else
    Put_Line("Did not understand that");
  end if;
end Main;
