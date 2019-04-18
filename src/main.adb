with Arguments; use Arguments;

-- Entry point for this command line application.
procedure Main is
	New_Action : constant Action := Define("new", "n", "Generate new source based on a specified template structure"); 
   Help_Flag : constant Flag := Define("help", "?", "Print the help info");
	Help_Action : constant Action := Define("help", "?", "Print the help info");
begin
   New_Action.Write;
   Help_Flag.Write;
end Main;
