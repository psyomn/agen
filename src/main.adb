with Arguments; use Arguments;
with Argument_Stack;

-- Entry point for this command line application.
procedure Main is
	New_Action : constant Action := Define("new", "n", "Generate new sources based on a specified template structure", Parameter_Array'(1 => Define("project"))); --This bizarre looking ending is because Ada can't properly figure out creating arrays of 1 item, so we need to explain. While it might seem like creating another overload of Define that accepts a single Parameter is a viable solution, it's not, as that would be dispatching on two types, and making either classwide results in dynamic expressions where they aren't allowed.
	Help_Flag : constant Flag := Define("help", "?", "Print the help info");
	Help_Action : constant Action := Define("help", "?", "Print the help info");
begin
	if Argument_Stack.Is_Empty then
		Help_Action.Write;
		return;
	end if;
	New_Action.Try_Parse.Work;
	Help_Action.Try_Parse.Work;
end Main;
