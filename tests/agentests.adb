with Agen; use Agen;
with Testing; use Testing;
with Agen.Testing; use Agen.Testing;

procedure AgenTests is
begin
	Testing.Start("Agen");

	declare
		Param : Parameter;
	begin
		Is_Equal("Try_Parse(""name:string"")", Try_Parse("name:string", Param), True);
		Is_Equal("Parameter", Param, "name", "string");
	end;

	declare
		Param : Parameter;
	begin
		Is_Equal("Try_Parse(""name"")", Try_Parse("name", Param), False);
		Is_Equal("Parameter", Param, "name", "");
	end;

	Testing.Stop;
end AgenTests;