with Ada.Text_IO; use Ada.Text_IO;

package body Arguments is

	function Define(Name : String; Modifier : Parameter_Modifier) return Parameter is
	begin
		return P : Parameter do
			P.Name := To_Unbounded_String(Name);
			P.Modifier := Modifier;
		end return;
	end Define;

	function Define(Name : String) return Parameter is (Define(Name, None));

	procedure Write(Value : in Parameter) is
	begin
		Put("<" & To_String(Value.Name) & ">");
		case Value.Modifier is
			when None => null;
			when Optional => Put("?");
			when Repeats => Put("+");
			when Optionally_Repeats => Put("*");
		end case;
	end Write;

	function Define(Name : String; Shorthand : String; Description : String) return Flag is
	begin
		return F : Flag do
			F.Name := To_Unbounded_String(Name);
			F.Shorthand := To_Unbounded_String(Shorthand);
			F.Description := To_Unbounded_String(Description);
		end return;
	end Define;

	procedure Write(Value : in Flag) is
	begin
		Put_Line(" -" & To_String(Value.Shorthand) & " | --" & To_String(Value.Name) & " - " & To_String(Value.Description));
	end Write;

	function Define(Name : String; Shorthand : String; Description : String) return Action is
	begin
		return A : Action do
			A.Name := To_Unbounded_String(Name);
			A.Shorthand := To_Unbounded_String(Shorthand);
			A.Description := To_Unbounded_String(Description);
		end return;
	end Define;

	function Define(Name : String; Shorthand : String; Description : String; Parameters : Parameter_Array) return Action is
	begin
		return A : Action do
			A := Define(Name, Shorthand, Description);
			for Parameter of Parameters loop
				A.Parameters.Append(Parameter);
			end loop;
		end return;
	end Define;

	procedure Write(Value : in Action) is
	begin
		Put(" " & To_String(Value.Shorthand) & " | " & To_String(Value.Name) & " ");
		for Parameter of Value.Parameters loop
			Parameter.Write;
		end loop;
		Put_Line(" - " & To_String(Value.Description));
		--! This is where flags need to be written
	end Write;

end Arguments;
