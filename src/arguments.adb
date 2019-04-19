with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Argument_Stack;

package body Arguments is

	---------------
	-- Parameter --
	---------------

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

	----------
	-- Flag --
	----------

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

	------------
	-- Action --
	------------

	function Define(Name : String; Shorthand : String; Description : String) return Action is
	begin
		return A : Action do
			A.Name := To_Unbounded_String(Name);
			A.Shorthand := To_Unbounded_String(Shorthand);
			A.Description := To_Unbounded_String(Description);
		end return;
	end Define;

	function Define(Name : String; Shorthand : String; Description : String; Workload : Workload_Access) return Action is
	begin
		return A : Action do
			A := Define(Name, Shorthand, Description);
			A.Workload := Workload;
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

	function Define(Name : String; Shorthand : String; Description : String; Parameters : Parameter_Array; Workload : Workload_Access) return Action is
	begin
		return A : Action do
			A := Define(Name, Shorthand, Description, Parameters);
			A.Workload := Workload;
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

	-------------
	-- Parsing --
	-------------

	function Try_Parse(Self : aliased Action) return Parser_Result'Class is
		Source : constant String := Argument_Stack.Pop;
	begin
		if To_Upper(To_String(Self.Name)) = To_Upper(Source) or To_Upper(To_String(Self.Shorthand)) = To_Upper(Source) then
			return Parser_Result'(Match => Self'Access);
		else 
			Argument_Stack.Push_Back;
			return Parser_Result'(Match => null);
		end if;
	end Try_Parse;

	procedure Work(Self : Parser_Result) is
	begin
		--If the parser wasn't a success, don't take action
		if Self.Match = null then
			return;
		else
			--If the parser was a success, we have stuff to work with
			if Self.Match.Parameters.Is_Empty then
				Self.Match.Workload.all;
			end if;
		end if;
	end Work;

end Arguments;
