with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Argument_Stack;

package Arguments is

	type Argument is abstract tagged limited private;

	type Parameter is new Argument with private;

	type Parameter_Modifier is (None, Optional, Repeats, Optionally_Repeats);

	function Define(Name : String; Modifier : Parameter_Modifier) return Parameter;

	procedure Write(Value : in Parameter);

	type Flag is new Argument with private;

	function Define(Name : String; Shorthand : String; Description : String) return Flag;

	procedure Write(Value : in Flag);

	type Action is new Argument with private;

	function Define(Name : String; Shorthand: String; Description : String) return Action;

	procedure Write(Value : in Action);

private
	type Argument is abstract tagged limited record
		Name : Unbounded_String;
	end record;

	type Parameter is new Argument with record
		Modifier : Parameter_Modifier;
	end record;

	type Flag is new Argument with record
		Shorthand : Unbounded_String;
		Description : Unbounded_String;
	end record;

	type Action is new Argument with record
		Shorthand : Unbounded_String;
		Description : Unbounded_String;
	end record;

end Arguments;