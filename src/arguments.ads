with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Arguments is

	--------------
	-- Argument --
	--------------

	type Argument is abstract tagged private;

	procedure Write(Value : in Argument) is abstract;

	---------------
	-- Parameter --
	---------------

	type Parameter is new Argument with private;

	type Parameter_Array is array(Positive range <>) of Parameter;

	type Parameter_Modifier is (None, Optional, Repeats, Optionally_Repeats);

	function Define(Name : String) return Parameter;

	function Define(Name : String; Modifier : Parameter_Modifier) return Parameter;

	overriding procedure Write(Value : in Parameter);

	----------
	-- Flag --
	----------

	type Flag is new Argument with private;

	function Define(Name : String; Shorthand : String; Description : String) return Flag;

	overriding procedure Write(Value : in Flag);

	------------
	-- Action --
	------------

	type Action is new Argument with private;

	function Define(Name : String; Shorthand : String; Description : String) return Action;

	function Define(Name : String; Shorthand : String; Description : String; Parameters : Parameter_Array) return Action;

	overriding procedure Write(Value : in Action);

private
	type Argument is abstract tagged record
		Name : Unbounded_String;
	end record;

	type Parameter is new Argument with record
		Modifier : Parameter_Modifier;
	end record;

	package Parameter_Vectors is new Ada.Containers.Vectors(Positive, Parameter);
	use Parameter_Vectors;

	type Flag is new Argument with record
		Shorthand : Unbounded_String;
		Description : Unbounded_String;
	end record;

	type Action is new Argument with record
		Shorthand : Unbounded_String;
		Description : Unbounded_String;
		Parameters : Parameter_Vectors.Vector := Empty_Vector;
	end record;

end Arguments;
