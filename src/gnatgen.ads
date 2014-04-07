with Ada.Strings.Unbounded;

package GnatGen is 
  package US renames Ada.Strings.Unbounded;

  type String_Array is array (Positive range <>) of US.Unbounded_String;

  procedure Handle_New(Params : String_Array);

end GnatGen;
