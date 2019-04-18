with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package GnatGen is
  type String_Array is array (Positive range <>) of Unbounded_String;

  procedure Handle_New(Params : String_Array);

  procedure Handle_Print(Params : String_Array);

  procedure Handle_Throwaway;

end GnatGen;
