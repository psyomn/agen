-- Certain types are rightfully private but need their fields tested. Those fields should not actually be exposed through functions. This allows us to still test those while keeping the API visibility what it should be.
package Agen.Testing is

  ---------------
  -- Parameter --
  ---------------

  procedure Is_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String);

  procedure Is_Not_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String);

end Agen.Testing;