with Ada.Text_IO, Ada.Wide_Wide_Text_IO, Testing;
use Ada.Wide_Wide_Text_IO, Testing;

package body Agen.Testing is

  ---------------
  -- Parameter --
  ---------------

  procedure Is_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String) is
  begin
		if To_String(Result.Name) = Expected_Name and then To_String(Result.Of_Type) = Expected_Type then
			Pass;
		else
			Fail;
		end if;
		Put(Statement & " → Name: ");
		Ada.Text_IO.Put(To_String(Result.Name));
		Put(" = """);
		Ada.Text_IO.Put(Expected_Name);
		Put(""" Type: ");
		Ada.Text_IO.Put(To_String(Result.Of_Type));
		Put(" = """);
		Ada.Text_IO.Put(Expected_Type);
		Put("""");
  end Is_Equal;

  procedure Is_Not_Equal(Statement : Wide_Wide_String; Result : in Parameter; Expected_Name, Expected_Type : String) is
  begin
		if To_String(Result.Name) /= Expected_Name and then To_String(Result.Of_Type) /= Expected_Type then
			Pass;
		else
			Fail;
		end if;
		Put(Statement & " → Name: ");
		Ada.Text_IO.Put(To_String(Result.Name));
		Put(" ≠ """);
		Ada.Text_IO.Put(Expected_Name);
		Put(""" Type: ");
		Ada.Text_IO.Put(To_String(Result.Of_Type));
		Put(" ≠ """);
		Ada.Text_IO.Put(Expected_Type);
		Put("""");
  end Is_Not_Equal;

end Agen.Testing;