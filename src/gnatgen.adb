with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body GnatGen is

  function Sanitize_Name(Source : String) return String is
    subtype Invalid_Chars is Character with Dynamic_Predicate => Invalid_Chars = '-';
    Result : String := Source;
  begin
    for I in Result'First .. Result'Last loop
      if Result(I) in Invalid_Chars then
        Result(I) := '_';
      end if;
    end Loop;
    return Result;
  end Sanitize_Name;

  procedure Create_Project(Name : String) is
	begin
		Put("Creating Project...");
		-- Create root directory
		Create_Directory(Name);

		-- Create obj directory
		Create_Directory(Name & Directory_Separator & "obj");

		-- Create obj/debug directory
		Create_Directory(Name & Directory_Separator & "obj" & Directory_Separator & "debug");

		-- Create obj/release directory
		Create_Directory(Name & Directory_Separator & "obj" & Directory_Separator & "release");

		-- Create src directory
		Create_Directory(Name & Directory_Separator & "src");

		-- Create bin directory
		Create_Directory(Name & Directory_Separator & "bin");

		-- Create GPR file
		Create_GPR(Name);

		-- Create empty Main
		Create_Program("main");
	end Create_Project;

  procedure Create_GPR(Name : String) is
		File : File_Type;
    Sanitzed_Name : constant String := Sanitize_Name(Name);
	begin
		Create(File, Out_File, Name & ".gpr");
		Put_Line(File, "--Generated GNAT Project file");
		Put_Line(File, "--Example use:");
		Put_Line(File, "--  gprbuild -P " & Sanitzed_Name & ".gpr -Xmode=debug -p");
		Put_Line(FIle, "--    or");
		Put_Line(File, "--  gnatmake -P " & Name & ".gpr -Xmode=debug -p");
		Put_Line(File, "project " & Sanitzed_Name & " is");
		New_Line(File);
		Put_Line(File, "  --Standard configurations");
		Put_Line(File, "  for Main        use (""main.adb"");");
		Put_Line(File, "  for Source_Dirs use (""src/**"");");
		Put_Line(File, "  for Exec_Dir    use ""bin/"";");
		New_Line(File);
		Put_Line(File, "  --Ignore git scm stuff");
		Put_Line(File, "  for Ignore_Source_Sub_Dirs use ("".git/"");");
		New_Line(File);
		Put_Line(File, "  for Object_Dir use ""obj/"" & external(""mode"", ""debug"");");
		Put_Line(File, "  for Object_Dir use ""obj/"" & external(""mode"", ""release"");");
		New_Line(File);
		Put_Line(File, "  package Builder is");
		Put_Line(File, "    for Executable (""main.adb"") use """ & To_Lower(Sanitzed_Name) & """;");
		Put_Line(File, "  end Builder;");
		Put_Line(File, "  --To invoke either case, you need to set the -Xmode= flag for gprbuild or gnatmake in the command line. You will also notice the Mode_Type type. This constrains the values of possible valid flags; it is basically an enumeration.");
		Put_Line(File, "  type Mode_Type is (""debug"", ""release"");");
		Put_Line(File, "  Mode : Mode_Type := external (""mode"", ""debug"");");
		Put_Line(File, "  package Compiler is");
		Put_Line(File, "    --Either debug or release mode");
		Put_Line(File, "    case Mode is");
		Put_Line(File, "    when ""debug"" =>");
		Put_Line(File, "      for Switches (""Ada"") use (""-g"");");
		Put_Line(File, "    when ""release"" =>");
		Put_Line(File, "      for Switches (""Ada"") use (""--O2"");");
		Put_Line(File, "    end case;");
		Put_Line(File, "  end Compiler;");
		New_Line(File);
		Put_Line(File, "  package Binder is");
		Put_Line(File, "  end Binder;");
		New_Line(File);
		Put_Line(File, "  package Linker is");
		Put_Line(File, "  end Linker;");
		Put_Line(File, "end " & Sanitzed_Name & ";");
		Close(File);
	end Create_GPR;

  procedure Create_Program(Name : String) is
		File : File_Type;
    Sanitzed_Name : constant String := Sanitize_Name(Name);
	begin
		Create(File, Out_File, Name & ".adb");
		Put_Line(File, "procedure " & Sanitzed_Name & "is");
		Put_Line(File, "begin");
		Put_Line(File, "  null;");
		Put_Line(File, "end " & Sanitzed_Name & ";");
		Close(File);
	end Create_Program;

  -- Handle things that we want to print (eg: gpr files, function defs etc)
  --procedure Handle_Print(Params : String_Array) is
  --  Wanted : Unbounded_String;
  --begin
  --  if Params'Length < 2 then
  --    Put_Line("You need to provide a type to print (eg: gpr)");
  --   return;
  --  end if;
  --
  --  Wanted := Params(2);
  --
  --  if Wanted = "gpr" or Wanted = "GPR" then
  --    Put(Code_Generator.GPR(To_String(Params(3))));
  --    return;
  --
  --  elsif Wanted = "main" then
  --    Put(Code_Generator.Main);
  --    return;
  --
  --  elsif Wanted = "fn" or Wanted = "function" then
  --    Put(Code_Generator.Make_Func(
  --      Name => To_String(Params(3)),
  --      Params => Params(Params'First + 3.. Params'Last)
  --      ));
  --    return;
  --
  --  elsif Wanted = "proc" or Wanted = "procedure" then
  --    Put(Code_Generator.Make_Procedure(
  --      Name => To_String(Params(3)),
  --      Params => Params(Params'First + 3 .. Params'Last)
  --      ));
  --    return;
  --
  --  elsif Wanted = "cmm" or Wanted = "comment" then
  --    Put(Code_Generator.Make_Comments(
  --      Params => Params(Params'First + 2 .. Params'Last)
  --      ));
  --    return;
  --
  --  end if;
  --
  --end Handle_Print;

end GnatGen;
