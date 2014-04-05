with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Directories;   use Ada.Directories; 

with Ada.Strings.Maps.Constants;
use Ada.Strings.Maps.Constants;

-- Project generator module
-- @author psyomn
package body GnatGen.Project_Generator is 

  -- Create the dirs, subdirs, main.adb, and gpr file
  procedure Generate_Project(Name : String) is
    Dir_Sep        : String := "/";
    GPR_Contents   : String := Make_GPR_Contents(Name);
    Main_Contents  : String := Make_Simple_Main_Contents;
    Current_Path   : String := Current_Directory;
    Lower_Filename : String := 
        Ada.Strings.Fixed.Translate(Name,
          Ada.Strings.Maps.Constants.Lower_Case_Map);
    GPR_File       : Ada.Text_IO.File_Type;
    Main_File      : Ada.Text_IO.File_Type;
  begin
    -- Create root dir
    Create_Directory(New_Directory => Lower_Filename);
    
    -- Create obj dir
    Create_Directory(New_Directory => Lower_Filename & Dir_Sep & "obj");
    
    -- Create obj/debug dir
    Create_Directory(New_Directory => Lower_Filename & Dir_Sep & "obj" 
      & Dir_Sep & "debug");
    
    -- Create obj/release dir
    Create_Directory(New_Directory => Lower_Filename & Dir_Sep & "obj"
      & Dir_Sep & "release");

    -- Create src dir
    Create_Directory(New_Directory => Lower_Filename & Dir_Sep & "src");

    -- Create bin/ dir
    Create_Directory(New_Directory => Lower_Filename & Dir_Sep & "bin");

    -- Create GPR file
    Create(GPR_File, Ada.Text_IO.Out_File, Lower_Filename & Dir_Sep 
      & Lower_Filename & ".gpr");
    Put(GPR_File, Make_GPR_Contents(Name)); 
    Close(GPR_File);

    -- Create hello world file
    Create(Main_File, Ada.Text_IO.Out_file, Lower_Filename & Dir_Sep
      & "src" & Dir_Sep & "main.adb");
    Put(Main_File, Main_Contents);
    Close(Main_File);
  end Generate_Project;

  -- Create simple hello world stuff
  function Make_Simple_Main_Contents return String is 
    Contents : String := 
      "with Ada.Text_IO; " & ASCII.LF & ASCII.LF &
      "procedure Main is begin" & ASCII.LF & 
      "  Ada.Text_IO.Put_Line(""hello world"");" & ASCII.LF &
      "end Main; " & ASCII.LF;
  begin
    return Contents;
  end Make_Simple_Main_Contents;

  -- TODO
  -- Make the gpr file with the required contents (ugly, but maybe fix later
  --   with something proper like a template).
  function Make_GPR_Contents(Name : String) return String is
    Contents : String :=  
      "-- Generated Gnat file " & ASCII.LF &
      "-- See: " & ASCII.LF &
      "--   http://docs.adacore.com/gnat-unw-docs/html/gnat_ugn_12.html " & ASCII.LF &
      "-- " & ASCII.LF &
      "-- Example use: " & ASCII.LF &
      "--   gnatmake -P " & Name & ".gpr -Xmode=debug -p " & ASCII.LF &
      "project " & Name & " is  " & ASCII.LF &
      ASCII.LF &
      "  -- Standard configurations " & ASCII.LF &
      "  for Main        use (""main.adb""); " & ASCII.LF &
      "  for Source_Dirs use (""src/**""); " & ASCII.LF &
      "  for Exec_Dir    use ""bin/""; " & ASCII.LF &
      ASCII.LF &
      "  -- Ignore git scm stuff " & ASCII.LF &
      "  for Ignore_Source_Sub_Dirs use ("".git/""); " & ASCII.LF &
      ASCII.LF &
      "  -- Objects are contained in their own directories (this is also " & ASCII.LF &
      "  -- known as an isolated build). " & ASCII.LF &
      "  for Object_Dir use ""obj/"" & external (""mode"", ""debug""); " & ASCII.LF &
      "  for Object_Dir use ""obj/"" & external (""mode"", ""release""); " & ASCII.LF &
      ASCII.LF & 
      "  package Builder is  " & ASCII.LF &
      "    for Executable (""main.adb"") use """
        &
        -- binary name should be lowercase
        Ada.Strings.Fixed.Translate(Name,
          Ada.Strings.Maps.Constants.Lower_Case_Map)
        &
      """; " & ASCII.LF &
      ASCII.LF &
      "  end Builder; " & ASCII.LF &
      ASCII.LF &
      "  -- To invoke either case, you need to set the -X flag at gnatmake in command " & ASCII.LF &
      "  -- line. You will also notice the Mode_Type type. This constrains the values " & ASCII.LF &
      "  -- of possible valid flags. " & ASCII.LF &
      "  type Mode_Type is (""debug"", ""release""); " & ASCII.LF &
      "  Mode : Mode_Type := external (""mode"", ""debug""); " & ASCII.LF &
      "  package Compiler is  " & ASCII.LF &
      "    -- Either debug or release mode " & ASCII.LF &
      "    case Mode is  " & ASCII.LF &
      "    when ""debug"" => " & ASCII.LF &
      "      for Switches (""Ada"") use (""-g""); " & ASCII.LF &
      "    when ""release"" =>  " & ASCII.LF &
      "      for Switches (""Ada"") use (""-O2""); " & ASCII.LF &
      "    end case; " & ASCII.LF &
      "  end Compiler; " & ASCII.LF &
      ASCII.LF &
      "  package Binder is  " & ASCII.LF &
      "  end Binder;  " & ASCII.LF &
      ASCII.LF &
      "  package Linker is  " & ASCII.LF &
      "  end Linker; " & ASCII.LF &
      ASCII.LF &
      "end " & Name & "; " & ASCII.LF;
  begin
    return Contents;
  end Make_GPR_Contents;

end GnatGen.Project_Generator;

