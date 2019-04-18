with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Directories;   use Ada.Directories;

with Ada.Strings.Maps.Constants;
use Ada.Strings.Maps.Constants;

package body GnatGen.Project_Generator is
   procedure Generate_Throwaway is
      Main_File : Ada.Text_IO.File_Type;
   begin
      Create (Main_File, Ada.Text_IO.Out_file, "main.adb");
      Put (Main_File, Make_Simple_Main_Contents);
      Close (Main_File);
   end Generate_Throwaway;

   procedure Generate_Project(Name : String) is
      Dir_Sep        : Constant String := "/";
      Main_Contents  : Constant String := Make_Simple_Main_Contents;
      Lower_Filename : Constant String :=
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

   function Make_Simple_Main_Contents return String is
      Contents : Constant String :=
        "with Ada.Text_IO;" & ASCII.LF &
          ASCII.LF &
          "procedure Main is begin" & ASCII.LF &
          "  Ada.Text_IO.Put_Line(""hello world"");" & ASCII.LF &
          "end Main;" ;
   begin
      return Contents;
   end Make_Simple_Main_Contents;

   function Make_GPR_Contents(Name : String) return String is
      use ASCII;
      Contents : Constant String :=
        "-- Generated Gnat file" & LF &
          "-- Example use:" & LF &
          "--   gnatmake -P " & Name & ".gpr -Xmode=debug -p" & LF &
          "project " & Name & " is" & LF &
          LF &
          "  -- Standard configurations" & LF &
          "  for Main        use (""main.adb"");" & LF &
          "  for Source_Dirs use (""src/**"");" & LF &
          "  for Exec_Dir    use ""bin/"";" & LF &
          LF &
          "  -- Ignore git scm stuff" & LF &
          "  for Ignore_Source_Sub_Dirs use ("".git/"");" & LF &
          LF &
          "  for Object_Dir use ""obj/"" & external (""mode"", ""debug"");" & LF &
          "  for Object_Dir use ""obj/"" & external (""mode"", ""release"");" & LF &
          LF &
          "  package Builder is" & LF &
          "    for Executable (""main.adb"") use """
          &
          -- binary name should be lowercase
          Ada.Strings.Fixed.Translate(Name,
                                      Ada.Strings.Maps.Constants.Lower_Case_Map)
          &
          """;" & LF &
          LF &
          "  end Builder;" & LF &
          LF &
          "  -- To invoke either case, you need to set the -X flag at gnatmake in command" & LF &
          "  -- line. You will also notice the Mode_Type type. This constrains the values" & LF &
          "  -- of possible valid flags." & LF &
          "  type Mode_Type is (""debug"", ""release"");" & LF &
          "  Mode : Mode_Type := external (""mode"", ""debug"");" & LF &
          "  package Compiler is" & LF &
          "    -- Either debug or release mode" & LF &
          "    case Mode is" & LF &
          "    when ""debug"" =>" & LF &
          "      for Switches (""Ada"") use (""-g"");" & LF &
          "    when ""release"" =>" & LF &
          "      for Switches (""Ada"") use (""-O2"");" & LF &
          "    end case;" & LF &
          "  end Compiler;" & LF &
          LF &
          "  package Binder is" & LF &
          "  end Binder;" & LF &
          LF &
          "  package Linker is" & LF &
          "  end Linker;" & LF &
          LF &
          "end " & Name & ";" & LF;
   begin
      return Contents;
   end Make_GPR_Contents;
end GnatGen.Project_Generator;
