-- Copyright 2014-2019 Simon Symeonidis (psyomn), Patrick Kelly (entomy)
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Agen is

  function Sanitize_Name(Source : String) return String is
    subtype Invalid_Chars is Character with Dynamic_Predicate => Invalid_Chars = '-' and Invalid_Chars = '.';
    Result : String := Source;
  begin
    for I in Result'First .. Result'Last loop
      if Result(I) in Invalid_Chars then
        Result(I) := '_';
      end if;
    end Loop;
    return Result;
  end Sanitize_Name;

  function Try_Parse(Candidate : String; Result : out Parameter) return Boolean is
    Split_Point : Positive := 1;
  begin
    --First we have to find where the : is
    for I in 1 .. Candidate'Length loop
      if Candidate(I) = ':' then
        Split_Point := I;
      end if;
    end loop;
    --if Split_Point is 1, we know a ':' was either never found, or found at the very beginging. Both of which are incorrect
    if Split_Point = 1 then
      return False;
    end if;
    Result := Parameter'(
      Name => To_Unbounded_String(Candidate(Candidate'First .. Split_Point - 1)),
      Of_Type => To_Unbounded_String(Candidate(Split_Point + 1 .. Candidate'Last)));
    return True;
  end Try_Parse;

  ------------
  -- Create --
  ------------

  procedure Create_Project(Name : String) is
  begin
    Put_Line("Creating Project...");
    
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
    Create_GPR(Name & Directory_Separator & Name);

    -- Create empty Main
    Create_Program(Name & Directory_Separator & "main");
  end Create_Project;

  procedure Create_GPR(Name : String) is
    File : File_Type;
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    Create(File, Out_File, Name & ".gpr");
    Put_Line(File, "--Generated GNAT Project file");
    Put_Line(File, "--Example use:");
    Put_Line(File, "--  gprbuild -P " & Name & ".gpr -Xmode=debug -p");
    Put_Line(FIle, "--    or");
    Put_Line(File, "--  gnatmake -P " & Name & ".gpr -Xmode=debug -p");
    Put_Line(File, "project " & Sanitized_Name & " is");
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
    Put_Line(File, "    for Executable (""main.adb"") use """ & To_Lower(Sanitized_Name) & """;");
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
    Put_Line(File, "end " & Sanitized_Name & ";");
    Close(File);
  end Create_GPR;

  procedure Create_Program(Name : String) is
    File : File_Type;
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    Create(File, Out_File, Name & ".adb");
    Put_Line(File, "procedure " & Sanitized_Name & "is");
    Put_Line(File, "begin");
    Put_Line(File, "  null;");
    Put_Line(File, "end " & Sanitized_Name & ";");
    Close(File);
  end Create_Program;

  -----------
  -- Print --
  -----------

  procedure Print_Comment(Message : String) is
  begin
    Put_Line("--" & Message);
  end Print_Comment;

  procedure Print_Description_Comment(Message : String) is
  begin
    Put_Line("--@description " & Message);
  end Print_Description_Comment;

  procedure Print_Exception_Comment(Name : String; Message : String) is
  begin
    Put_Line("--@exception " & Name & " " & Message);
  end Print_Exception_Comment;

  procedure Print_Field_Comment(Name : String; Message : String) is
  begin
    Put_Line("--@field " & Name & " " & Message);
  end Print_Field_Comment;

  procedure Print_Function_Comment(Name : String) is
  begin
    Print_Comment("Summary of " & Name);
    Print_Return_Comment("Summary of return value");
  end Print_Function_Comment;

  procedure Print_Function_Comment(Name : String; Param : Parameter) is
  begin
      Print_Comment("Summary of " & Name);
      Print_Param_Comment(To_String(Param.Name), "Summary of " & To_String(Param.Name));
      Print_Return_Comment("Summary of return value");
  end Print_Function_Comment;

  procedure Print_Function_Comment(Name : String; Params : Parameter_Array) is
  begin
      Print_Comment("Summary of " & Name);
      for Param of Params loop
        Print_Param_Comment(To_String(Param.Name), "Summary of " & To_String(Param.Name));
      end loop;
      Print_Return_Comment("Summary of return value");
  end Print_Function_Comment;

  procedure Print_Param_Comment(Name : String; Message : String) is
  begin
    Put_Line("--@param " & Name & " " & Message);
  end Print_Param_Comment;

  procedure Print_Procedure_Comment(Name : String) is
  begin
    Print_Comment("Summary of " & Name);
  end Print_Procedure_Comment;

  procedure Print_Procedure_Comment(Name : String; Param : Parameter) is
  begin
     Print_Comment("Summary of " & Name);
    Print_Param_Comment(To_String(Param.Name), "Summary of " & To_String(Param.Name));
  end Print_Procedure_Comment;

  procedure Print_Procedure_Comment(Name : String; Params : Parameter_Array) is
  begin
    Print_Comment("Summary of " & Name);
    for Param of Params loop
      Print_Param_Comment(To_String(Param.Name), "Summary of " & To_String(Param.Name));
    end loop;
  end Print_Procedure_Comment;

  procedure Print_Return_Comment(Message : String) is
  begin
    Put_Line("--@return " & Message);
  end Print_Return_Comment;

  procedure Print_Summary_Comment(Message : String) is
  begin
    Put_Line("--@summary " & Message);
  end Print_Summary_Comment;

  procedure Print_Value_Comment(Name : String; Message : String) is
  begin
    Put_Line("--@value " & Name & " " & Message);
  end Print_Value_Comment;

  procedure Print_Procedure(Name : String) is
  begin
    Print_Procedure(Name, False);
  end Print_Procedure;

  procedure Print_Procedure(Name : String; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Procedure_Comment(Name);
    end if;
    Put_Line("procedure " & Sanitized_Name & " is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Procedure;

  procedure Print_Procedure(Name : String; Param : Parameter) is
  begin
    Print_Procedure(Name, Param, False);
  end Print_Procedure;

  procedure Print_Procedure(Name : String; Param : Parameter; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Procedure_Comment(Name, Param);
    end if;
    Put_Line("procedure " & Sanitized_Name & "(" & To_String(Param.Name) & " : " & To_String(Param.Of_Type) & ") is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Procedure;

  procedure Print_Procedure(Name : String; Params : Parameter_Array) is
  begin
    Print_Procedure(Name, Params, False);
  end Print_Procedure;

  procedure Print_Procedure(Name : String; Params : Parameter_Array; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Procedure_Comment(Name, Params);
    end if;
    Put("procedure " & Sanitized_Name & "(");
    for I in 1 .. Params'Length - 1 loop
      Put(To_String(Params(I).Name) & " : " & To_String(Params(Params'Last).Of_Type) & "; ");
    end loop;
    Put_Line(To_String(Params(Params'Last).Name) & " : " & To_String(Params(Params'Last).Of_Type) & ") is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Procedure;

  procedure Print_Function(Form : Parameter) is
  begin
    Print_Function(Form, False);
  end Print_Function;

  procedure Print_Function(Form : Parameter; Stub_Comments : Boolean) is
  begin
    Print_Function(To_String(Form.Name), To_String(Form.Of_Type), Stub_Comments);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String) is
  begin
    Print_Function(Name, Returns, False);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Function_Comment(Name);
    end if;
    Put_Line("function " & Sanitized_Name & " return " & Returns & " is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Function;

  procedure Print_Function(Form : Parameter; Param : Parameter) is
  begin
    Print_Function(Form, Param, False);
  end Print_Function;

  procedure Print_Function(Form : Parameter; Param : Parameter; Stub_Comments : Boolean) is
  begin
    Print_Function(To_String(Form.Name), To_String(Form.Of_Type), Param, Stub_Comments);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String; Param : Parameter) is
  begin
    Print_Function(Name, Returns, Param, False);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String; Param : Parameter; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Function_Comment(Name, Param);
    end if;
    Put_Line("function " & Sanitized_Name & "(" & To_String(Param.Name) & " : " & To_String(Param.Of_Type) & ") return " & Returns & " is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Function;

  procedure Print_Function(Form : Parameter; Params : Parameter_Array) is
  begin
    Print_Function(Form, Params, False);
  end Print_Function;

  procedure Print_Function(Form : Parameter; Params : Parameter_Array; Stub_Comments : Boolean) is
  begin
    Print_Function(To_String(Form.Name), To_String(Form.Of_Type), Params, Stub_Comments);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String; Params : Parameter_Array) is
  begin
    Print_Function(Name, Returns, Params, False);
  end Print_Function;

  procedure Print_Function(Name : String; Returns : String; Params : Parameter_Array; Stub_Comments : Boolean) is
    Sanitized_Name : constant String := Sanitize_Name(Name);
  begin
    if Stub_Comments then
      Print_Function_Comment(Name, Params);
    end if;
    Put("function " & Sanitized_Name & "(");
    -- Iterate through all but the last parameter, which is printed differently
    for I in 1 .. Params'Length - 1 loop
      Put(To_String(Params(I).Name) & " : " & To_String(Params(Params'Last).Of_Type) & "; ");
    end loop;
    Put_Line(To_String(Params(Params'Last).Name) & " : " & To_String(Params(Params'Last).Of_Type) & ") return " & Returns & " is");
    Put_Line("begin");
    New_Line;
    Put_Line("end " & Sanitized_Name & ";");
  end Print_Function;

end Agen;
