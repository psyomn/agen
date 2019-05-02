-- Copyright 2019 Patrick Kelly (entomy)
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

with Ada.Directories; use Ada.Directories;
with Agen; use Agen;
with Testing; use Testing;
with Testing.Directories; use Testing.Directories;
with Agen.Testing; use Agen.Testing;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure AgenTests is
begin
   Testing.Start("Agen");

   declare
      Param : Parameter;
   begin
      Is_Equal("Try_Parse(""name:string"")", Try_Parse("name:string", Param), True);
      Is_Equal("Parameter", Param, "name", "string");
   end;

   declare
      Param : Parameter;
   begin
      Is_Equal("Try_Parse(""name"")", Try_Parse("name", Param), False);
      Is_Equal("Parameter", Param, "name", "");
   end;

   declare
      Name : constant String := "dummy";
   begin
      Create_Project(Name);
      Directory_Exists(Name);
      Directory_Exists(Name & Directory_Separator & "obj");
      Directory_Exists(Name & Directory_Separator & "obj" & Directory_Separator & "debug");
      Directory_Exists(Name & Directory_Separator & "obj" & Directory_Separator & "release");
      Directory_Exists(Name & Directory_Separator & "src");
      Directory_Exists(Name & Directory_Separator & "bin");
      File_Exists(Name & Directory_Separator & Name & ".gpr");
      File_Exists(Name & Directory_Separator & "main.adb");
      Delete_Tree(Name); --Cleans up afterwards
   end;

   Testing.Stop;
end AgenTests;