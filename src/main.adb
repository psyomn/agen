with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GnatGen; use GnatGen;
with Argument_Stack; use Argument_Stack;

-- Entry point for this command line application.
procedure Main is
   procedure Print_Usage is
   begin
      Put_Line("Use:");
      Put_Line("gnatgen <action> [params]");
      Put_Line("  new project   - to create a template of a new project");
      Put_Line("  <throwaway|t> - to create a quick hello world program");
      Put_Line("  <print|p> <fn|proc|cmm> funcname:returntype [param:type]+");
      Put_Line("                - to print generated code for functions, procedures, or comments");
      Put_Line("  <help>        - print this info");
   end Print_Usage;

begin

   if Argument_Stack.Is_Empty then
      Print_Usage;
      return;
   end if;

   -- Copy parameters
   --for ix in 1..Argument_Stack loop
   --   Parameters(ix) := To_Unbounded_String(Argument(Number => ix));
   --end loop;

   -- Add here any other possible commands
   --if Argument(Number => 1) = "new" then
   --   handle_new(Parameters);

   --elsif Argument (Number => 1) = "throwaway" Or
   --  Argument (Number => 1) = "t" then
   --   Handle_Throwaway;

   --elsif Argument(Number => 1) = "print" Or
   --  Argument(Number => 1) = "p" then
   --   Handle_Print(Parameters);

   --elsif Argument(Number => 1) = "help" Or
   --  Argument(Number => 1) = "h" Then
   --   Print_Usage;

   --else
   --   Print_Usage;

   --end if;
end Main;
