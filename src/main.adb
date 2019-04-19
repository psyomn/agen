with Ada.Text_IO;      use Ada.Text_IO;
with Actions.Init;

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

   Success : Boolean := False;

begin

   -- Copy parameters
   --for ix in 1..Argument_Stack loop
   --   Parameters(ix) := To_Unbounded_String(Argument(Number => ix));
   --end loop;

   if not Success then
      Success := Actions.Init.Try_Act;
   end if;

   --elsif Argument (Number => 1) = "throwaway" Or
   --  Argument (Number => 1) = "t" then
   --   Handle_Throwaway;

   --elsif Argument(Number => 1) = "print" Or
   --  Argument(Number => 1) = "p" then
   --   Handle_Print(Parameters);

   --elsif Argument(Number => 1) = "help" Or
   --  Argument(Number => 1) = "h" Then
   --   Print_Usage;

   if not Success then
      Actions.Init.Help;
   end if;
end Main;
