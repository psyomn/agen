with Ada.Text_IO; use Ada.Text_IO;
with Actions.Comment;
with Actions.Func;
with Actions.Init;
with Actions.Proc;

-- Entry point for this command line application.
procedure Main is
   Success : Boolean := False;
begin
   if not Success then
      Success := Actions.Comment.Try_Act;
   end if;
   if not Success then 
      Success := Actions.Func.Try_Act;
   end if;
   if not Success then
      Success := Actions.Init.Try_Act;
   end if;
   if not Success then
      Success := Actions.Proc.Try_Act;
   end if;
   if not Success then --Whatever was entered wasn't understood, so print all the help
      -- Part of the beauty of this approach is it handles --help for us without any code
      Actions.Comment.Help;
      Actions.Func.Help;
      Actions.Init.Help;
      Actions.Proc.Help;
   end if;
exception
   when others =>
      Put_Line("Looks like something bad happened. Please consider filing a bug report, including the command you wrote, and the exception that printed out, to https://github.com/psyomn/gnatgen/issues");
      raise;
end Main;
