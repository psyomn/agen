# AGEN

This is a small, and stupid tool for command line users programming in Ada. It
generates boilerplate code for you.

#### Building

To build the project, you need to go into the root directory, and locate the
gnatmake _(.gpr)_ file. Once you're there, run the following:

~~~~bash
gnatmake -P agen -p
~~~~
or
~~~~bash
gprbuild -P agen -p
~~~~

#### Commands

To create a new project template, you can run the following command:

~~~~bash
$ agen new project my-project
~~~~

The above will create the project with a template gnatmake file. There is a
provided hello world program that you can compile if you want to make sure that
everything is alright - so you can go ahead and compile the newly generated
project.

If you want to generate the code for a function:
~~~~bash
$ agen function funcname:string param1:string param2:int
~~~~

or the shorter equivalent:

~~~~bash
$ agen fn funcname:string param1:string param2:int
~~~~

Which will yield:
~~~~ada
-- @author simon
-- @param world
function hello(world : string) return string is
begin
end hello;
~~~~

Generating code for a procedure is similar:

~~~~bash
$ agen procedure myproc param1:int
~~~~

or the shorter equivalent:

~~~~bash
$ agen proc myproc param1:int
~~~~

and yield the following results:

~~~~ada
-- @author simon
-- @param param1
procedure myproc(param1 : int) is
-- Enter your contents here...
begin
end myproc;
~~~~

You can also quickly generate comments. For example if you want to document
a function that was not previously documented, you can enter the parameters
in the following command:

~~~~bash
$ agen comment function funcname:string param1:int param2:int
~~~~
or
~~~~bash
$ agen cmm fn funcname:string param1:int param2:int
~~~~

and will yield:

~~~~bash
-- @author simon
-- @param param1
-- @param param2
-- @return
~~~~
