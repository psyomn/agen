# GNATGEN

This is a small, and cute tool for command line users programming in Ada. It
generates boilerplate code for you.

#### Building 

To build the project, you need to go into the root directory, and locate the
gnatmake _(.gpr)_ file. Once you're there, run the following:

~~~~bash
gnatmake -P gnatgen -p
~~~~

#### Commands

To create a new project template, you can run the following command: 

~~~~bash
$ gnatgen new project my-project
~~~~

The above will create the project with a template gnatmake file. There is a 
provided hello world program that you can compile if you want to make sure that
everything is alright - so you can go ahead and compile the newly generated
project.

If you want to generate the code for a function: 
~~~~bash
$ gnatgen print function funcname:string param1:string param2:int
~~~~

or the shorter equivalent:

~~~~bash 
$ gnatgen p fn funcname:string param1:string param2:int
~~~~

Which will yield:
~~~~ada
-- @author simon
-- @param world
-- @date 2014-04-16 (iso)
function hello(world : string) return string is 
-- Enter your contents here...
begin
end hello;
~~~~

Generating code for a procedure is similar: 

~~~~bash
$ gnatgen print procedure myproc param1:int
~~~~

or the shorter equivalent: 

~~~~bash
$ gnatgen p proc myproc param1:int
~~~~

and yield the following results: 

~~~~ada
-- @author simon
-- @param param1
-- @date 2014-04-16 (iso)
procedure myproc(param1 : int) is 
-- Enter your contents here...
begin
end myproc;
~~~~

Notice that you don't need to specify a return for the proc, in the command. 
You could provide one, but it would be ignored. 

You can also quickly generate comments. For example if you want to document
a function that was not previously documented, you can enter the parameters
in the following command: 

~~~~bash
$ gnatgen p cmm param1:int param2:int
~~~~

and yield:

~~~~bash
-- @author simon
-- @param param1
-- @param param2
-- @date 2014-04-16 (iso)
~~~~

#### Non-system wide install

That will create the binary and place it in _bin/_. If you want to have this
tool handy, but don't want a system-wide install you can do the following hack.

Create a directory called `.custom` in your home directory such that
_$HOME/.custom/_ is a valid path. Inside custom, create a _bin/_ dir.

Now in your bash profile _(.bash__profile)_ add the following lines:

~~~~bash
export PATH=$PATH:$HOME/.custom/bin/
~~~~

This means that your shell will look into that directory as well when you're
trying to execute some tool. 

Finally copy the _gnatgen_ binary into the _.custom/bin_ directory.

This is not something that only works for _gnatgen_; you can stick any other
binary or executable file/script in there as you see fit.


