-- Project generator module
-- @author psyomn

package GnatGen.Project_Generator is

  procedure Generate_Project(Name : String);

  -- Make the contents of the GPR file
  function Make_GPR_Contents(Name : String) return String;

  -- Creates a simple hello world file
  function Make_Simple_Main_Contents return String;

  procedure Generate_Throwaway;
end GnatGen.Project_Generator;
