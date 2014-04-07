with GnatGen.Project_Generator; use GnatGen.Project_Generator;

package body GnatGen.Code_Generator is 

  function GPR(Name : String) return String is
  begin
    return Project_Generator.Make_GPR_Contents(Name);
  end GPR;

  function Main return String is 
  begin
    return Make_Simple_Main_Contents;
  end Main;

end GnatGen.Code_Generator; 

