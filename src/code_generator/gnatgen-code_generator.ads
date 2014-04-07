with Ada.Strings.Unbounded;

package GnatGen.Code_Generator is 

  package US renames Ada.Strings.Unbounded;

  function GPR(Name : String) return String;

  function Main return String;

end GnatGen.Code_Generator;

