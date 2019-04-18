-- Copyright 2014-2019 Simon Symeonidis (psyomn)
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

package GnatGen.Project_Generator is

  procedure Generate_Project(Name : String);

  -- Make the contents of the GPR file
  function Make_GPR_Contents(Name : String) return String;

  -- Creates a simple hello world file
  function Make_Simple_Main_Contents return String;

  procedure Generate_Throwaway;
end GnatGen.Project_Generator;
