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

--@summary Provides argument stack semantics
--@description Doesn't actually implement a stack, but exposes Ada.Command_Line as a stack, because those semantics are useful
package Argument_Stack with Preelaborate is

   -----------
   -- Stack --
   -----------
   --This is implemented as an "abstract state machine" or singleton because when would you actually have multiple arguments to parse?

   function Is_Empty return Boolean with Inline; --Whether the argument stack is empty.

   function Length return Natural with Inline; --Length of the stack, the amount of arguments in it.

   procedure Reset with Inline; --Reset the stack to its original state

   procedure Push_Back; --"Push" back onto the stack. This doesn't need a value because technically it just adjusts an index and the value was always there.

   function Pop return String; --Pop the value off the stack.

   function Pop_Remaining return String; --Pop the remaining items off the stack as one single string

private
   Current : Positive := 1;

end Argument_Stack;