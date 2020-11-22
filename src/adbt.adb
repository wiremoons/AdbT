-------------------------------------------------------------------------------
-- Program     : adbt (Acronym DataBase Tool)                                --
-- Description : CLI tool to manage an SQLite database of acronyms.           --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
-- local packages below:
with Cmd_Flags;

procedure AdbT is

--------------------------
-- MAIN
--------------------------
begin

   --  print info on how to compile a 'release' version
   pragma Debug
     (Put_Line
        (Standard_Error,
         "DEBUG: build a 'release' version with: gprclean && gprbuild -XBUILD=release"));

   --  check for any user provided command line flags
   if Cmd_Flags.Command_Line_Flags_Exist then
      Set_Exit_Status (Success);
      return; -- exit as flags found and executed
   end if;

   -- execute the application
   Put ("Program run complete.");
   Set_Exit_Status (Success);
   return; -- program execution completed
end AdbT;
