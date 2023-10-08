-------------------------------------------------------------------------------
-- Program     : adbt (Acronym DataBase Tool)                                --
-- Description : CLI tool to manage an SQLite database of acronyms.          --
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
   pragma Debug (Put_Line (Standard_Error, "DEBUG: build a 'release' version with:  gprclean && gprbuild -XBUILD=release"));
   pragma Debug (Put_Line (Standard_Error, "DEBUG: or re-build using Alire with:    alr build --release"));

   --  check for any command line flags to execute
   if Cmd_Flags.Command_Line_Flags_Exist then
      Set_Exit_Status (Success);
      pragma Debug (Put_Line (Standard_Error, "DEBUG: Exit with 'success'."));
      return; -- exit as flags found and executed
   else
      Put_Line (Standard_Error, "ERROR: no command line option selected. Exit.");
      Set_Exit_Status (Failure); -- failed as no database found
      pragma Debug (Put_Line (Standard_Error, "DEBUG: Exit with 'failure'."));
      return; -- exit as nothing provided to execute
   end if;

end AdbT;
