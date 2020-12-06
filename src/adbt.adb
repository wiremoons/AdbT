-------------------------------------------------------------------------------
-- Program     : adbt (Acronym DataBase Tool)                                --
-- Description : CLI tool to manage an SQLite database of acronyms.          --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- local packages below:
with Cmd_Flags;
with Locate_DB;
with Manage_DB;

procedure AdbT is

   Dbfile : Unbounded_String := Null_Unbounded_String;

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

   -- locate a database file
   if Locate_DB.Get_DB_Filename (Dbfile) then
      Put_Line ("Database filename: '" & To_String (Dbfile) & "'");
      Manage_DB.Run_DB_Query (To_String (Dbfile));
   else
      Put_Line (Standard_Error, "ERROR: no database file found. Exit.");
      Set_Exit_Status (Failure); -- failed as no database found
      return;
   end if;

   -- execute the application
   Put ("Program run complete.");
   Set_Exit_Status (Success);
   return; -- program execution completed
end AdbT;
