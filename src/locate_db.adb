-------------------------------------------------------------------------------
-- Package     : Locate_DB                                                   --
-- Description : Find the acronyms database file for use with the program.   --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Locate_DB is

   function Get_DB_Filename (Dbfile : in out Unbounded_String) return Boolean is
   -----------------------------------------------
   --  Locate a Database file
   -----------------------------------------------

   --  Try to locate the database file name given and update the passed in
   --  Unbounded_String if found. Boolean is returned based the success of
   --  finding the dbfile or not.

   begin

      --  check environment variable "ACRODB" for a database file
      if Ada.Environment_Variables.Exists ("ACRODB") then
         Dbfile := (To_Unbounded_String (Ada.Environment_Variables.Value ("ACRODB", "")));
         pragma Debug
           (Put_Line (Standard_Error, "DEBUG: environment variable 'ACRODB' is: '" & To_String (Dbfile) & "'"));

         if Exists (To_String (Dbfile)) then
            --  environment path for database file exists - return it
            return True;
         end if;

         Put (Standard_Error, "ERROR: 'ACRODB' env file location given as: '");
         Put (Standard_Error, To_String (Dbfile) & "' - ");
         Put_Line (Standard_Error, "but file location not found.");
      end if;

      --  check for filename 'acronyms.db' in the same directory as the program
      Dbfile := (To_Unbounded_String (Containing_Directory (Ada.Command_Line.Command_Name)));
      Dbfile := Dbfile & "/acronyms.db";
      pragma Debug (Put_Line (Standard_Error, "DEBUG: programs local 'dbfile' path is: '" & To_String (Dbfile) & "'"));

      if Exists (To_String (Dbfile)) then
         --  constructed filename for database file exists - return it
         return True;
      else
         --  reset 'dbfile' variable as constructed file name does not exist
         Dbfile := Null_Unbounded_String;
         pragma Debug (Put_Line (Standard_Error, "DEBUG: No programs local database file found"));
         return False;
      end if;
   end Get_DB_Filename;

end Locate_DB;
