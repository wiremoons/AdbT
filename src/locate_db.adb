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

   -----------------------------------------------
   --  Locate a DataBase file
   -----------------------------------------------
   function Get_DB_Filename
     (dbfile : in out Unbounded_String) return Boolean is

   -- Try to locate the database file name and path and return it as and
   -- Unbounded_String if found. Boolean is returned bases on success.
   begin
      -- check environment varialble "ACRODB" for a database file
      if Ada.Environment_Variables.Exists ("ACRODB") then
         dbfile :=
           (To_Unbounded_String
              (Ada.Environment_Variables.Value ("ACRODB", "")));
         pragma Debug
           (Put_Line
              (Standard_Error,
               "DEBUG: environment variable 'ACRODB' is: '" &
               To_String (dbfile) & "'"));
         -- check the database file actually exists as well...
         if Exists (To_String (dbfile)) then
            return True;
         end if;
         Put (Standard_Error, "ERROR: 'ACRODB' env file location given as: '");
         Put (Standard_Error, To_String (dbfile) & "' - ");
         Put_Line (Standard_Error, "but file location not found.");
      end if;

      -- check same directory as program executed in for filename 'acronyms.db'
      dbfile :=
        (To_Unbounded_String
           (Containing_Directory (Ada.Command_Line.Command_Name)));
      pragma Debug
        (Put_Line
           (Standard_Error,
            "DEBUG: Containing directory: '" & To_String (dbfile) & "'"));
      dbfile := dbfile & "/acronyms.db";
      pragma Debug
        (Put_Line
           (Standard_Error,
            "DEBUG: Contructed dbfile is: '" & To_String (dbfile) & "'"));
      -- check of contructed database filename exists?
      if Exists (To_String (dbfile)) then
         -- constructed filename for database file exists - return it
         return True;
      else
         -- reset 'dbfile' variable as contructed file name does not exist
         dbfile := Null_Unbounded_String;
         pragma Debug
           (Put_Line
              (Standard_Error, "DEBUG: No constructed database file found"));
         return False;
      end if;
   end Get_DB_Filename;

end Locate_DB;
