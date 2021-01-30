-------------------------------------------------------------------------------
-- Package     : DB_File_Stats                                               --
-- Description : Find the acronyms database file for use with the program.   --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
-- with Text_IO.Editing;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body DB_File_Stats is

   function Get_File_Size (Dbfile : String) return String is
      ---------------------------------------------------
      --  Get the size in bytes of a file name provided
      ---------------------------------------------------

      Dbfile_Size : File_Size;

   begin
      --  TODO : add exception handling
      --
      --  TODO : add thousands coma formating
      Dbfile_Size := (Size (Dbfile));
      return (Ada.Strings.Fixed.Trim (Dbfile_Size'Image, Ada.Strings.Left));

   exception
      when Ada.Directories.Name_Error =>
         --  error with provided file name
         New_Line (2);
         Put
           (Standard_Error,
            "ERROR: DB_File_Stats.Get_File_Size() NAME for: '");
         Put_Line (Standard_Error, Dbfile & "'.");
         return ("ERROR");
      when Constraint_Error =>
         --  error with provided file name
         New_Line (2);
         Put
           (Standard_Error,
            "ERROR: DB_File_Stats.Get_File_Size() SIZE for: '");
         Put_Line (Standard_Error, Dbfile & "'.");
         return ("ERROR");

   end Get_File_Size;

   function Get_Full_Directory (Dbfile : String) return String is
   -----------------------------------------------
   --  Get the full directory path of a filename
   -----------------------------------------------
   begin
      return Full_Name (Dbfile);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  error with provided file name
         New_Line (2);
         Put (Standard_Error, "ERROR: unable to find full path for: '");
         Put_Line (Standard_Error, Dbfile & "'.");
         return "UNKOWN";
   end Get_Full_Directory;

   function Get_File_Mod_Time (Dbfile : String) return String is
   -----------------------------------------------
   --  Get the modification time for the filename
   -----------------------------------------------
   begin
      return (Image (Modification_Time (Dbfile)));
   end Get_File_Mod_Time;

end DB_File_Stats;
