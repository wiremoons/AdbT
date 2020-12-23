-------------------------------------------------------------------------------
-- Package     : DB_File_Stats                                                   --
-- Description : Find the acronyms database file for use with the program.   --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

package DB_File_Stats is

   function Get_Full_Directory (Dbfile : String) return String;
   --  Get the full directory path of a filename

   function Get_File_Size (Dbfile : String) return String;
   --  Get the size in bytes of a file name provided

   function Get_File_Mod_Time (Dbfile : String) return String;
   --  Get the modification time for the file name providded

end DB_File_Stats;
