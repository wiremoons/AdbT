-------------------------------------------------------------------------------
-- Package     : Locate_DB                                                   --
-- Description : Find the acronyms database file for use with the program.   --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Locate_DB is

   function Get_DB_Filename (dbfile : in out Unbounded_String) return Boolean;

end Locate_DB;
