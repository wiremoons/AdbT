-------------------------------------------------------------------------------
-- Package     : Maange_DB                                                   --
-- Description : Package to manage acronyms SQLite database file.            --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

package Manage_Db is

   function Set_SQLite_Handle (Dbfile : String) return Database_Connection;
   procedure Run_DB_Query (DB : Database_Connection);
   function Get_SQLite_Version (DB : Database_Connection) return String;

end Manage_Db;
