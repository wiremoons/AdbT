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
   --  Get a handle aligned to the database file and path 'DBfile'
   
   function DB_Connected (DB : Database_Connection) return Boolean;
   --  Check if the handle is valif and can connected or not
   
   procedure Run_DB_Query (DB : Database_Connection);
   --  Run a SQL query agains the database handle provided
   
   function Get_SQLite_Version (DB : Database_Connection) return String;
   --  Return the version of the SQLite database code being used
   
end Manage_Db;
