-------------------------------------------------------------------------------
-- Package     : Maange_DB                                                   --
-- Description : Package to manage acronyms SQLite database file.            --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

package Manage_Db is

   procedure Show_DB_Info;
   --  Provide an overview of the database

   procedure Run_DB_Query;
   --  Run a SQL query against the database handle provided

   function Set_SQLite_Handle (Dbfile : String) return Database_Connection;
   --  Get a handle aligned to the database file and path 'DBfile'

   function DB_Connected (DB : Database_Connection) return Boolean;
   --  Check if the handle is valid and can access the DB or not

   function Get_SQLite_Version (DB : Database_Connection) return String;
   --  Return the version of the SQLite database code being used

   function Get_Total_DB_Records (DB : Database_Connection) return String;
   --  Return the total number of records held in the database

   function Get_Last_Acronym (DB : Database_Connection) return String;
   --  Return the last acronym entered into the database

end Manage_Db;
