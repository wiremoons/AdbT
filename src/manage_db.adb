-------------------------------------------------------------------------------
-- Package     : Maange_DB                                                   --
-- Description : Package to manage acronyms SQLite database file.            --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

--
--  Example SQlite Database access from Ada. Ada Gem:
--  https://www.adacore.com/gems/gem-130-type-safe-database-api-part-2
--
--  Database columns:
--  Acronym|Definition|Description|Source
--

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.SQL.Sqlite;

package body Manage_DB is

   DB_Descr : GNATCOLL.SQL.Exec.Database_Description ;
   DB       : GNATCOLL.SQL.Exec.Database_Connection;


   function Set_SQLite_Handle (Dbfile : String) return Database_Connection is
   begin

      -- create a database descriptor with the provided path and file name
      DB_Descr := GNATCOLL.SQL.Sqlite.Setup (Dbfile);
      
      --  get a task specific database connection:
      --  DB := GNATCOLL.SQL.Exec.Get_Task_Connection (DB_Descr);
      --
      --  get a connection in non multi tasking app:
      DB := DB_Descr.Build_Connection;
      return DB;
         
   end Set_SQLite_Handle;


   function DB_Connected (DB : Database_Connection) return Boolean is
   begin
   
      if GNATCOLL.SQL.Exec.Check_Connection(DB) then
         return true;
      else
         Put_Line (Standard_Error,"ERROR: Connection to database failed.");
         return false;
      end if;
               
   end DB_Connected;


   function Get_SQLite_Version (DB : Database_Connection) return String is

      --  Get runtime SQLite version: sqlite3_libversion()
      --  The C version of call returns an 'Int' as per docs:
      --    https://www.sqlite.org/c3ref/libversion.html
      --
      Q : constant String :=
         "SELECT sqlite_version()";
      R : GNATCOLL.SQL.Exec.Direct_Cursor;

   begin
      pragma Debug
         (Put_Line
             (Standard_Error,
              "DEBUG: SQLite version check query: " & Q));

      if DB_Connected (DB) then
         --  check DB is actual connection
         R.Fetch (Connection => DB, Query => Q);
         return GNATCOLL.SQL.Exec.Value (R, 0);
         --  output the use fetched query result as first value in 'R'
      else
         return "UNKNOWN";
         --  no database connection - so version not known
      end if;
      
   end Get_SQLite_Version;


   procedure Run_DB_Query (DB : Database_Connection) is
      --  Q : constant GNATCOLL.SQL.SQL_Query :=
      --   GNATCOLL.SQL.SQL_Select ("Select * from Acronyms limit 5");
      --
      --  Q : constant String := "Select * from Acronyms limit 6";
      --
      --  Changed to use 'ifnull' to handle the return of any null database
      --  records to avoid crashes
      Q : constant String :=
         "Select rowid, ifnull(Acronym,''), " & "ifnull(Definition,''), " &
         "ifnull(Description,''), " &
         "ifnull(Source,'') from Acronyms limit 5";

      --  cursor that gets one row at a time
      R : GNATCOLL.SQL.Exec.Forward_Cursor;
      --  cursor that get all rows into memory immediatly R :
      --  GNATCOLL.SQL.Exec.Direct_Cursor;
   begin

      --  read query results into 'R'
      R.Fetch (Connection => DB, Query => Q);

      while GNATCOLL.SQL.Exec.Has_Row (R) loop
         Put ("ID:");
         Set_Col (15);
         Put_Line (GNATCOLL.SQL.Exec.Value (R, 0));
         Put ("ACRONYM:");
         Set_Col (15);
         Put ("'");
         Put (GNATCOLL.SQL.Exec.Value (R, 1));
         Put ("' is: '");
         Put_Line (GNATCOLL.SQL.Exec.Value (R, 2) & "'.");
         Put ("DESCRIPTION:");
         Set_Col (15);
         Put_Line (GNATCOLL.SQL.Exec.Value (R, 3));
         Put ("SOURCE:");
         Set_Col (15);
         Put ("'");
         Put_Line (GNATCOLL.SQL.Exec.Value (R, 4) & "'.");
         New_Line (1);
         GNATCOLL.SQL.Exec.Next (R);
      end loop;

   end Run_DB_Query;

end Manage_DB;
