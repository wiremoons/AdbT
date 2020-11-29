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
with GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;

package body Manage_DB is

   procedure Run_DB_Query (dbfile : String) is

      DB_Descr : constant GNATCOLL.SQL.Exec.Database_Description :=
        GNATCOLL.SQL.Sqlite.Setup (dbfile);
      DB : GNATCOLL.SQL.Exec.Database_Connection;

   begin

      --  get a task specific connection DB :=
      --  GNATCOLL.SQL.Exec.Get_Task_Connection (DB_Descr); get a connection
      --  in non multi tasking app
      DB := DB_Descr.Build_Connection;

      declare
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
      end;

   end Run_DB_Query;

end Manage_DB;
