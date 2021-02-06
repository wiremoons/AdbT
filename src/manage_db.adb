-------------------------------------------------------------------------------
-- Package     : Manage_DB                                                   --
-- Description : Package to manage acronyms SQLite database file.            --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

--
--  Example SQLite Database access from Ada. Ada Gem:
--  https://www.adacore.com/gems/gem-130-type-safe-database-api-part-2
--
--  Database table 'ACRONYMS' columns are:
--  rowid|Acronym|Definition|Description|Source
--

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.SQL.Sqlite;
with Ada.Directories;
with Ada.Integer_Text_IO;
-- use local packages
with Locate_DB;
with DB_File_Stats;

package body Manage_DB is

   DB_Descr : GNATCOLL.SQL.Exec.Database_Description;
   DB       : GNATCOLL.SQL.Exec.Database_Connection;
   Dbfile   : Unbounded_String := Null_Unbounded_String;

   procedure Show_DB_Info is
   -----------------------------------------
   --  Provide an overview of the database
   -----------------------------------------

   begin
      -- locate and get handle to the database file
      if Locate_DB.Get_DB_Filename (Dbfile) then
         DB := (Set_SQLite_Handle (To_String (Dbfile)));
      else
         Put_Line (Standard_Error, "ERROR: no database file found. Exit.");
         New_Line (1);
         return;
      end if;

      if DB_Connected (DB) then
         Put ("Database file name: '");
         Put (Ada.Directories.Simple_Name (To_String (Dbfile)));
         Put_Line ("'");
         Put ("Database full path: '");
         Put_Line (DB_File_Stats.Get_Full_Directory (To_String (Dbfile)) & "'");
         Put ("Database file size: '");
         Put_Line (DB_File_Stats.Get_File_Size (To_String (Dbfile)) & "' bytes");
         Put ("Database modified:  '");
         Put_Line (DB_File_Stats.Get_File_Mod_Time (To_String (Dbfile)) & "'");
         New_Line (1);
         Put_Line ("SQLite version: '" & Get_SQLite_Version (DB) & "'");
         Put_Line ("Total acronyms: '" & Get_Total_DB_Records (DB) & "'");
         Put_Line ("Last acronym entered: '" & Get_Last_Acronym (DB) & "'");
         New_Line(1);
      else
         Put_Line (Standard_Error, "ERROR: no database file found or unable to connect. Exit.");
         --  Set_Exit_Status (Failure); -- failed as no database found return;
      end if;

   end Show_DB_Info;

   procedure Run_DB_Query (DB_Search_String : String) is
      -----------------------------------------
      --  Run a search query on the database
      -----------------------------------------

      --  Q : constant GNATCOLL.SQL.SQL_Query :=
      --   GNATCOLL.SQL.SQL_Select ("Select * from Acronyms limit 5");
      --
      --  Q : constant String := "Select * from Acronyms limit 6";
      --
      --  Changed to use 'ifnull' to handle the return of any null database
      --  records to avoid crashes. The '?1' is the search param placeholder.
      Q : constant String :=
        "Select rowid, ifnull(Acronym,''), " & "ifnull(Definition,''), " &
        "ifnull(Description,''), " & "ifnull(Source,'') " &
        "from Acronyms where Acronym like ?1 COLLATE NOCASE ORDER BY Source";

      --  cursor that gets one row at a time
      R : GNATCOLL.SQL.Exec.Forward_Cursor;
      --  cursor that get all rows into memory immediately R :
      --  GNATCOLL.SQL.Exec.Direct_Cursor;
   begin

      -- locate and get handle to the database file
      if Locate_DB.Get_DB_Filename (Dbfile) then
         DB := (Set_SQLite_Handle (To_String (Dbfile)));
      end if;

      if DB_Connected (DB) then
         --  read query results into 'R' without parameter
         -- R.Fetch (Connection => DB, Query => Q);
         --
         --  read query results into 'R' includes one parameter designated by
         --  the '+' for SQL_Parameter in GNATCOLL.SQL.Exec
         R.Fetch
           (Connection => DB, Query => Q, Params => (1 => +DB_Search_String));

         if not Success (DB) then
            Put_Line
              (Standard_Error, "ERROR: search returned '" & Error (DB) & "'");
         end if;

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
      else
         Put_Line
           (Standard_Error,
            "ERROR: no database file found or unable to connect. Exit.");
         --  Set_Exit_Status (Failure); -- failed as no database found return;
      end if;

      -- check how many row were process in the loop above
      Put ("Search for '" & DB_Search_String & "' found '");
      Ada.Integer_Text_IO.Put (Processed_Rows (R), Width => 0);
      Put_Line ("' records.");

   end Run_DB_Query;

   -----------------------------------------------------------------------
   --  BELOW ARE PRIVATE
   -----------------------------------------------------------------------

   function Set_SQLite_Handle (Dbfile : String) return Database_Connection is
   -----------------------------------------
   --  Get a handle to the database file
   -----------------------------------------
   begin
      -- create a database descriptor with the provided path and file name
      DB_Descr := GNATCOLL.SQL.Sqlite.Setup (Dbfile);

      --  get a task specific database connection: DB :=
      --  GNATCOLL.SQL.Exec.Get_Task_Connection (DB_Descr);
      --
      --  get a connection in non multi tasking app:
      DB := DB_Descr.Build_Connection;
      return DB;
   end Set_SQLite_Handle;


   function DB_Connected (DB : Database_Connection) return Boolean is
   ------------------------------------------------
   --  Check for a connection to the database file
   ------------------------------------------------
   begin
      if GNATCOLL.SQL.Exec.Check_Connection (DB) then
         return True;
      else
         Put_Line (Standard_Error, "ERROR: Connection to database failed.");
         return False;
      end if;

   end DB_Connected;

   function Get_SQLite_Version (DB : Database_Connection) return String is
      ---------------------------------------------------
      --  Get the SQLite version used with the database
      ---------------------------------------------------
      --  Get runtime SQLite version: sqlite3_libversion() The C version of
      --  call returns an 'Int' as per docs:
      --    https://www.sqlite.org/c3ref/libversion.html
      --
      Q : constant String := "SELECT sqlite_version()";
      R : GNATCOLL.SQL.Exec.Direct_Cursor;

   begin
      pragma Debug
        (Put_Line (Standard_Error, "DEBUG: SQLite version check query: " & Q));

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

   function Get_Total_DB_Records (DB : Database_Connection) return String is
      -----------------------------------------------------
      --  Get the total acronyms records in the database
      -----------------------------------------------------
      --  Get runtime SQLite version: sqlite3_libversion() The C version of
      --  call returns an 'Int' as per docs:
      --    https://www.sqlite.org/c3ref/libversion.html
      --
      -- printf "%,d" adds thousands separator
      Q : constant String := "select printf('%,d', count(*)) from ACRONYMS";
      R : GNATCOLL.SQL.Exec.Direct_Cursor;

   begin
      pragma Debug
        (Put_Line (Standard_Error, "DEBUG: Total records count query: " & Q));

      if DB_Connected (DB) then
         --  check DB is actual connection
         R.Fetch (Connection => DB, Query => Q);
         return GNATCOLL.SQL.Exec.Value (R, 0);
         --  output the use fetched query result as first value in 'R'
      else
         return "UNKNOWN";
         --  no database connection - so version not known
      end if;

   end Get_Total_DB_Records;

   function Get_Last_Acronym (DB : Database_Connection) return String is
      -----------------------------------------------------
      --  Get the last acronym entered in the database
      -----------------------------------------------------
      --  Get runtime SQLite version: sqlite3_libversion() The C version of
      --  call returns an 'Int' as per docs:
      --    https://www.sqlite.org/c3ref/libversion.html
      --
      Q : constant String :=
        "SELECT Acronym FROM acronyms Order by rowid DESC LIMIT 1";
      R : GNATCOLL.SQL.Exec.Direct_Cursor;

   begin
      pragma Debug
        (Put_Line (Standard_Error, "DEBUG: Last acronym entered query: " & Q));

      if DB_Connected (DB) then
         --  check DB is actual connection
         R.Fetch (Connection => DB, Query => Q);
         return GNATCOLL.SQL.Exec.Value (R, 0);
         --  output the use fetched query result as first value in 'R'
      else
         return "UNKNOWN";
         --  no database connection - so version not known
      end if;

   end Get_Last_Acronym;

end Manage_DB;
