-------------------------------------------------------------------------------
-- Package     : Cmd_Flags                                                   --
-- Description : Manage user provided CLI flags for the program.             --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Strings;      use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with Show_Version;
with Manage_Db;

package body Cmd_Flags is

   function Command_Line_Flags_Exist return Boolean is
      -------------------------------------------
      --  Parse and manage and command line flags
      -------------------------------------------

      --  GNAT.Command_Line variables and config
      Help_Option : aliased Boolean := False;
      Info_Option : aliased Boolean := False;
      -- Search_Option : aliased Boolean := False;
      Search_Option  : aliased String_Access;
      Version_Option : aliased Boolean := False;
      Config         : Command_Line_Configuration;

   begin
      --  define params for the 'help' option
      Define_Switch
        (Config, Help_Option'Access, Switch => "-h", Long_Switch => "--help",
         Help => "Show command line usage for application");
      --  define params for the database 'information' option
      Define_Switch
        (Config, Info_Option'Access, Switch => "-i", Long_Switch => "--info",
         Help => "Show SQlite database information for application");
      --  define params for the database 'search' option
      Define_Switch
        (Config, Search_Option'Access, Switch => "-s:",
         Long_Switch => "--search:", Argument => "'search string'",
         Help => "Search the SQLite database for the given acronym");
      --  define params for the 'version' option
      Define_Switch
        (Config, Version_Option'Access, Switch => "-v",
         Long_Switch => "--version", Help => "Show version details");
      --  Additional help message as first line of 'Display_Help()'
      Set_Usage
        (Config,
         Usage => "[switches]", -- override default: "[switches] [arguments]";
         Help  => "Program to manage SQLite database of acronyms.");

      --  cli flags parse using config and above defined switched
      Getopt (Config);

      --  check if 'version' was requested
      if Version_Option then
         Show_Version.Show;
         return True;
      end if;

      --  check if 'information' was requested
      if Info_Option then
         Manage_Db.Show_DB_Info;
         return True;
      end if;

      --  check if 'search' was requested
      if (Search_Option'Length > 0) then

         --  Debug only output for search string
         pragma Debug
           (Put_Line
              (Standard_Error,
               "DEBUG: Search string length: " &
               Integer'Image (Search_Option'Length)));
         pragma Debug
           (Put_Line
              (Standard_Error,
               "DEBUG: Search string content: " & Search_Option.all));
         --  call database search with search string
         Manage_Db.Run_DB_Query (Search_Option.all);
         return True;
      end if;

      --  check if 'help' was requested
      if Help_Option then
         Display_Help (Config);
         New_Line (1);
         return True;
      end if;

      --  no cli flags used : so display usage and return false
      Display_Help (Config);
      New_Line (1);
      return False;

   exception
      when Invalid_Switch =>
         New_Line (1);
         Put_Line
           (Standard_Error,
            "Exception caught: caused by the use of an invalid command line switch.");
         New_Line (1);
         Display_Help (Config);
         return True;
      when Invalid_Parameter =>
         New_Line (1);
         Put_Line
           (Standard_Error,
            "Exception caught: caused by the use of an invalid parameter to a command line switch.");
         New_Line (1);
         Display_Help (Config);
         return True;
      when Exit_From_Command_Line =>
         New_Line (1);
         Put_Line (Standard_Error, "Exit following display of help message.");
         return True;

   end Command_Line_Flags_Exist;

end Cmd_Flags;
