-------------------------------------------------------------------------------
-- Package     : Misc_Utils                                                  --
-- Description : Misc utilities to support the program.                      --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.IO_Exceptions;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
with Interfaces.C;             use Interfaces.C;

package body Misc_Utils is

   -- function Convert_Epoch_From_Integer (Epoch : Positive) return String is
   -- ----------------------------------------------------------
   -- --  Convert an Epoch to local time and return as a string
   -- ----------------------------------------------------------
   -- Epoch_Time : Integer; 1696344211
   -- My_Time    : Time;
   -- My_Time_2    : Time;
   -- Conv_Epoch : Integer;
   -- begin
   -- pragma Debug (Put_Line (Standard_Error, "[DEBUG] converted time: " & Image (Time)));
   -- Conv_Epoch := Integer'Value (Epoch);
   -- Put_Line ("Epoch integer is: " & Conv_Epoch'Image);
   -- My_Time_2 := To_Ada_Time (Interfaces.C.long'Value(Epoch));
   -- My_Time := To_Ada_Time (1696344211);
   -- My_Time := To_Ada_Time (Conv_Epoch);
   -- Put_Line ("Timestamp is: " & Image (My_Time_2));
   -- return Image (My_Time_2);
   -- end Convert_Epoch_From_Integer;

   function Convert_Epoch_String (Epoch : String) return String is
      ----------------------------------------------------------
      --  Convert an epoch value from a string (ie "1696344211")
      --  to a date and time value ("2023-10-03 14:43:31") and
      --  return as a string.
      ----------------------------------------------------------
      Ada_Time : Time;
   begin
      if Epoch'Length = 0 then
         return "NO RECORD";
      end if;
      pragma Debug (New_Line (1));
      pragma Debug (Put_Line (Standard_Error, "[DEBUG] received epoch value: " & Integer'Value (Epoch)'Image));
      pragma Debug
        (Put_Line (Standard_Error, "[DEBUG] 'C.long' converted epoch value: " & Interfaces.C.long'Value (Epoch)'Image));
      -- Obtain Ada.Calendar.Conversions.Time from converted epoch value
      Ada_Time := To_Ada_Time (Interfaces.C.long'Value (Epoch));
      pragma Debug (Put_Line (Standard_Error, "[DEBUG] converted timestamp: " & Image (Ada_Time)));
      return Image (Ada_Time);
   end Convert_Epoch_String;

end Misc_Utils;
