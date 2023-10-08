-------------------------------------------------------------------------------
-- Package     : Misc_Utils                                                  --
-- Description : Misc utilities to support the program.                      --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

package Misc_Utils is

   --function Convert_Epoch_From_Integer (Epoch : Positive) return String;
   --  Convert an Epoch to local time and return as a string

   function Convert_Epoch_String (Epoch : String) return String;
   --  Convert an epoch string to date and time and return as a string

end Misc_Utils;
