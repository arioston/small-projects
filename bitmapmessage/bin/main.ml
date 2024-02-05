open Bitmapmessage

(*** 
  There are 68 periods along the top and bottom of this string:		 
  You can also copy and paste this string from    
  https://inventwithpython.com/bitmapworld.txt)	
*)

let bitmap = ("		
....................................................................		
   **************   *  *** **  *      ******************************		
  ********************* ** ** *  * ****************************** *		
 **      *****************       ******************************		
          *************          **  * **** ** ************** *		
           *********            *******   **************** * *		
            ********           ***************************  *		
   *        * **** ***         *************** ******  ** *		
               ****  *         ***************   *** ***  *		
                 ******         *************    **   **  *		
                 ********        *************    *  ** ***		
                   ********         ********          * *** ****		
                   *********         ******  *        **** ** * **		
                   *********         ****** * *           *** *   *		
                     ******          ***** **             *****   *		
                     *****            **** *            ********		
                    *****             ****              *********		
                    ****              **                 *******   *		
                    ***                                       *    *		
                    **     *                    *		
....................................................................")

let () =
  while true do
    print_endline "Bitmap Message, by Al Sweigart al@inventwithpython.com";
    print_endline "Enter the message to display with the bitmap.";
    let message = read_line () in

    if String.length message == 0 then exit 0;

    let mapMessageTo = Lib.translate message in

    print_endline (String.mapi mapMessageTo bitmap)
  done