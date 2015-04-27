/* sys_keyin.c  This version works on _most_ Unix platforms 
  Fortran calls: 
      CALL SYS_KEYSET(1)   to set single-keystroke mode
      CALL SYS_KEYIN(KEY)  to get integer ASCII code for next key-stroke
                           e.g. 32 for space, 97 for "a" etc.
                           (Integer rather than character to cope with 
                           control characters etc.)
      CALL SYS_KEYSET(0)   to restore normal input mode
  Author:  Clive Page,  cgp@le.ac.uk, 1994-JUL-13

	NOTE: My use of this program is to suppress the visual input of the user
	      during game-time.  This makes for a clean visual flow.  Otherwise,
	      the screen jumps each time the user inputs a command.  I think the
	      implicit Fortran function FGET does something similar.  That will
	      be tested in the future.
*/


#include <stdio.h>
#include <termios.h>
sys_keyset_(int *mode)
{
  static struct termios termattr,saveattr;
  static tcflag_t save_flags;
  
  if(*mode != 0) 
  {
    tcgetattr(0,&termattr);
    saveattr=termattr;
    termattr.c_lflag&=~(ICANON);
    termattr.c_lflag&=~(ECHO); 
    termattr.c_cc[VMIN] = 1;
    termattr.c_cc[VTIME] = 0;
    tcsetattr(0,TCSADRAIN,&termattr);
  }
  else
  {  
    tcsetattr(0,TCSADRAIN,&saveattr);
  }
  return;
}


sys_keyin_(int *nextch)
{
  *nextch = getchar();
  return;
}


/* ctimer_()
  Fortran call: 
      CALL ctimer()   to pause for 1/12th of a second

   Author: Found on StackOverflow - http://stackoverflow.com/questions/7684359/using-nanosleep-in-c

	NOTE: This function allows the the game to be paused for fractions of a second without
		using system resources.  Fortran is only capable of pausing for an integer
		value of seconds.  The alternative method, calling two timers on loop, is *very*
		heavy on system resources. (Lowered cpu usage from ~24% to ~10% and most
		importantly allows for a consistent game speed between different computers.
*/


#include <stdio.h>
#include <time.h>

ctimer_()
{
   struct timespec tim, tim2;
   tim.tv_sec = 0;		/* seconds */
   tim.tv_nsec = 95238095L;	/* nanoseconds */

  if(nanosleep(&tim , &tim2) < 0 )   
   {
   }

}
