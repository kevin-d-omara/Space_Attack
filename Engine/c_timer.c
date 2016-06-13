/* ctimer_()
  Fortran call: 
      CALL ctimer()   to pause for 1/10.5th of a second

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

#include <stdio.h>
#include <time.h>

ctimerfast_(timer)		/* 1/20 second */
{
   struct timespec tim, tim2;
   tim.tv_sec = 0;		/* seconds */
   tim.tv_nsec = 20000000L;	/* nanoseconds */

  if(nanosleep(&tim , &tim2) < 0 )   
   {
   }

}
