##Motivation

  This semester was my first time learning to program and from day one I was intrigued by the process.  The day we learned about IF statements I realized so many cool things that could be done with Fortran.  I had an "a-ha!" moment that day and said to myself, "I'm going to make the old arcade game Space Invaders in Fortran!"  I told my friends and the teacher, but I was quickly surprised by the lack of support.  Everyone, teacher included, told me that Fortran was not for making games and that it was either impossible or would take me all summer.  I resolved to do my best anyways and that night I sat down to write some code!

####First Taste
  The first night was a flurry of ideas and I programmed for about 6 hours.  At the end I had a very minimalistic prototype which I presented the next class session.  That first burst of programming gave me a taste of what real, complicated projects felt like to work on.  Open ended, complicated tasks, completely up to the programmer.  I loved it!
  
  One of my goals throughout the project has been to keep everything modular.  That is, to be able to alter the size of the play field at any time without having to re-code anything.  This led me to learn about arrays, which we still haven't covered in class yet as of 04/06/15.
  
####Major Setback
  After updating my code to use arrays I ran into the next problem.  I wanted my game to be played real time.  This means even if the player does not enter any commands the Invaders still encroach upon the Player.  This was by far the largest setback in my game.  The probelm lies inherently within the way Fortran records user input.  The player enters a command which is read by the READ statement.  When Fortran computes a READ statement it completely pauses until the user hits enter.  This means if the user does not hit enter then the rest of the program cannot continue.
  
  I brainstormed several ideas to overcome this obstacle and each one of them failed to one extent or another.  Ultimately I learned about something called OpenMP.  OpenMP is a way of running parts of a program in parallel with one another.  Although I was unsure if using parallel programming would solve my problem I decided to venture forth and try anyways.  This required me dredging through a large manual and learning OpenMP from the ground up.  After working through nearly 50 pages I discovered what I needed!
  
####Success! (for the moment)
  My wild plan worked!  By utilizing parallel programming in an unorthodox way I unlocked the key to real time action!  Now the game was fundamentally changed for the better.  Real time action adds pressure and constancy which make the game exciting.
  
  Unfortunately, as a result of parallel programming I was unable to print graphics in the same way as before.  Previously I cleared the screen before printing each new frame in order to anchor all visuals in the same location.  With parallel programming I could no longer clear the screen.
  
  I found the only way to fix this issue, and retain usability, was to import a function from C into my program.  This opened up new doors because I was unaware of the ability to compile multiple source files, yet alone languages, together.  So far this small imported C function is the only piece of code which is not original from me.  All other aspects of Space Attack were completely grown from my mind.
  
####Graphics ♅ ☋ ❈
  Still, though, the game felt lacking.  The visuals were very clunky and far less than stellar.  I envisioned a way to create lively animations which involved calling many individual subroutines.  Having just learned about compiling multiple source files together I decided to make the graphics an independent file.
  
####Wave 2+, the Final Frontier
  After implementing cool new graphics the game reached a major milestone.  Playability and presentability.  At this point Space Attack is finally presentable and actually fun.  All that remains now is to add new Waves, Invader types, Projectile types, and Powerups!
  
####Conclusion
  What began innocuously as a simple idea, and about 200 lines of code, has grown into quite the project.  Now weighing in at over 800 lines of code, Space Attack has become a beautiful piece of work.  I fully intend to continue working on this project in my free time until one day I can call it completed.
