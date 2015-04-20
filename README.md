Space Attack
======================
Retro "Space Invaders" style arcade game made in Fortran.
-----------------------------------------------------------------------------------

Although Space Attack is playable it is still a work in progress. I have grand plans to include new enemies, weapons, waves, power-ups, and more!

##Motivation
  I decided it would be a great experience to try and re-produce the old arcade game Space Invaders in Fortran.  My initial Google searches suggested that Fortran is bad for making games.  I decided to attempt it anyway.  Throughout the project I ran into several setbacks that required learning more about Fortran and crafting creative solutions.

####Setbacks
1) Modular - I wanted the game to fit any size terminal

2) Real Time - The Fortran READ statement pauses computation

3a) Graphics 1 - "clear screen" command does not work in parallel with a READ statement

3b) Graphics 2 - Screen output blocky and jumpy

####Solutions
1) Modular - I utilized an array and kept all aspects in terms of variables

2) Real Time - Use OpenMP parallel programming to bypass this inherent limitation

3a) Graphics 1 - No longer clear screen and import C function which hides user input

3b) Graphics 2 - Create independent graphics file containing animation subroutines, each of which is 3 "frames" long

###Conclusion
  I've learned a tremendous amount about programming techniques and possibilities.  I've added parallel programming and compiling of multiple source files to my skill set.  Space Attack remains a work in progress waiting for more of my free time.


##Screenshots
![Image](<http://i.imgur.com/qNdxfxM.png>)
![Image](<http://i.imgur.com/a8E6ww0.png>)
![Image](<http://i.imgur.com/x0Sbp4n.png>)
*Screen shots were taken using the Cool-Retro-Term terminal, found at: https://github.com/Swordfish90/cool-retro-term

##Compile
Space Attack utilizes OpenMP to handle parallel tasks and is spread across multiple files.  It therefore requires a couple of extra steps to compile.

The easiest way to compile the game is to use "make".  Change to the directory containing the game files and type the following after the command prompt:

    make
  
    export OMP_NUM_THREADS=16
  
  Viola!  Now type "./Space_Attack" and enjoy =)
  
  
To compile manually instead, type the following after the command prompt:
  
    gfortran -fopenmp -c Space_Attack_v1_9.f90 primaries_sub.f90 graphics_sub.f90 lose_animation.f90 sys_keyin.c
  
    gfortran -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o lose_animation.o Space_Attack_v1_9.o
  
    export OMP_NUM_THREADS=16

Now type "./a.out" and enjoy!

*Note: you may copy and paste the above commands into your terminal instead of typing them manually.

*Note: Each time you open a new terminal you must type "export OMP_NUM_THREADS=16" before running the game. 
