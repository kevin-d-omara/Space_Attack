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
Space Attack utilizes OpenMP to handle parallel tasks and therefore requires a couple of extra steps to compile.
To compile, type the following after the command prompt (">"):
  >export OMP_NUM_THREADS=16
  
  >gfortran -fopenmp -c Space_Attack_v1_8.f90
  
  >gfortran -c sys_keyin.c
  
  >gfortran -c graphics_sub.f90

  >gfortran -c primaries_sub.f90
  
  >gfortran -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o Space_Attack_v1_6.o

Viola!  Type "./a.out" and enjoy =)

*Note: The first step must be done every time you open a new terminal, otherwise the program will not run properly.
