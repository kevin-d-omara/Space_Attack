Space Attack
======================
Retro "Space Invaders" style arcade game made in Fortran.
-----------------------------------------------------------------------------------

Although Space Attack is playable it is still a work in progress. I have grand plans to include new enemies, weapons, waves, powerups, and more!

##Screenshots
![Image](<http://i.imgur.com/qNdxfxM.png>)
![Image](<http://i.imgur.com/a8E6ww0.png>)
![Image](<http://i.imgur.com/x0Sbp4n.png>)
*Screen shots were taken using the Cool-Retro-Term terminal, found at: https://github.com/Swordfish90/cool-retro-term

##Compile
Space Attack utilizes OpenMP to handle parallel tasks and therefore requires a couple of extra steps to compile.
To compile, type the following after the command prompt (">"):
  >export OMP_NUM_THREADS=16
  
  >gfortran -fopemp -c Space_Attack_v1_6.f90
  
  >gfortran -c sys_keyin.c
  
  >gfortran -c graphics_sub.f90
  
  >gfortran -fopenmp -c sys_keyin.c graphics_sub.o Space_Attack_v1_6.o

Viola!  Type "./a.out" and enjoy =)

*Note: The first step must be done every time you open a new terminal, otherwise the program will not run properly.
