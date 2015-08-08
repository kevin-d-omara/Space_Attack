Space Attack
======================
Retro "Space Invaders" style arcade game made in Fortran.
-----------------------------------------------------------------------------------

[install intructions at the end]

This is an arcade game which plays inside of a terminal.  Bonus points if you use the Cool Retro Term (https://github.com/Swordfish90/cool-retro-term)

##NOTE:
Due to Fortran limitations (or my lack of knowledge ...) you *must* press the [enter] key after every. single. keystroke.  This includes during the menu and the game.  So, if you want to move or shoot, press the appropriate key AND enter.

#Controls (default):
Shoot - 'W'

Move Right - 'D'

Move Left - 'A'

  -- Special Ordinance --
  
Scattershot - 1

Vaporizer - 2

Missile - 3


##Screenshots
![Image](<http://i.imgur.com/qNdxfxM.png>)
![Image](<http://i.imgur.com/a8E6ww0.png>)
![Image](<http://i.imgur.com/x0Sbp4n.png>)
*Screen shots were taken using the Cool-Retro-Term terminal, found at: https://github.com/Swordfish90/cool-retro-term

##Compile
Space Attack utilizes OpenMP to handle parallel tasks and is spread across multiple files.  It therefore requires a couple of extra steps to compile.

The easiest way to compile the game is to use "make".  Change to the directory containing the game files and type the following after the command prompt:

    make
  
    export OMP_NUM_THREADS=2
  
  Viola!  Now type "./Space_Attack" and enjoy =)
  
  
To compile manually instead, type the following after the command prompt:
  
    gfortran -fopenmp -c Space_Attack.f90 primaries_sub.f90 graphics_sub.f90 lose_animation.f90 menus.f90 sys_keyin.c
  
    gfortran -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o lose_animation.o menus.o Space_Attack.o
  
    export OMP_NUM_THREADS=2

Now type "./a.out" and enjoy!

*Note: you may copy and paste the above commands into your terminal instead of typing them manually.

*Note: Each time you open a new terminal you must type "export OMP_NUM_THREADS=2" before running the game. 
