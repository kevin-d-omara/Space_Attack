Space Attack
======================
Retro "Space Invaders" style arcade game made in Fortran.
-----------------------------------------------------------------------------------

[install intructions at the end]

This is an arcade game which plays inside of a terminal.  Bonus points if you use the [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term), as seen in the screenshots.

##NOTE:
1) Due to Fortran limitations (or my lack of knowledge ...) you *must* press the [enter] key after every. single. keystroke.  This includes during the menu and the game.  Each time you wish to move, shoot, etc, press the appropriate key AND enter.

2) This game operates using parallel processing.  Therefore, each time you open up a new terminal and wish to play, you must first type

    export OMP_NUM_THREADS=2

##Controls (default):
Shoot

    W

Move Right

    D

Move Left

    A

######Special Ordinance
  
Scattershot

    1

Vaporizer

    2

Missile

    3


##Screenshots
![Image](<http://i.imgur.com/qNdxfxM.png>)
![Image](<http://i.imgur.com/a8E6ww0.png>)
![Image](<http://i.imgur.com/x0Sbp4n.png>)
*Screen shots were taken using the Cool-Retro-Term terminal, found at: https://github.com/Swordfish90/cool-retro-term

#How to Run the Game
Inside your terminal navigate to the unzipped folder 'Space_Attack-master/'.  Type the following at the command prompt:

    export OMP_NUM_THREADS=2
    
    ./Space_Attack.x

##Install Instructions (How to Compile)
The game comes pre-compiled.  If for some reason you wish to compile the game, do the following:

First, navigate to the unzipped folder 'Space_Attack-master/'.  Then choose a method below and type each line at the command prompt.

####Compile with Make

    make
  
    export OMP_NUM_THREADS=2

####Compile Manually
  
    gfortran -fopenmp -c Engine/Space_Attack.f90 Engine/menus.f90 Engine/primaries_sub.f90 Engine/sys_keyin.c Graphics/graphics_sub.f90 Graphics/lose_animation.f90
  
    gfortran -fopenmp Space_Attack.o menus.o primaries_sub.o sys_keyin.o graphics_sub.o lose_animation.o -o Space_Attack.x
  
    export OMP_NUM_THREADS=2

*Note: you may copy and paste the above commands into your terminal instead of typing them manually.

####REMINDER
- Each time you open a new terminal you *must* type "export OMP_NUM_THREADS=2" before running the game. 
