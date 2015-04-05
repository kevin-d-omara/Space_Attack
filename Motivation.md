##Motivation
  I decided it would be a great experience to try and re-produce the old arcade game Space Invaders in Fortran.  My initial Google searches yielded only knowledge that Fortran is bad for making games.  I decided to attempt it anyways.  Throughout the project I ran into several setbacks which required me to learn more about programming and craft creative solutions.

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
  This project has taught me a tremendous amount of information about programming techniques and possibilities.  It broadened my skillset to include parallel programming and compiling of multiple source files, and also challenged me at every turn.  Ultimately this project remains a work in progress which I will tackle during my free time.
