!
PROGRAM array_invaders
!
! Purpose:
!    This began as an attempt to recreate the classic game Space Invaders, made by Tomohiro Nishikado and released in 1978.
!	Instead this has organically grown into a different game, and is still evolving as such.
!
! To Compile:
!	>export OMP_NUM_THREADS=16	(Must do this step every time you open a new terminal!)
!	>gfortran -fopenmp -c Space_Attack_v1_6.f90
!	>gfortran -c sys_keyin.c
!	>gfortran -c graphics_sub.f90
!	>gfortran -fopenmp sys_keyin.o graphics_sub.o Space_Attack_v1_6.o
!	>./a.out	=) enjoy!
!
! History:
!     Version   Programmer	Date		Description
!     -------   ----------	--------	-----------
!	1.0	K. O'Mara	02/25/15	created
!	1.1	K. O'Mara	02/26/15	finished COMMANDS
!	1.2	K. O'Mara	03/07/15	updated to ARRAY!!
!	1.3	K. O'Mara	03/08/15	finished updating to array, added variable commands, and attempted a spawning algorithm
!	1.4	K. O'Mara	04/01/15	ADDED PARALLEL PROGRAMMING !$OMP REAL TIME!!!! and converted to subroutines
!	1.5	K. O'Mara	04/03/15	put graphical border around play area
!	1.6	K. O'Mara	04/04/15	made graphics have 3 frames (i.e. smoother)
!						added win condition (finally!) and pre-game terminal size fit
!	1.6.1	K. O'Mara	04/05/15	added new comments to make more presentable
!
!
! GOTO Statements:
!	Number	GOTO
!	------	-----
!	10	Beginning of COMMANDS
!	20	Beginning of Determine Controls
!	30	End rowfinder spawn
!	40	End MOVE RIGHT sequence
!	50	Game Timer
!	60	Enter Command Loopback
!	70	Print Screen
!	80	Exit Win Condition Loop
!	66	Exit Command Loop
!	666	Exit Main Loop (You Lose)
!	6666	Loss Condition within [Movement]
!	777	Restart
!	888	Exit Main Loop (after WRITE statement)
!	999	Exit Main Loop (You Win)
!
! FORMAT Statements:
!	Number	FORMAT
!	------	-----
!	9000	Print Screen Formatting [Main]
!	9001	Print Screen Formatting [Graphics]
!	7000	Print Player [Shoot]
!	7001	Print Player [Right]
!	7002	Print Player [Left]
!	6000	Print shots fired and kills [loser]
!	6001	Print accuracy [loser]
!	6002	Print score [loser]
!	6003	Print score [printscreen]
!
!
! ------------------ Variable declarations -----------------------------

	IMPLICIT NONE
	INTEGER, PARAMETER :: row=19, col=15	!#rows must be = 4N+3 for code to work // (X-3)/4=N
	INTEGER :: endgame, x00, i, j, k, h, flip, yesno, frame, rl, numcol, charge, updown, shots, kills, hits, score, wincon, wave
	INTEGER, DIMENSION (row,col) :: x
	REAL :: start, finish
	CHARACTER(1) ::	command, shoot, right, left, gcommand	!C(LEN=1) == C(1) == C

!------------------------------------------------------------------------------------------!
!----------BEGIN GAME----------BEGIN GAME----------BEGIN GAME----------BEGIN GAME----------!
!------------------------------------------------------------------------------------------!

	777 CALL controls(shoot,right,left,yesno,i,j,row,col)		!Determine Controls and Terminal Size
	CALL initialize(x,x00,i,j,row,col,endgame,frame,charge,updown,shots,hits,kills,score,wave)	!Initialize Positions and Variables
		call printscreen(i,j,row,col,x,flip,rl,gcommand,left,right,shoot,frame,numcol,charge,updown,score,wave)

!This parallel region has two main sections.  The first is the bulk of the game and takes care of all the action.  The second allows the
!  player to enter commands at will.  This is entirely necessary as Fortran pauses computation at a READ statement until the user hits enter.
!  By running these two sections in parallel the game is able to be real time and continue even if the user does not enter any commands.

!$OMP PARALLEL
	!$OMP SECTIONS
	!$OMP SECTION
	!$OMP SECTION
		10 CALL movement(x00, i, j, k, h, flip,x,row,col,endgame,kills,hits,score,wincon)		!Movement/Collisions
		   IF (endgame==1) THEN		!Game On!
			CALL laser(i,j,row,col,x)								!Laser Movement
			CALL commands(command,shoot,left,right,j,row,col,x,gcommand,charge,updown,shots)	!Player's Command
			70 CALL printscreen(i,j,row,col,x,flip,rl,gcommand,left,right,shoot,frame,numcol&
						,charge,updown,score,wave)					!Print Screen
				IF (frame<3) THEN				!This is where the 3 frame animation comes from.
					CALL game_timer(start,finish)		!Timer to allow a pause between frames.
					frame=frame+1
					GOTO 70	!printscreen again
				ELSE
					CALL game_timer(start,finish)		!Timer
					frame=1; !reset frame count
					GOTO 10	 !Loop back to Movement/Collisions
				END IF	
		   ELSE IF (endgame==0) THEN
			WRITE(*,*) 'Game over man, game over! (hit any key to continue)'	!EXIT LOOP
		   ELSE		!endgame==9
			WRITE(*,*) "You did it, you killed them all! (hit any key to continue)"	!EXIT LOOP
		   END IF		
	!$OMP SECTION
		60 CALL enter_command(command)		!Repetedly lets the user enter a command
		IF (endgame==1) GOTO 60			!endgame=1 means the game is still going (no win or lose)
				!Else EXIT LOOP
	!$OMP END SECTIONS
!$OMP END PARALLEL

IF (endgame==0) THEN		!endgame=0 means You Lose
	CALL write_lose_sequence(yesno,kills,shots,hits,score)	!Lose Sequence
	command='p'	!if the player decides to play again this prevents a pre-set first command
		IF (yesno==1) GOTO 777
ELSE				!otherwise endgame=9 and You Win
	CALL write_win_sequence(yesno,kills,shots,hits,score)	!Win Sequence
	command='p'	!if the player decides to play again this prevents a pre-set first command
		IF (yesno==1) GOTO 777
END IF

END PROGRAM array_invaders



!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
	!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
		!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
			!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
		!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
	!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------
!-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------SUBROUTINES-------



!--------Controls--------Controls--------Controls-------Controls--------Controls--------Controls-------Controls-------
SUBROUTINE controls(shoot,right,left,yesno,i,j,row,col)	!This is the very first subroutine called. It allows the user to
	IMPLICIT NONE					!appropriately resize their terminal and then set ther controls.
	CHARACTER(1) :: shoot, right, left		!If the terminal is too tall (width is irrelevant) then the player
	INTEGER :: yesno, i, j				!can "see into the past" (see the previous frame).
	INTEGER, intent(in) :: row, col
							
call execute_command_line('clear')	!Clear screen before printing
!All of these dumb "&" signs are the only way to get past the compiler, it thinks the lines are to long otherwise
		WRITE(*,9000) '┌─────────────────────────────&
		&──────────────────┐'
		WRITE(*,9000) '│       ✭✭✭ Welcome to Space A&
		&ttack! ✭✭✭        │'
		WRITE(*,9000) '│                             &
		&                  │'
		WRITE(*,9000) '│         ♅ ♅ ♅   ♃  ♄  ☫  &
		&♇  ☿  ♅ ♅ ♅          │'
		WRITE(*,9000) '│                             &
		&                  │'
		WRITE(*,9000) '│  Please adjust the height of&
		& your terminal to │'
		WRITE(*,9000) '│              fit the border &
		&now.              │'
		WRITE(*,9000) '│                             &
		&                  │'
		WRITE(*,9000) '│          Press any key to co&
		&ntinue            │'
		DO i=1, row-5		!Fills in empty rows
			WRITE(*,9000,advance='no') '│ '
			DO j=1,col	!Fills in empty columns
				call space()
			END DO
			WRITE(*,9000) ' │'
		END DO
		WRITE(*,9000) '└─────────────────────────────&
		&──────────────────┘'
		READ(*,*) 
20 WRITE(*,*) 'Please enter your commands. W,D,A are recommended.'
WRITE(*,*) 'Also note that in-game you must hit enter after each command.'
WRITE(*,*) ''
WRITE(*,*) 'Please enter a command to shoot (1 character).'		!W is recommended
READ(*,*) shoot
WRITE(*,*) 'Please enter a command to move right (1 character).'	!D is recommended
READ(*,*) right
WRITE(*,*) 'Please enter a command to move left (1 character).'		!A is recommended
READ(*,*) left
WRITE(*,*) 'Your commands are:'
WRITE(*,*) 'Shoot  ==  ', shoot
WRITE(*,*) 'Right  ==  ', right
WRITE(*,*) 'Left   ==  ', left
WRITE(*,*) ''
WRITE(*,*) ''
WRITE(*,*) 'Re-enter commands? Yes=1 No=0'
READ(*,*) yesno
IF (yesno==1) THEN
	call execute_command_line('clear')
	GOTO 20
ELSE
END IF

9000 FORMAT(A)	!To allow the advance=no clause
!NOTE: even for 'unformatted' statements this is needed, otherwise spacing is different

RETURN
END SUBROUTINE controls

!--------Initialize positions--------Initialize Positions--------Initialize positions--------Initialize Positions--------
SUBROUTINE initialize(x,x00,i,j,row,col,endgame,frame,charge,updown,shots,hits,kills,score,wave)
	IMPLICIT NONE
	INTEGER :: x00, i, j, row, col, endgame, frame, charge, updown, shots, hits, kills, score, wave
	INTEGER, DIMENSION (row,col) :: x
									!This subroutine initializes all relevant variables
endgame=1	!1 = game on!
x=0		!empty matrix
x00=0		!empty spawn
x(19,8)=4	!spawn you :)
frame=1		!set first frame count for graphics
 charge=3	!set at max weapon charge
shots=0		!set to 0 shots fired
hits=0		!set to 0 hits
kills=0		!set to 0 kills
updown=0	!set to idle charge animation
score=0		!set score to 0 points
wave=1		!set to wave 1

DO i=3,7,2			!Spawns the Wave 1 Invaders!
	DO j=5,11
		x(i,j)=6
	END DO
END DO

	!Print initial position
CALL execute_command_line('clear')	!Clear screen before printing

RETURN
END SUBROUTINE initialize

!-------Movement-------Movement-------Movement-------Movement-------Movement-------Movement-------Movement-------
SUBROUTINE movement(x00, i, j, k, h, flip,x,row,col,endgame,kills,hits,score,wincon)
	IMPLICIT NONE
	INTEGER :: x00, i, j, k, h, flip, row, col, endgame, kills, hits, score, wincon
	INTEGER, DIMENSION (row,col) :: x
						!This subroutine handles the bulk of all the action: Movement and Collisions
!Check for loss condition
IF (x((row-2),col)==6) THEN	!Check final Invader position (bottom right)
	endgame=0
	GOTO 6666		!jump to end of this subroutine
END IF

!Check for win condition
wincon=0	!reset win condition counter
DO i=1,(row-2),2	!scale rows
	DO j=1,col	!scale columns
		wincon=wincon+x(i,j)	!add value of Invader in the space being checked
	END DO
END DO
IF (wincon==0) THEN	!check if total value of all Invader spaces = 0 (i.e. no Invaders left)
	endgame=9; GOTO 6666	!endgame=9 means You Win!!
END IF
	!In essence this section moves the Invaders starting with the final position and snaking its way back to the first position
									!It also handles collisions between Laser and Invader
!BEGIN MOVEMENT/COLLISIONS----------------------------------------------!Note that 1=Laser, 6=Invader, and 4=Player
flip=1		!flip determines odd/even; 1=odd, 0=even

DO h=1, (row-2), 2	!BEGIN scale rows; steps of 2 (evens reserved for lasers)
   DO k=1, col		!BEGIN scale columns
	i=row-1-h	!scale rows backwards

	IF (flip==1) THEN	!ODD or EVEN? flip=1=ODD

		j = col+1-k	!reverses order to maintain proper movement flow

		!SPAWN sequence
	   	IF ((i==1).AND.(j==1)) THEN	!Check for x(1,1) (i.e. spawn)
			IF (x00+x(2,1)<3) THEN		!x00 (not part of the array; invisible)
			ELSE IF (x00+x(2,1)>6) THEN			!Destroy
				x00=0; x(2,1)=0
				kills=kills+1; hits=hits+1; score=score+10
			ELSE
				x(1,1)=x00+x(2,1); x00=0; x(2,1)=0	!Damage
				!hits=hits+1	!LEAVE OF FOR NOW, THIS INCLUDES [6] [0] WHICH IT SHOULD NOT!!
			END IF
	  	ELSE		!ELSE run normal ODD sequence

			!ODD action sequence
			IF (j==1) THEN	!Determine 1st column or not

				IF (x((i-2),j)+x((i+1),j)<3) THEN	!Skip
				ELSE IF (x((i-2),j)+x((i+1),j)>6) THEN	!Destroy
					x((i-2),j)=0; x((i+1),j)=0
					kills=kills+1; hits=hits+1; score=score+10
				ELSE				!Damage
					x(i,j)=x((i-2),j)+x((i+1),j); x((i-2),j)=0; x((i+1),j) =0
					!hits=hits+1
				END IF			!END last column

			ELSE		!Otherwise move right to left

				IF (x(i,(j-1))+x((i+1),j)<3) THEN	!Skip
				ELSE IF (x(i,(j-1))+x((i+1),j)>6) THEN	!Destroy
					x(i,(j-1))=0; x((i+1),j)=0
					kills=kills+1; hits=hits+1; score=score+10
				ELSE					!Damage
					x(i,j)=x(i,(j-1))+x((i+1),j); x(i,(j-1))=0; x((i+1),j) =0
					!hits=hits+1

				END IF			!End right to left

			END IF		!END 1st column or not

	 	 END IF		!END spawn sequence
	ELSE
		!EVEN action sequence
		j = k		!maintains direction
		IF (j==col) THEN	!Determine last column or not

			IF (x((i-2),j)+x((i+1),j)<3) THEN	!Skip
			ELSE IF (x((i-2),j)+x((i+1),j)>6) THEN	!Destroy
				x((i-2),j)=0; x((i+1),j)=0
				kills=kills+1; hits=hits+1; score=score+10
			ELSE				!Damage
				x(i,j)=x((i-2),j)+x((i+1),j); x((i-2),j)=0; x((i+1),j) =0
				!hits=hits+1
			END IF			!END last column

		ELSE		!Otherwise move left to right

			IF (x(i,(j+1))+x((i+1),j)<3) THEN	!Skip
			ELSE IF (x(i,(j+1))+x((i+1),j)>6) THEN	!Destroy
				x(i,(j+1))=0; x((i+1),j)=0
				kills=kills+1; hits=hits+1; score=score+10
			ELSE				!Damage
				x(i,j)=x(i,(j+1))+x((i+1),j); x(i,(j+1))=0; x((i+1),j) =0
				!hits=hits+1
			END IF			!End left to right

		END IF		!END last column or not

			

	END IF		!END ODD or EVEN
			
   END DO	!END column scaling

  IF (flip==1) THEN	!Alternate odd/even rows
	flip=0
  ELSE
	flip=1
  END IF

END DO		!END row scaling

6666 RETURN
END SUBROUTINE movement

!-------Laser-------Laser-------Laser-------Laser-------Laser-------Laser-------Laser--------------Laser-------
SUBROUTINE laser(i,j,row,col,x)
	INTEGER :: i, j, row, col			!This subroutine simply moves all remaining lasers up two rows.
	INTEGER, DIMENSION (row,col) :: x

DO i=2, (row-1), 2		!Even rows are reserved for Lasers
	DO j=1, col
		IF (i/=(row-1)) THEN
			x(i,j)=x((i+2),j)
		ELSE
			x(i,j)=0
		END IF
	END DO
END DO

RETURN
END SUBROUTINE laser

!-------Commands-------Commands-------Commands-------Commands-------Commands-------Commands-------Commands-------
SUBROUTINE commands(command,shoot,left,right,j,row,col,x,gcommand,charge,updown,shots)
	IMPLICIT NONE					!This subroutine acts upon the command entered by the player
	INTEGER :: j, row, col, charge, updown, shots
	INTEGER, DIMENSION (row,col) :: x
	CHARACTER(1) ::	command, shoot, right, left, gcommand

!BEGIN SEQUENCE "SHOOT"
IF ((command==shoot).AND.(charge/=0)) THEN
	charge=charge-1		!-1 charge (ammo)
	shots=shots+1		!+1 to shots counter
	updown=-1	!triggers charge_down animation
	DO j=1,col
		IF (x(row,j)==4) x((row-1),j)=1
	END DO

END IF

!BEGIN SEQUENCE "MOVE LEFT"
IF (command==left) THEN
	IF (charge/=3) THEN
		updown=updown+1		!when updown >1, charge increases by +1
	ELSE
		updown=0		!stops the charge_up animation from playing when the charge is full
	END IF
	DO j=2,col
		IF (x(row,j)==4) THEN
			x(row,(j-1))=4; x(row,j)=0
		END IF
	END DO

END IF

!BEGIN SEQUENCE "MOVE RIGHT"
IF (command==right) THEN
	IF (charge/=3) THEN
		updown=updown+1
	ELSE
		updown=0
	END IF
	DO j=1,(col-1)
		IF (x(row,j)==4) THEN
			x(row,(j+1))=4; x(row,j)=0; GOTO 40
		END IF
	END DO

40 END IF

IF ((command/=shoot).AND.(command/=right).AND.(command/=left).AND.(charge/=3)) THEN
	updown=updown+1			!This area accounts for the player not entering a command
ELSE IF ((command/=shoot).AND.(command/=right).AND.(command/=left).AND.(charge==3)) THEN
	updown=0
ELSE
	CONTINUE
END IF

IF ((updown>0).AND.(charge/=3)) charge=charge+1	!Increase charge
gcommand=command	!gcommand is for graphics. This avoids animation glitches caused by the user entering new commands mid-animation
command='p'	!reset command, otherwise the same command is repeated over and over again until a new one is entered

RETURN
END SUBROUTINE commands

!-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------
SUBROUTINE printscreen(i,j,row,col,x,flip,rl,gcommand,left,right,shoot,frame,numcol,charge,updown,score,wave)
	IMPLICIT NONE				!This subroutine handles the beautiful graphics
	INTEGER :: i, j, row, col, flip, rl, frame, numcol, charge, updown, score, wave
	INTEGER, DIMENSION (row,col) :: x
	CHARACTER(1) ::	gcommand, right, left, shoot

WRITE(*,*)''	!Buffer between screen prints
WRITE(*,6003) '♥ LIVES: 1       ✪ SCORE: ', score, '       ⌘ Wave: ', wave
6003 FORMAT(A,I5,A,I2)

WRITE(*,9000,advance='no') '┌'	!Prints Top Border
DO i=1,3*col+2
	WRITE(*,9000,advance='no') '─'
END DO
WRITE(*,9000) '┐'

!Invader/Laser movment
rl=1		!rl determines right/left movement; 1=right, 0=left 
flip=1		!flip determines odd/even; 1=odd, 0=even
DO i=1,(row-1)		!scale rows (-player row)
	DO j=1,col	!scale columns
		IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
		IF (flip==1) THEN	!Invader row
			IF (rl==1) THEN	!moving right
				IF(x(i,j)==6) THEN
					call invader_r(frame)
				ELSE
					call space()
				END IF
			ELSE	!moving left
				IF(x(i,j)==6) THEN
					call invader_l(frame)
				ELSE
					call space()
				END IF
			END IF
		ELSE	!Laser row
			IF(x(i,j)==0) THEN
				call space()
			ELSE
				call laser_move(frame)
			END IF
		END IF
		IF (j==col) WRITE(*,9000) ' │'	!Right border
	END DO	!End col
	IF (flip==1) THEN	!Invader row -> Laser row
		flip=0
		IF (rl==1) THEN	!Right -> Left
			rl=0
		ELSE		!Left -> Right
			rl=1
		END IF
	ELSE			!Laser row -> Invader row
		flip=1
	END IF
END DO		!End row

!Player movement
DO j=1,col	!scale columns
	IF (gcommand==right) THEN	!MOVE RIGHT ANIMATION
		IF (j==1) WRITE(*,9000,advance='no') '│'	!Left border
		IF(x(row,j)==0) THEN	!Open space 
			call space()
		ELSE
			IF(gcommand==left) THEN		!Move left
				call player_l(frame)
			ELSE IF(gcommand==right) THEN	!Move right
				call player_r(frame)
			ELSE				!No move
				call player(frame)
			END IF
		END IF
		IF (j==col) WRITE(*,9000) ' │'	!Right border
	ELSE IF (gcommand==left) THEN	!MOVE LEFT ANIMATION
			IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
		IF(x(row,j)==0) THEN	!Open space 
			call space()
		ELSE
			IF(gcommand==left) THEN		!Move left
				call player_l(frame)
			ELSE IF(gcommand==right) THEN	!Move right
				call player_r(frame)
			ELSE				!No move
				call player(frame)
			END IF
		END IF
		IF (j==col) WRITE(*,9000) '│'	!Right border
	ELSE			!SHOOT/IDLE ANIMATION
		IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
		IF(x(row,j)==0) THEN	!Open space 
			call space()
		ELSE
			IF(gcommand==left) THEN		!Move left
				call player_l(frame)
			ELSE IF(gcommand==right) THEN	!Move right
				call player_r(frame)
			ELSE				!No move
				call player(frame)
			END IF
		END IF
		IF (j==col) WRITE(*,9000) ' │'	!Right border
	END IF
END DO

WRITE(*,9000,advance='no') '└'	!Prints Bottom Border
DO i=1,3*col+2
	WRITE(*,9000,advance='no') '─'
END DO
WRITE(*,9000) '┘'

!Weapon Charge
WRITE(*,9000,advance='no') 'WEAPON CHARGE: '
	IF (updown<0) THEN
		call wcharge_down(frame,charge)
	ELSE IF (updown>0) THEN
		call wcharge_up(frame,charge)
	ELSE
		call wcharge_idle(frame,charge)
	END IF
WRITE(*,9000) ''

gcommand='p'	!reset gcommand

9000 FORMAT(A)	!To allow the advance=no clause
!NOTE: even for 'unformatted' statements this is needed, otherwise spacing is different

RETURN
END SUBROUTINE printscreen

!-------Enter Command-------Enter Command-------Enter Command-------Enter Command-------Enter Command-------
SUBROUTINE enter_command(command)	!This subroutine continuously loops parallel to the main program and allows
	IMPLICIT NONE			!the user to enter their command
	CHARACTER(1) :: command

CALL SYS_KEYSET(1)		!Use FGET (Fortran implicit) instead?
READ(*,*) command
CALL SYS_KEYSET(0)

RETURN
END SUBROUTINE enter_command

!-------Write Lose Sequence-------Write Lose Sequence-------Write Lose Sequence-------Write Lose Sequence-------
SUBROUTINE write_lose_sequence(yesno,kills,shots,hits,score)
	IMPLICIT NONE
	INTEGER :: yesno, kills, shots, hits, score

call execute_command_line('clear')
WRITE (*,*) '☠ You lose!☠'
WRITE(*,*) ''
WRITE(*,6000) '❈ SHOTS: ', shots
WRITE(*,6000) '☄ HITS: ', hits
WRITE(*,6001) '◎ ACCURACY: ', FLOAT(hits)/FLOAT(shots)*100., '%'
WRITE(*,6000) '☠ KILLS: ', kills
WRITE(*,6002) '✪ SCORE: ', score
WRITE(*,*) ''
WRITE(*,*) 'Try again? Yes=1 No=0'
READ(*,*) yesno

6000 FORMAT(A,I3)
6001 FORMAT(A,F4.1,A)
6002 FORMAT(A,I5)

RETURN
END SUBROUTINE write_lose_sequence

!-------Write Win Sequence-------Write Win Sequence-------Write Win Sequence-------Write Win Sequence-------
SUBROUTINE write_win_sequence(yesno,kills,shots,hits,score)
	IMPLICIT NONE
	INTEGER :: yesno, kills, shots, hits, score

call execute_command_line('clear')
WRITE (*,*) '✰ You Win!✰'
WRITE(*,*) ''
WRITE(*,6000) '❈ SHOTS: ', shots
WRITE(*,6000) '☄ HITS: ', hits
WRITE(*,6001) '◎ ACCURACY: ', FLOAT(hits)/FLOAT(shots)*100., '%'
WRITE(*,6000) '☠ KILLS: ', kills
WRITE(*,6002) '✪ SCORE: ', score
WRITE(*,*) ''
WRITE(*,*) 'Kick ass again? Yes=1 No=0'
READ(*,*) yesno

6000 FORMAT(A,I3)
6001 FORMAT(A,F4.1,A)
6002 FORMAT(A,I5)

RETURN
END SUBROUTINE write_win_sequence

!-------Game Timer-------Game Timer-------Game Timer-------Game Timer-------Game Timer-------Game Timer-------
SUBROUTINE game_timer(start,finish)	!This subroutine pauses the main program between animation frames and controls
	IMPLICIT NONE			!the speed of the game.
	REAL :: start, finish

!call sleep(1) -- this works in parallel, but only for integer values [no overhead vs 25% == HUGE]
	call cpu_time(start)
	50 call cpu_time(finish)
	IF ((finish-start)<0.05) GOTO 50

RETURN
END SUBROUTINE game_timer
