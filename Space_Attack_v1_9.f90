!
PROGRAM array_invaders
!
! Purpose:
!    This began as an attempt to recreate the classic game Space Invaders, made by Tomohiro Nishikado and released in 1978.
!	Instead this has organically grown into a different game, and is still evolving as such.
!
! To Compile with make:
!	>make
!	>export OMP_NUM_THREADS=16
!	>./Space_Attack.out
!
! To Compile manually:
!	>export OMP_NUM_THREADS=16	(Must do this step every time you open a new terminal!)
!	>gfortran -fopenmp -c Space_Attack_v1_9.f90
!	>gfortran -c sys_keyin.c
!	>gfortran -c graphics_sub.f90
!	>gfortran -c primaries_sub.f90
!	>gfortran -c lose_animation.f90
!	>gfortran -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o lose_animation.o Space_Attack_v1_6.o
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
!	1.7	K. O'Mara	04/16/15	replaced game_timer with a c function (nanosleep) and made menu screen modular
!	1.7.1	K. O'Mara	04/17/15	removed most GOTO's and unnecessary variables (i.e. i,j,k import into subroutine)
!	1.8	K. O'Mara	04/17/15	HUGE OVERHAUL:
!							-massive update to graphics library
!							-new subroutine file: primaries_sub.f90
!							-added support for powerups, new enemy types, and new weapon types
!	1.8.1	K. O'Mara	04/18/15	Fixed many bugs, now the huge overhaul is successful
!	1.8.2	K. O'Mara	04/18/15	Added lose sequence animation (big explosion across screen) and added in the skirmisher
!	1.9	K. O'Mara	04/19/15	Added waves and powerups (missing weapon up/ammo up)
!
!				TO DO:	combination animation; final wave animation (i.e. win); last 2 powerups; powerup flag on main screen;
!					new waves, new enemies; menu screen; save game (high score)
!
!
! GOTO Statements:
!	Number	GOTO
!	------	-----
!	40	End MOVE RIGHT sequence
!	777	Restart
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
	INTEGER :: endgame, x00, i, j, k, h, yesno, frame, charge, updown, shots, kills, hits, dframe
	INTEGER ::  score, wincon, wave, buffer, last, length(10), row_num(10), lives, eplaser, flop, score2
	INTEGER :: tcounter, mcount, dcount
	INTEGER, DIMENSION (row,col) :: animation, invader, laser, elaser, powerup
	REAL :: start, finish
	CHARACTER(1) ::	command, shoot, right, left, gcommand	!C(LEN=1) == C(1) == C
	CHARACTER(3*col), DIMENSION(10) :: string
	LOGICAL :: multiplier, damageboost

!------------------------------------------------------------------------------------------!
!----------BEGIN GAME----------BEGIN GAME----------BEGIN GAME----------BEGIN GAME----------!
!------------------------------------------------------------------------------------------!

	777 CALL controls(shoot,right,left,yesno,row,col,string,buffer,length,row_num,last)	!Determine Controls and Terminal Size
	CALL initialize(invader,elaser,laser,powerup,animation,x00,row,col,endgame,frame,charge,updown,shots,&
								hits,kills,score,wave,lives,multiplier,tcounter,mcount,dcount)
		call printscreen(row,col,animation,invader,gcommand,left,right,shoot,frame,charge,updown,score,wave,lives,dframe,endgame)

!This parallel region has two main sections.  The first is the bulk of the game and takes care of all the action.  The second allows the
!  player to enter commands at will.  This is entirely necessary as Fortran pauses computation at a READ statement until the user hits enter.
!  By running these two sections in parallel the game is able to be real time and continue even if the user does not enter any commands.

!$OMP PARALLEL
	!$OMP SECTIONS
	!$OMP SECTION
	!$OMP SECTION
	   DO WHILE (endgame==1)	!MAIN GAME LOOP
		CALL turn_start(animation,row,col,tcounter,multiplier,damageboost,mcount,dcount,score,score2)
		CALL move_invader(x00,invader,elaser,row,col)		!MOVE invader
		CALL move_powerup(powerup,row,col)								!MOVE powerup
		CALL move_laser(laser,invader,powerup,animation,row,col,eplaser,kills,hits,score,lives,multiplier,damageboost)
		CALL move_enemy_laser(elaser,invader,powerup,animation,row,col,lives,eplaser,kills,hits,score,multiplier,damageboost)
		CALL commands(command,shoot,left,right,row,col,invader,laser,gcommand,charge,updown,shots)	!Player's Command
		CALL fill_animation(animation,invader,laser,elaser,powerup,row,col)				!Finalize animation matrix
		IF (multiplier.EQV. .TRUE.) score=score+2*(score-score2)	!apply 2x score multiplier powerup

		DO WHILE (frame<4)		!3 Frame Animation.
			CALL printscreen(row,col,animation,invader,gcommand,left,right,shoot,frame,charge,&
										updown,score,wave,lives,dframe,endgame)
			CALL ctimer()		!Timer to allow a pause between frames.
			frame=frame+1
		END DO
		frame=1; !reset frame count
		CALL win_condition(invader,elaser,laser,powerup,x00,row,col,charge,updown,wave,endgame,lives&
			,animation,gcommand,left,right,shoot,frame,score,dframe)

		IF (endgame==0) THEN					!Begin Lose Animation
			dframe=0
			frame=3		!to freeze final positions
			flop=0; h=0	!set first wave explosions
			DO k=1,70	!loser_animation frame count
				CALL loser_animation(row,col,animation,dframe,h,flop,invader)
				CALL printscreen(row,col,animation,invader,gcommand,left,right,shoot,frame,charge,updown,&
												score,wave,lives,dframe,endgame)
				CALL ctimer()	!Timer to allow a pause between frames
			END DO
		END IF
	   END DO			!END MAIN GAME LOOP
	IF (endgame==0) THEN
		WRITE(*,*) 'Game over man, game over! (hit any key to continue)'
	ELSE		!endgame==9
		WRITE(*,*) "You did it, you killed them all! (hit any key to continue)"
	END IF		
	!$OMP SECTION
		DO WHILE (endgame==1)			!endgame=1 means the game is still going (no win or lose)
			CALL enter_command(command)	!Repetedly lets the user enter a command
		END DO
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
SUBROUTINE controls(shoot,right,left,yesno,row,col,string,buffer,length,row_num,last)
	IMPLICIT NONE					!This is the very first subroutine called. It allows the user to
	CHARACTER(1) :: shoot, right, left		!appropriately resize their terminal and then set ther controls.
	INTEGER :: yesno, i, j, k, buffer, last		!If the terminal is too tall (width is irrelevant) then the player
	INTEGER, intent(in) :: row, col			!can "see into the past" (see the previous frame).
	CHARACTER(3*col), DIMENSION(10) :: string
	INTEGER, DIMENSION(10) :: length, row_num
	
call execute_command_line('clear')	!Clear screen before printing
call menu_main(string,length,row_num,last,col)
call print_menu(row,col,string,buffer,length,row_num,last)
READ(*,*) 

yesno=1
DO WHILE (yesno==1)
	WRITE(*,*) 'Please enter your commands. W,D,A are recommended.'
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
	IF (yesno==1) call execute_command_line('clear')
END DO

9000 FORMAT(A)	!To allow the advance=no clause
!NOTE: even for 'unformatted' statements this is needed, otherwise spacing is different

RETURN
END SUBROUTINE controls

!--------Initialize positions--------Initialize Positions--------Initialize positions--------Initialize Positions--------
SUBROUTINE initialize(invader,elaser,laser,powerup,animation,x00,row,col,endgame,frame,charge,updown,&
							shots,hits,kills,score,wave,lives,multiplier,tcounter,mcount,dcount)
	IMPLICIT NONE
	INTEGER :: x00, endgame, frame, charge, updown, shots, hits, kills, score, wave, lives, i, j, tcounter, mcount, dcount
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: invader, elaser, laser, powerup, animation
	LOGICAL :: multiplier
									!This subroutine initializes all relevant variables
endgame=1	!1 = game on!
invader=0; elaser=0; laser=0; powerup=0; animation=0	!empty matrices
x00=0		!empty spawn
invader(row,8)=-777!spawn you :)
frame=1		!set first frame count for graphics
 charge=3	!set at max weapon charge
shots=0		!set to 0 shots fired
hits=0		!set to 0 hits
kills=0		!set to 0 kills
updown=0	!set to idle charge animation
score=0		!set score to 0 points
wave=1		!set to wave 1
lives=1		!set lives to 1
multiplier=.FALSE.	!set score multiplier OFF
tcounter=0	!set turn counter to 0
mcount=0	!set multiplier turn counter to 0
 dcount=0	!set damage boost turn counter to 0
CALL init_random_seed()	!randomly pick a new random number seed

DO i=3,7,2			!Spawns the Wave 1 Invaders!
	DO j=5,11
		invader(i,j)=11
	END DO
END DO

	!Print initial position
CALL execute_command_line('clear')	!Clear screen before printing

RETURN
END SUBROUTINE initialize

!-------Commands-------Commands-------Commands-------Commands-------Commands-------Commands-------Commands-------
SUBROUTINE commands(command,shoot,left,right,row,col,x,laser,gcommand,charge,updown,shots)
	IMPLICIT NONE					!This subroutine acts upon the command entered by the player
	INTEGER :: j, row, col, charge, updown, shots
	INTEGER, DIMENSION (row,col) :: x, laser	!x==invader
	CHARACTER(1) ::	command, shoot, right, left, gcommand

!BEGIN SEQUENCE "SHOOT"
IF ((command==shoot).AND.(charge/=0)) THEN
	charge=charge-1		!-1 charge (ammo)
	shots=shots+1		!+1 to shots counter
	updown=-1	!triggers charge_down animation
	DO j=1,col
		IF (x(row,j)==-777) laser((row-1),j)=-1
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
		IF (x(row,j)==-777) THEN
			x(row,(j-1))=-777; x(row,j)=0
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
		IF (x(row,j)==-777) THEN
			x(row,(j+1))=-777; x(row,j)=0; GOTO 40
		END IF
	END DO

40 END IF

!BEGIN SEQUENCE "PIERCING SHOT"
IF (command=='1') THEN
	shots=shots+1		!+1 to shots counter
	DO j=1,col
		IF (x(row,j)==-777) laser((row-1),j)=-2
	END DO

END IF	

!BEGIN SEQUENCE "VAPORIZER"
IF (command=='2') THEN
	shots=shots+1		!+1 to shots counter
	DO j=1,col
		IF (x(row,j)==-777) laser((row-1),j)=-3
	END DO

END IF	

!BEGIN SEQUENCE "MISSILE FIRE"
IF (command=='3') THEN
	shots=shots+1		!+1 to shots counter
	DO j=1,col
		IF (x(row,j)==-777) laser((row-1),j)=-4
	END DO

END IF	

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

!-------Fill Animation-------Fill Animation-------Fill Animation-------Fill Animation-------Fill Animation
SUBROUTINE fill_animation(animation,invader,laser,elaser,powerup,row,col)
	IMPLICIT NONE
	INTEGER :: i, j, k
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION(row,col) :: animation, invader, laser, elaser, powerup

DO i=1,row,2		!INVADER/PLAYER
	DO j=1,col
		IF (invader(i,j)/=0) THEN
			IF (i<row) THEN
				k=mod(invader(i,j),10)
				animation(i,j)=invader(i,j)-k
			ELSE
				animation(i,j)=invader(i,j)	!player
			END IF
		END IF
	END DO
END DO

DO i=2,row-1,2		!POWERUP
	DO j=1,col
		IF (powerup(i,j)/=0) THEN
			k=mod(powerup(i,j),10)
			animation(i,j)=powerup(i,j)-k
		END IF
	END DO
END DO

DO i=2,row-1,2		!ONLY LASER
	DO j=1,col
		IF ((laser(i,j)/=0).AND.(elaser(i,j)==0)) THEN		!LASER
			animation(i,j)=laser(i,j)
		ELSE IF ((laser(i,j)==0).AND.(elaser(i,j)/=0)) THEN	!ELASER
			animation(i,j)=elaser(i,j)
		ELSE IF ((laser(i,j)/=0).AND.(elaser(i,j)/=0)) THEN	!LASER+ELASER
			animation(i,j)=666	!combined animation flag
		END IF
	END DO
END DO

RETURN
END SUBROUTINE fill_animation

!-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------Print Screen-------
SUBROUTINE printscreen(row,col,animation,invader,gcommand,left,right,shoot,frame,charge,updown,score,wave,lives,dframe,endgame)
	IMPLICIT NONE				!This subroutine handles the beautiful graphics
	INTEGER :: i, j, flip, rl, frame, charge, updown, score, wave, lives, dframe, endgame
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: animation, invader	!x==animation; y==invader
	CHARACTER(1) ::	gcommand, right, left, shoot

WRITE(*,*)''	!Buffer between screen prints
WRITE(*,9000,advance='no') ' LIVES:'		!Print lives
DO i=1,lives
	WRITE(*,9000,advance='no') '♥ '	!Print hearts
END DO
DO i=1,11-2*lives
	WRITE(*,9000,advance='no') ' '		!Fill spaces between lives and score
END DO
WRITE(*,6003) '✪ SCORE:', score, '        ⌘ Wave:', wave
6003 FORMAT(A,I5.5,A,I2.2)

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
		IF (flip==1) THEN	!INVADER row
				odd_move: SELECT CASE(animation(i,j))
					CASE(0)
						call space()			!empty space
					CASE(10)
						call invader_move(frame,rl)	!begin invaders
					CASE(20)
						call skirmisher_move(frame,rl)
					CASE(30)
						call bomber_move(frame,rl)
					CASE(40)
						call gunner_move(frame,rl)
					CASE(50)
						call shielder_move(frame,rl)
					CASE(60)
						call warper_move(frame,rl)
					CASE(70)
						call carrier_move(frame,rl)
					CASE(80)
						call mothership_move(frame,rl)
					CASE(-80:-10)
						call invader_killed(frame)	!invader death
					CASE(601)
						call explosion_1(frame)		!explosions
					CASE(602)
						call explosion_2(frame)
					CASE(603)
						call explosion_3(frame)
					CASE(1000)
						call ending_1000(dframe)	!begin lose animation
					CASE(1001)
						call ending_A(dframe,animation,i,j,row,col)
					CASE(1002)
						call ending_B(dframe,animation,i,j,row,col)
					CASE(2000:2002)
						call nextwave_2000(dframe,row,col,animation,i,j)
					CASE(2010:2012)
						call nextwave_2010(dframe,row,col,animation,i,j)
				END SELECT odd_move

		ELSE			!LASER/ELASER/POWERUP ROW
				even_move: SELECT CASE(animation(i,j))
					CASE(0)
						call space()			!empty space
					CASE(-1)
						call laser_move(frame,rl)	!begin lasers
					CASE(-2)
						call piercing_move(frame,rl)
					CASE(-3)
						call vaporizer_move(frame,rl)
					CASE(-4)
						call missile_move(frame,rl)
					CASE(+1)
						call elaser_move(frame,rl)
					CASE(200)
						call powerup_move(frame,rl)	!begin powerups
					CASE(-250:-200)
						call powerup_destroyed(row,col,animation,i,j)
					CASE(601)
						call explosion_1(frame)		!begin explosion
					CASE(602)
						call explosion_2(frame)
					CASE(603)
						call explosion_3(frame)
					CASE(666)
						call combination(frame,rl)	!combination
					CASE(1001)
						call ending_A(dframe,animation,i,j,row,col)	!begin lose animation
					CASE(1002)
						call ending_B(dframe,animation,i,j,row,col)
					CASE(2000:2002)
						call nextwave_2000(dframe,row,col,animation,i,j)
					CASE(2010:2012)
						call nextwave_2010(dframe,row,col,animation,i,j)

				END SELECT even_move
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
   IF (endgame==1) THEN
	IF (gcommand==right) THEN	!MOVE RIGHT ANIMATION
		IF (j==1) WRITE(*,9000,advance='no') '│'	!Left border
		IF( invader(row,j)==0) THEN	!Open space 
			call space()
		ELSE
			call player_r(frame)	!Move Right
		END IF
		IF (j==col) WRITE(*,9000) ' │'	!Right border

	ELSE IF (gcommand==left) THEN	!MOVE LEFT ANIMATION
		IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
		IF (invader(row,j)==0) THEN	!Open space 
			call space()
		ELSE
			call player_l(frame)	!Move Right
		END IF
		IF (j==col) WRITE(*,9000) '│'	!Right border

	ELSE			!SHOOT/IDLE ANIMATION
		IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
		IF (invader(row,j)==0) THEN	!Open space 
			call space()
		ELSE
				call player(frame)!No Move
		END IF
		IF (j==col) WRITE(*,9000) ' │'	!Right border
	END IF
   ELSE IF (endgame==0) THEN		!LOSE ANIMATION
	IF (j==1) WRITE(*,9000,advance='no') '│ '	!Left border
	IF (animation(row,j)==1000) THEN
		call ending_1000(dframe)
	ELSE IF (invader(row,j)==-777) THEN
		call player(frame)
	ELSE
		call space()
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

!-------INITIALIZE RANDOM SEED-------INITIALIZE RANDOM SEED-------INITIALIZE RANDOM SEED-------
!SOURCE: stackoverflow -> http://stackoverflow.com/questions/18754438/generating-random-numbers-in-a-fortran-module
SUBROUTINE init_random_seed()
      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
RETURN
END

!-------Write Lose Sequence-------Write Lose Sequence-------Write Lose Sequence-------Write Lose Sequence-------
SUBROUTINE write_lose_sequence(yesno,kills,shots,hits,score)
	IMPLICIT NONE
	INTEGER :: yesno, kills, shots, hits, score

call execute_command_line('clear')
WRITE (*,*) '☠ You lose!☠'
WRITE(*,*) ''
WRITE(*,6000) '❈ SHOTS:', shots
WRITE(*,6000) '☄ HITS:', hits
WRITE(*,6001) '◎ ACCURACY:', FLOAT(hits)/FLOAT(shots)*100., '%'	!⌖
WRITE(*,6000) '☠ KILLS:', kills
WRITE(*,6002) '✪ SCORE:', score
WRITE(*,*) ''
WRITE(*,*) 'Try again? Yes=1 No=0'
READ(*,*) yesno

6000 FORMAT(A,I3.3)
6001 FORMAT(A,F4.1,A)
6002 FORMAT(A,I5.5)

RETURN
END SUBROUTINE write_lose_sequence

!-------Write Win Sequence-------Write Win Sequence-------Write Win Sequence-------Write Win Sequence-------
SUBROUTINE write_win_sequence(yesno,kills,shots,hits,score)
	IMPLICIT NONE
	INTEGER :: yesno, kills, shots, hits, score

call execute_command_line('clear')
WRITE (*,*) '✰ You Win!✰'
WRITE(*,*) ''
WRITE(*,6000) '❈ SHOTS:', shots
WRITE(*,6000) '☄ HITS:', hits
WRITE(*,6001) '⌖ ACCURACY:', FLOAT(hits)/FLOAT(shots)*100., '%'	!⌖
WRITE(*,6000) '☠ KILLS:', kills
WRITE(*,6002) '✪ SCORE:', score
WRITE(*,*) ''
WRITE(*,*) 'Kick ass again? Yes=1 No=0'
READ(*,*) yesno

6000 FORMAT(A,I3.3)
6001 FORMAT(A,F4.1,A)
6002 FORMAT(A,I5.5)

RETURN
END SUBROUTINE write_win_sequence

!-------Win Condition-------Win Condition-------Win Condition-------Win Condition-------Win Condition-------
SUBROUTINE win_condition(invader,elaser,laser,powerup,x00,row,col,charge,updown,wave,endgame,lives&
			,animation,gcommand,left,right,shoot,frame,score,dframe)
	IMPLICIT NONE
	INTEGER :: x00, charge, updown, wave, i, j, k, endgame, lives, score, frame, dframe
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: invader, elaser, laser, powerup, animation
	CHARACTER(1) ::	command, shoot, right, left, gcommand

!Check for loss condition
IF (invader((row-2),col)/=0) THEN	!Check final Invader position (bottom right)
	lives=lives-1
	IF (lives/=0) invader((row-2),col)=0
END IF
IF (lives==0) endgame=0

IF (endgame/=0) THEN	!Check for win condition	!check if maximum value of gamespace == 0 (i.e. no Invaders left)
	IF (MAXVAL(invader)==0) THEN
		frame=3; dframe=0	!freeze final positions & set secondary frame count
		animation(row/4,col/2-1)=2000; animation(row/4,col/2)=2001; animation(row/4,col/2+1)=2002	!flag animations
		animation(row/2,col/2-1)=2010; animation(row/2,col/2)=2011; animation(row/2,col/2+1)=2012
		DO k=1,16
			dframe=dframe+1
			CALL printscreen(row,col,animation,invader,gcommand,left,right,shoot,frame,charge,updown,&
											score,wave,lives,dframe,endgame)
			CALL ctimer()	!Timer to allow a pause between frames
		END DO
		CALL wave_set(invader,elaser,laser,powerup,x00,row,col,charge,updown,wave,endgame)
	END IF
END IF

RETURN
END SUBROUTINE win_condition

!--------Wave Set--------Wave Set--------Wave Set--------Wave Set--------Wave Set--------
SUBROUTINE wave_set(invader,elaser,laser,powerup,x00,row,col,charge,updown,wave,endgame)
	IMPLICIT NONE
	INTEGER :: x00, charge, updown, wave, i, j, endgame, p
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: invader, elaser, laser, powerup

wave=wave+1	!increase wave count
elaser=0; laser=0; powerup=0;	!empty matrices
x00=0		!empty spawn
 charge=3	!set at max weapon charge
updown=0	!set to idle charge animation

wave_number: SELECT CASE(wave)
	CASE(2)
		DO i=7,7,2			!Spawns the Wave 2 Invaders!
			DO j=5,11
				invader(i,j)=11
			END DO
		END DO
		invader(3,6)=21; invader(3,10)=21
		powerup(4,8)=202
	CASE DEFAULT
		endgame=9
	END SELECT wave_number

RETURN
END SUBROUTINE wave_set

!-------LOSER ANIMATION-------LOSER ANIMATION-------LOSER ANIMATION-------LOSER ANIMATION-------
SUBROUTINE loser_animation(row,col,animation,dframe,h,flop,invader)
	IMPLICIT NONE
	INTEGER :: dframe, i, j, h, flop
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION(row,col) :: animation, invader

dframe=dframe+1
IF (dframe==1) THEN
	IF (invader((row-2),col)/=0) THEN	!Check final Invader position (bottom right)
		animation(row-2,col)=1000	!Flag Invader (bottom right)
	ELSE
		DO j=1,col
			IF (invader(row,j)==-777) THEN
				animation(row,j)=1000	!Flag player
				invader(row,j)=0
			END IF
		END DO
	END IF
END IF

IF ((MOD(dframe-10,6)==0).AND.(dframe/=10)) THEN		!every 6 frames past the 10th
	IF (flop==1) h=h+1		!h determines which wave of explosions (1->4)	
	DO i=(4-h)*row/4+flop,row,2		!carve out 1/4 of map, starting bottom right
		DO j=(4-h)*col/4+flop,col,2				!(5-h)*row/4+flop
			IF ((i<(5-h)*row/4).OR.(j<(5-h)*col/4)) THEN	!carve out L shape in map
				IF ((MOD(dframe-4,12)==0).AND.(dframe/=4))animation(i,j)=1001		!flags animation set A
				IF ((MOD(dframe-10,12)==0).AND.(dframe/=10)) animation(i,j)=1002	!flags animation set B
			END IF
		END DO
	END DO
	IF (flop/=0) THEN		!flop==0==first half explosions
		flop=0			!alternate flop
	ELSE
		flop=1
	END IF
END IF

RETURN
END SUBROUTINE loser_animation

!-------TURN START-------TURN START-------TURN START-------TURN START-------TURN START-------
SUBROUTINE turn_start(animation,row,col,tcounter,multiplier,damageboost,mcount,dcount,score,score2)
	IMPLICIT NONE
	INTEGER, intent(in) :: row, col
	INTEGER :: tcounter, mcount, dcount, score, score2
	INTEGER, DIMENSION (row,col) :: animation
	LOGICAL :: multiplier, damageboost

tcounter=tcounter+1	!increase turn count
animation=0		!clear animation matrix
IF (multiplier.EQV. .TRUE.) THEN				!Score Multiplier
	IF (mcount==0) mcount=tcounter	!start timer
	score2=score			!record starting score
	IF (tcounter-mcount>16) THEN	!check for time limit
		multiplier=.FALSE.	!end multiplier
		mcount=0		!reset turn count
	END IF
END IF
IF (damageboost.EQV. .TRUE.) THEN				!Damage Boost
	IF (dcount==0) dcount=tcounter	!start timer
	IF (tcounter-dcount>16) THEN	!check for time limit
		damageboost=.FALSE.	!end multiplier
		dcount=0		!reset turn count
	END IF
END IF

RETURN
END SUBROUTINE turn_start
