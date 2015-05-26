! This block initializes all the relevant variables before loading the menu sequence.
SUBROUTINE menu_initialize(wmenu,wchoice,endgame,manim,rblank,control,difficulty,gametype,num_ordinance,ordinancepoints,&
												yesno,cheat,naming)
	IMPLICIT NONE
	CHARACTER(10) :: wmenu, gametype
	CHARACTER(1), DIMENSION(2,5) ::	control
	INTEGER :: wchoice, endgame, ordinancepoints, yesno, num_ordinance(5), difficulty
	LOGICAL :: rblank, manim, cheat(5), naming

wmenu='Intro'	!set to intro screen
wchoice=1	!set to default choice
endgame=0	!set to game off
manim=.FALSE.	!set menu animation flag off
rblank=.TRUE.	!set to read blank (i.e. press enter, no key required)
 cheat=.FALSE.	!set all chets to De-Active -> 1 = Year o' Plenty; 2 = Fiver
naming=.FALSE.	!set naming high score to off
control(1,1)='w'; control(1,2)='d'; control(1,3)='a'; control(1,4)='p'				!shoot; right; left; pause
control(2,1)='1'; control(2,2)='2'; control(2,3)='3'; control(2,4)='4'; control(2,5)='5'	!scatterhots; vaporizer; missile
difficulty=1	!set difficulty -> 0 = Easy; 1 = Normal; 2 = Brutal
gametype='Standard'	!set gametype
num_ordinance(1)=2; num_ordinance(2)=2; num_ordinance(3)=1	!set starting Special Ordinance
ordinancepoints=9	!set "Normal" allotment of ordinance points
yesno=1		!set game to ON

RETURN
END SUBROUTINE menu_initialize

!------- WHICH OPTION ------- WHICH OPTION ------- WHICH OPTION -------
! This block takes "wchoice" and "wmenu" and a) which menu player is in and b) what choice they selected
! It then decides the appropriate course of action, either a) flag animation for selection or b) set wmenu
! to the next menu selected.
SUBROUTINE which_option(command,wchoice,wmenu,manim,rblank,control,difficulty,gametype,num_ordinance,&
								total,ordinancepoints,yesno,lives,cheat)
	IMPLICIT NONE
	CHARACTER(1) :: command
	CHARACTER(1), DIMENSION(2,5) :: control
	CHARACTER(10) :: wmenu, gametype
	INTEGER :: wchoice, total, ordinancepoints, yesno, lives, num_ordinance(5), difficulty, i, j
	LOGICAL :: manim, rblank, cheat(5)

option_selector: SELECT CASE(wmenu)
	CASE('Intro')
		IF (manim .EQV. .FALSE.) THEN
			manim=.TRUE.	!flag animation on
		ELSE
			wmenu='Main'	!Load Main
			manim=.FALSE.	!de-flag menu animation
			rblank=.FALSE.	!set enter command
		END IF
		
	CASE('Main')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Prep'	!Start Game
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Controls'!Controls
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='3') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=3	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_Main'!Guide_Main
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='4') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=4	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='S_Standard'!Standard High Scores
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Cheats'	!Cheats
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='EXIT'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
				yesno=0		!Flag exit game loop
			END IF
		END IF	


	CASE('S_Standard')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='S_Endless'!Endless High Scores
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				OPEN(unit=100, file='.scores_standard.dat', status='unknown')
				DO i=1,10
					DO j=1,6
						reset_standard: SELECT CASE(j)
							CASE(1)
								WRITE(100,5001) '_______'
							CASE(2)
								WRITE(100,5001) '00000  '
							CASE(3)
								WRITE(100,5001) 'Easy   '
							CASE(4)
								WRITE(100,5001) '000    '
							CASE(5)
								WRITE(100,5001) '00.0%  '
							CASE(6)
								WRITE(100,5001) '00     '
							END SELECT reset_standard
							5001 FORMAT(A7)
					END DO
				END DO
				CLOSE(100)
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF

	CASE('S_Endless')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='S_Standard'!Standard High Scores
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				OPEN(unit=200, file='.scores_endless.dat', status='unknown')
				DO i=1,10
					DO j=1,6
						reset_endless: SELECT CASE(j)
							CASE(1)
								WRITE(200,5001) '_______'
							CASE(2)
								WRITE(200,5001) '00000  '
							CASE(3)
								WRITE(200,5001) 'Easy   '
							CASE(4)
								WRITE(200,5001) '000    '
							CASE(5)
								WRITE(200,5001) '00.0%  '
							CASE(6)
								WRITE(200,5001) '00     '
							END SELECT reset_endless
					END DO
				END DO
				CLOSE(200)
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF		

	CASE('Cheats')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (cheat(1) .EQV. .FALSE.) THEN
					cheat(1)=.TRUE.
					ordinancepoints=99	!Year o' Plenty Cheat
				ELSE
					cheat(1)=.FALSE.
					IF (difficulty==2) THEN	!set difficulty
						ordinancepoints=3	!set allotment of ordinance points
						num_ordinance(1)=0; num_ordinance(2)=0; num_ordinance(3)=1
					ELSE IF (difficulty==0) THEN
						ordinancepoints=15	!set allotment of ordinance points
						num_ordinance(1)=2; num_ordinance(2)=2; num_ordinance(3)=3
					ELSE
						ordinancepoints=9	!set allotment of ordinance points
						num_ordinance(1)=2; num_ordinance(2)=2; num_ordinance(3)=1
					END IF
				END IF
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (cheat(2) .EQV. .FALSE.) THEN
					cheat(2)=.TRUE.		!Fiver Cheat
				ELSE
					cheat(2)=.FALSE.
				END IF
				manim=.FALSE.	!de-flag menu animation
			END IF
!		ELSE IF (command=='3') THEN
!			IF (manim .EQV. .FALSE.) THEN
!				wchoice=3	!flag choice
!				manim=.TRUE.	!flag animation on
!			ELSE
!				IF (cheat3 .EQV. .FALSE.) THEN
!					cheat3=.TRUE.		!Level Skip
!				ELSE
!					cheat3=.FALSE.
!				END IF
!				manim=.FALSE.	!de-flag menu animation
!			END IF	
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF

	CASE('Prep')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (total<=ordinancepoints) wmenu='START!'	!Start Game
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (difficulty==1) THEN	!set difficulty
					difficulty=2		!set Brutal
					ordinancepoints=3	!set allotment of ordinance points
					num_ordinance(1)=0; num_ordinance(2)=0; num_ordinance(3)=1
				ELSE IF (difficulty==2) THEN
					difficulty=0		!set Easy
					ordinancepoints=15	!set allotment of ordinance points
					num_ordinance(1)=2; num_ordinance(2)=2; num_ordinance(3)=3
				ELSE
					difficulty=1		!set Normal
					ordinancepoints=9	!set allotment of ordinance points
					num_ordinance(1)=2; num_ordinance(2)=2; num_ordinance(3)=1
				END IF
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='3') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=3	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (gametype=='Standard') THEN	!set gametype
					gametype='Endless'	!set Endless
				ELSE
					gametype='Standard'	!set Standard
				END IF
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='4') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=4	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_ordinance(1)<ordinancepoints) num_ordinance(1)=num_ordinance(1)+1	!+1 scattershot
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='5') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=5	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_ordinance(1)>0) num_ordinance(1)=num_ordinance(1)-1		!-1 scattershot
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='6') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=6	!flag choice
				manim=.TRUE.	!flag animation on					!+1 vaporizer
				IF (num_ordinance(2)<1+ordinancepoints/2) num_ordinance(2)=num_ordinance(2)+1
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_ordinance(2)>0) num_ordinance(2)=num_ordinance(2)-1		!-1 vaporizer			
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='8') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=8	!flag choice
				manim=.TRUE.	!flag animation on					!+1 missile
				IF (num_ordinance(3)<ordinancepoints/3) num_ordinance(3)=num_ordinance(3)+1
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='9') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=9	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_ordinance(3)>0) num_ordinance(3)=num_ordinance(3)-1		!-1 missile			
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF

	CASE('Controls')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(1,1)=command	!read "shoot" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(1,2)=command	!read "right" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='3') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=3	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(1,3)=command	!read "left" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='4') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=4	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(2,1)=command!read "scattershot" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='5') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=5	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(2,2)=command!read "vaporizer" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='6') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=6	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(2,3)=command	!read "missile" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				control(1,4)=command	!read "pause" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='9') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=9	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				control(1,1)='w'; control(1,2)='d'; control(1,3)='a'; control(1,4)='p'
				control(2,1)='1'; control(2,2)='2'; control(2,3)='3'
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF

	CASE('Guide_Main')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a1'!Overview
				manim=.FALSE.	!de-flag menu animation
				rblank=.TRUE.	!set blank command
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_b1'!Enemies
				manim=.FALSE.	!de-flag menu animation
				rblank=.TRUE.	!set blank command
			END IF
		ELSE IF (command=='3') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=3	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_c1'!Weapons
				manim=.FALSE.	!de-flag menu animation
				rblank=.TRUE.	!set blank command
			END IF
		ELSE IF (command=='4') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=4	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_d1'!Powerups
				manim=.FALSE.	!de-flag menu animation
				rblank=.TRUE.	!set blank command
			END IF
		ELSE IF (command=='0') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=0	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Main'	!Main Menu
				manim=.FALSE.	!de-flag menu animation
			END IF
		END IF

	!Guides_a,b,c are subsets of Guide_Main, but for simplicity are contained within the primary
	!CASE block.
		!GUIDE_A1
		CASE('Guide_a1')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a2'!Load Guide_a2
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_a2')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a3'!Load Guide_a3
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_a3')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a4'!Load Guide_a4
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_a4')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a5'!Load Guide_a5
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_a5')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_a6'!Load Guide_a6
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_a6')				!Directs back to Guide_Main at end
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_Main'!Load Guide_Main
				manim=.FALSE.	!de-flag menu animation
				rblank=.FALSE.	!set read command
			END IF

		!GUIDE_B1
		CASE('Guide_b1')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_b2'!Load Guide_b2
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_b2')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_b3'!Load Guide_b3
				manim=.FALSE.	!de-flag menu animation
			END IF
		CASE('Guide_b3')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_Main'!Load Guide_Main
				manim=.FALSE.	!de-flag menu animation
				rblank=.FALSE.	!set read command
			END IF

		!GUIDE_C1
		CASE('Guide_c1')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_c2'!Load Guide_c2
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_c2')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_Main'!Load Guide_Main
				manim=.FALSE.	!de-flag menu animation
				rblank=.FALSE.	!set read command
			END IF

		!GUIDE_D1
		CASE('Guide_d1')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_d2'!Load Guide_d2
				manim=.FALSE.	!de-flag menu animation
			END IF

		CASE('Guide_d2')
			IF (manim .EQV. .FALSE.) THEN
				manim=.TRUE.	!flag animation on
			ELSE
				wmenu='Guide_Main'!Load Guide_Main
				manim=.FALSE.	!de-flag menu animation
				rblank=.FALSE.	!set read command
			END IF
	END SELECT option_selector

RETURN
END SUBROUTINE which_option

!------- SELECT MENU ------- SELECT MENU ------- SELECT MENU -------
! This block is used to load the appropriate strings before printing.  It uses "wmenu"
! and "wchoice" to determine the correct strings to load.
SUBROUTINE select_menu(string,length,row_num,last,row,col,wmenu,wchoice,manim,control,difficulty,gametype,&
				num_ordinance,total,ordinancepoints,lives,cheat,score_array,naming,ranking)
	IMPLICIT NONE
	CHARACTER(LEN=7), DIMENSION(11,6) :: score_array
	CHARACTER(3*col), DIMENSION(row) :: string
	CHARACTER(LEN=7) :: int_to_string, fullscore
	CHARACTER(10) :: wmenu, gametype
	CHARACTER(1), DIMENSION(2,5) :: control
	CHARACTER(1), DIMENSION(5) :: Lnum_ordinance
	CHARACTER(LEN=2) :: Ltotal, Lordinancepoints
	INTEGER, intent(in) :: row, col
	INTEGER :: length(row), row_num(row), last, wchoice, k, h, total, ordinancepoints, lives
	INTEGER :: i, j, num_ordinance(5), difficulty, ranking
	LOGICAL :: manim, cheat(5), naming

k=1
menu_selector: SELECT CASE(wmenu)
	CASE('Intro')		!INTRO
		string(k)='♅  ♓  ☫  ⇼  ۞  ♑  ⋤  ↂ  ♅'
		length(k)=25;	row_num(k)=3; k=k+1

		string(k)='♅     Space  Attack     ♅'
		length(k)=25;	row_num(k)=4; k=k+1

		string(k)='♅  ↂ  ⋥  ♑  ۞  ⇼  ☫  ♓  ♅'
		length(k)=25;	row_num(k)=5; k=k+1

		IF (manim .EQV. .FALSE.) THEN
			string(k)='✭  ✭  ✭  ✭  ✭  ✭  ✭  ✭  ✭'
		ELSE
			string(k)='✰  ✰  ✰  ✰  ✰  ✰  ✰  ✰  ✰'
		END IF
		length(k)=25;	row_num(k)=7; k=k+1

		string(k)='Please adjust the height of your terminal to'
		length(k)=44;	row_num(k)=9; k=k+1

		string(k)='fit the border now.'
		length(k)=19;	row_num(k)=10; k=k+1

		string(k)='If you have not already done so, please exit'
		length(k)=44;	row_num(k)=12; k=k+1

		string(k)='and type: export OMP_NUM_THREADS=2'
		length(k)=34;	row_num(k)=13; k=k+1

		string(k)='YOU MUST PRESS ENTER AFTER EVERY COMMAND'
		length(k)=40;	row_num(k)=15; k=k+1

		string(k)='Press any key to continue.'
		length(k)=26;	row_num(k)=17; k=k+1

		string(k)="by Kevin O'Mara"
		length(k)=15;	row_num(k)=20; k=k+1

		string(k)='version 1.9.7'
		length(k)=13;	row_num(k)=row+3; k=k+1

		last=k-1

	CASE('Main')		!MAIN
		string(k)='⋲     MAIN MENU     ⋺'
		length(k)=21;	row_num(k)=3; k=k+1

		string(k)='♅  ♓  ♃  ♄  ☫  ♑  ♇  ☿  ♅'
		length(k)=25;	row_num(k)=5; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(k)='✰ ✰ ✰ ✰ (1) PLAY! ✰ ✰ ✰ ✰'
			length(k)=25
		ELSE
			string(k)='(1) PLAY!'
			length(k)=9
		END IF
		row_num(k)=7; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==2)) THEN
			string(k)='✰ ✰ ✰ ✰ (2) Controls ✰ ✰ ✰ ✰'
			length(k)=28
		ELSE
			string(k)='(2) Controls'
			length(k)=12
		END IF
		row_num(k)=9; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==3)) THEN
			string(k)='✰ ✰ ✰ ✰ (3) Guide ✰ ✰ ✰ ✰'
			length(k)=25
		ELSE
			string(k)='(3) Guide'
			length(k)=9
		END IF
		row_num(k)=11; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==4)) THEN
			string(k)='✰ ✰ ✰ (4) High Scores ✰ ✰ ✰'
			length(k)=27
		ELSE
			string(k)='(4) High Scores'
			length(k)=15
		END IF
		row_num(k)=13; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(k)='✰ ✰ ✰ ✰ (0) Exit Game ✰ ✰ ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(0) Exit Game'
			length(k)=13
		END IF
		row_num(k)=row+3; k=k+1

		last=k-1

	CASE('S_Standard')	!SCORE STANDARD HIGH SCORES
		string(k)='⋲    HIGH SCORES    ⋺'
		length(k)=21;	row_num(k)=3; k=k+1

		string(k)='- - Standard - -'
		length(k)=16;	row_num(k)=4; k=k+1

		string(k)='Name     Score  Diff.   Kills  Acc.  Wave'
		length(k)=41;	row_num(k)=6; k=k+1

		OPEN(unit=100, file='.scores_standard.dat', status='old')	!Open Score File
		DO i=1,10
			DO j=1,6
				READ(100,5001) score_array(i,j)			!Record information
				5001 FORMAT(A7)
			END DO
		END DO
		CLOSE(100)

		DO i=1,10
			IF ((naming .EQV. .TRUE.).AND.(ranking==i)) THEN
				string(k)= '> ' // score_array(i,1) // '  ' // TRIM(score_array(i,2)) // '  ' // &
					score_array(i,3) // '  ' // TRIM(score_array(i,4)) // '   ' // &
					TRIM(score_array(i,5))  // '   ' // TRIM(score_array(i,6)) // ' <'
				length(k)=45
			ELSE
				string(k)=score_array(i,1) // '  ' // TRIM(score_array(i,2)) // '  ' // &
					score_array(i,3) // '  ' // TRIM(score_array(i,4)) // '   ' // &
					TRIM(score_array(i,5)) // '   ' // TRIM(score_array(i,6))
				length(k)=41
			END IF
				row_num(k)=6+i; k=k+1
		END DO

		IF (naming .EQV. .TRUE.) THEN
			string(k)='Enter your name cadet! (7 characters max)'
			length(k)=41
			row_num(k)=row-1; k=k+1
		END IF

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(k)='✰ ✰ (1) Endless High Scores ✰ ✰'
			length(k)=31
		ELSE
			string(k)='(1) Endless High Scores'
			length(k)=23
		END IF
		IF (naming .EQV. .FALSE.) THEN
			row_num(k)=row-1
		ELSE
			row_num(k)=row
		END IF
		k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==7)) THEN
			string(k)='✰ ✰ (7) Reset High Scores ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(7) Reset High Scores'
			length(k)=21
		END IF
		row_num(k)=row+1; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(k)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(0) Back to Main Menu'
			length(k)=21
		END IF
		row_num(k)=row+3; k=k+1

		last=k-1

	CASE('S_Endless')	!SCORE ENDLESS HIGH SCORES
		string(k)='⋲    HIGH SCORES    ⋺'
		length(k)=21;	row_num(k)=3; k=k+1

		string(k)='- - Endless - -'
		length(k)=15;	row_num(k)=4; k=k+1

		string(k)='Name     Score  Diff.   Kills   Acc.'
		length(k)=36;	row_num(k)=6; k=k+1

		OPEN(unit=200, file='.scores_endless.dat', status='old')	!Open Score File
		DO i=1,10
			DO j=1,6
				READ(200,5001) score_array(i,j)			!Record information
			END DO
		END DO
		CLOSE(200)

		DO i=1,10
			IF ((naming .EQV. .TRUE.).AND.(ranking==i)) THEN
				string(k)= '> ' // score_array(i,1) // '  ' // TRIM(score_array(i,2)) // '  ' // &
					score_array(i,3) // '  ' // TRIM(score_array(i,4)) // '   ' // &
					TRIM(score_array(i,5))  // ' <'
				length(k)=40
			ELSE
				string(k)=score_array(i,1) // '  ' // TRIM(score_array(i,2)) // '  ' // &
					score_array(i,3) // '  ' // TRIM(score_array(i,4)) // '   ' // &
					TRIM(score_array(i,5))
				length(k)=36
			END IF
				row_num(k)=6+i; k=k+1
		END DO

		IF (naming .EQV. .TRUE.) THEN
			string(k)='Enter your name cadet! (7 characters max)'
			length(k)=41
			row_num(k)=row-1; k=k+1
		END IF

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(k)='✰ ✰ (1) Standard High Scores ✰ ✰'
			length(k)=32
		ELSE
			string(k)='(1) Standard High Scores'
			length(k)=24
		END IF
		IF (naming .EQV. .FALSE.) THEN
			row_num(k)=row-1
		ELSE
			row_num(k)=row
		END IF
		k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==7)) THEN
			string(k)='✰ ✰ (7) Reset High Scores ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(7) Reset High Scores'
			length(k)=21
		END IF
		row_num(k)=row+1; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(k)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(0) Back to Main Menu'
			length(k)=21
		END IF
		row_num(k)=row+3; k=k+1

		last=k-1

	CASE('Cheats')		!CHEATS
		string(1)='⋲         Cheats        ⋺'
		length(1)=25;	row_num(1)=3

		IF (cheat(1) .EQV. .FALSE.) THEN
			string(2)="(1) Year o' Plenty: De-Active"
			length(2)=29

		ELSE
			string(2)="(1) Year o' Plenty: Active"
			length(2)=26
		END IF
		row_num(2)=6

		IF (cheat(2) .EQV. .FALSE.) THEN
			string(3)='(2) Fiver: De-Active'
			length(3)=20
		ELSE
			string(3)='(2) Fiver: Active'
			length(3)=17
		END IF
		row_num(3)=8

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(4)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(4)=29
		ELSE
			string(4)='(0) Back to Main Menu'
			length(4)=21
		END IF
		row_num(4)=row+3

		last=4

	CASE('Prep')		!PREP
		string(1)='⋲   Prepare for Combat  ⋺'
		length(1)=25;	row_num(1)=3

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1).AND.(total<=ordinancepoints)) THEN
			string(2)='✰ ✰ ✰ ✰ (1) FIGHT‼ ✰ ✰ ✰ ✰'
			length(2)=26
		ELSE IF ((manim .EQV. .TRUE.).AND.(wchoice==1).AND.(total>ordinancepoints)) THEN
			string(2)='✖ ✖ ✖ ✖ (1) FIGHT‼ ✖ ✖ ✖ ✖'
			length(2)=26
		ELSE
			string(2)='(1) FIGHT‼'
			length(2)=10
		END IF
		row_num(2)=5

		IF (difficulty==1) THEN
			string(3)='(2) Difficulty: Normal'
			length(3)=22
		ELSE IF (difficulty==2) THEN
			string(3)='(2) Difficulty: Brutal'
			length(3)=22
		ELSE
			string(3)='(2) Difficulty: Easy'
			length(3)=20
		END IF
		row_num(3)=6

		IF (gametype=='Standard') THEN
			string(4)='(3) Gametype: Standard'
			length(4)=22
		ELSE
			string(4)='(3) Gametype:  Endless'
			length(4)=22
		END IF
		row_num(4)=7

		string(5)='✫ Special Ordinance ✫'		!→
		length(5)=21
		row_num(5)=9

		total=num_ordinance(1)+2*num_ordinance(2)+3*num_ordinance(3)	!total ordinance points remaining
		IF (ordinancepoints<10) THEN
			WRITE(Lordinancepoints,10) ordinancepoints	!record ordinance points
			h=1	!numer of columns occupied by Lordinancepoints
		ELSE
			WRITE(Lordinancepoints,20) ordinancepoints
			h=2
		END IF

		IF (total<=ordinancepoints) THEN
			WRITE(Ltotal,10) ordinancepoints-total
			string(6)=trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
			length(6)=19+h
		ELSE IF (total<=ordinancepoints+9) THEN
			WRITE(Ltotal,10) total-ordinancepoints
			IF (wchoice==1) THEN		!if trying to start game and poitns are over -> flag
				string(6)='⇶ -' // trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=22+h
			ELSE
				string(6)='-' // trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=20+h
			END IF
		ELSE
			WRITE(Ltotal,20) total-ordinancepoints
			IF (wchoice==1) THEN		!if trying to start game and poitns are over -> flag
				string(6)='⇶ -' // Ltotal // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=23+h
			ELSE
				string(6)='-' // Ltotal // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=21+h
			END IF
		END IF
		row_num(6)=10

		10 FORMAT(i1); 20 FORMAT(i2)					!BINGO
		WRITE(Lnum_ordinance(1),10) num_ordinance(1)
		string(7)='⁂  Scattershot [1 Point]: ' // Lnum_ordinance(1)
		length(7)=27
		row_num(7)=12

		IF ((manim .EQV. .TRUE.).AND.((wchoice==4).OR.(wchoice==5))) THEN
			string(8)='✰ ✰ ✰ ✰ (4/5) +1 / -1 ✰ ✰ ✰ ✰'
			length(8)=29
		ELSE
			string(8)='(4/5) +1 / -1'
			length(8)=13
		END IF
		row_num(8)=13

		WRITE(Lnum_ordinance(2),10) num_ordinance(2)
		string(9)='✹  Vaporizer [2 Points]: ' // Lnum_ordinance(2)
		length(9)=26
		row_num(9)=14

		IF ((manim .EQV. .TRUE.).AND.((wchoice==6).OR.(wchoice==7))) THEN
			string(10)='✰ ✰ ✰ ✰ (6/7) +1 / -1 ✰ ✰ ✰ ✰'
			length(10)=29
		ELSE
			string(10)='(6/7) +1 / -1'
			length(10)=13
		END IF
		row_num(10)=15

		WRITE(Lnum_ordinance(3),10) num_ordinance(3)
		string(11)='☢  Missile [3 Points]: ' // Lnum_ordinance(3)
		length(11)=24
		row_num(11)=16

		IF ((manim .EQV. .TRUE.).AND.((wchoice==8).OR.(wchoice==9))) THEN
			string(12)='✰ ✰ ✰ ✰ (8/9) +1 / -1 ✰ ✰ ✰ ✰'
			length(12)=29
		ELSE
			string(12)='(8/9) +1 / -1'
			length(12)=13
		END IF
		row_num(12)=17

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(13)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(13)=29
		ELSE
			string(13)='(0) Back to Main Menu'
			length(13)=21
		END IF
		row_num(13)=row+3

		last=13

	CASE('Controls')	!CONTROLS
		string(k)='⋲      CONTROLS     ⋺'
		length(k)=21;	row_num(k)=3; k=k+1

		string(k)='Select a Command below to enter a new key.'
		length(k)=42;	row_num(k)=5; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(k)='(1) Shoot: ' // control(1,1) // '→ _'
			length(k)=15
		ELSE
			string(k)='(1) Shoot: ' // control(1,1)
			length(k)=12
		END IF
		row_num(k)=7; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==2)) THEN
			string(k)='(2) Right: ' // control(1,2) // '→ _'
			length(k)=15
		ELSE
			string(k)='(2) Right: ' // control(1,2)
			length(k)=12
		END IF
		row_num(k)=8; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==3)) THEN
			string(k)='(3) Left: ' // control(1,3) // '→ _'
			length(k)=14
		ELSE
			string(k)='(3) Left: ' // control(1,3)
			length(k)=11
		END IF
		row_num(k)=9; k=k+1

		string(k)='✫ Special Ordinance ✫'
		length(k)=21
		row_num(k)=11; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==4)) THEN
			string(k)='(4) Scattershot: ' // control(2,1) // '→ _'
			length(k)=21
		ELSE
			string(k)='(4) Scattershot: ' // control(2,1)
			length(k)=18
		END IF
		row_num(k)=12; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==5)) THEN
			string(k)='(5) Vaporizer: ' // control(2,2) // '→ _'
			length(k)=19
		ELSE
			string(k)='(5) Vaporizer: ' // control(2,2)
			length(k)=16
		END IF
		row_num(k)=13; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==6)) THEN
			string(k)='(6) Missile: ' // control(2,3) // '→ _'
			length(k)=17
		ELSE
			string(k)='(6) Missile: ' // control(2,3)
			length(k)=14
		END IF
		row_num(k)=14; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==7)) THEN
			string(k)='(7) Pause: ' // control(1,4) // '→ _'
			length(k)=15
		ELSE
			string(k)='(7) Pause: ' // control(1,4)
			length(k)=12
		END IF
		row_num(k)=row-1; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==9)) THEN
			string(k)='✰ ✰ ✰ (9) Reset Controls ✰ ✰ ✰'
			length(k)=30
		ELSE
			string(k)='(9) Reset Controls'
			length(k)=18
		END IF
		row_num(k)=row+1; k=k+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(k)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(k)=29
		ELSE
			string(k)='(0) Back to Main Menu'
			length(k)=21
		END IF
		row_num(k)=row+3; k=k+1

		last=k-1

	CASE('Guide_Main')		!Guide Main
		string(1)='⋲       GUIDE       ⋺'
		length(1)=21;	row_num(1)=3

		string(2)='♅  ♓  ♃  ♄  ☫  ♑  ♇  ☿  ♅'
		length(2)=25;	row_num(2)=5

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(3)='✰ ✰ ✰ ✰ (1) Overview ✰ ✰ ✰ ✰'
			length(3)=28
		ELSE
			string(3)='(1) Overview'
			length(3)=12
		END IF
		row_num(3)=7

		IF ((manim .EQV. .TRUE.).AND.(wchoice==2)) THEN
			string(4)='✰ ✰ ✰ ✰ (2) Enemies ✰ ✰ ✰ ✰'
			length(4)=27
		ELSE
			string(4)='(2) Enemies'
			length(4)=11
		END IF
		row_num(4)=9

		IF ((manim .EQV. .TRUE.).AND.(wchoice==3)) THEN
			string(5)='✰ ✰ ✰ ✰ (3) Weapons ✰ ✰ ✰ ✰'
			length(5)=27
		ELSE
			string(5)='(3) Weapons'
			length(5)=11
		END IF
		row_num(5)=11

		IF ((manim .EQV. .TRUE.).AND.(wchoice==4)) THEN
			string(6)='✰ ✰ ✰ ✰ (4) Powerups ✰ ✰ ✰ ✰'
			length(6)=28
		ELSE
			string(6)='(4) Powerups'
			length(6)=12
		END IF
		row_num(6)=13

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(7)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(7)=29
		ELSE
			string(7)='(0) Back to Main Menu'
			length(7)=21
		END IF
		row_num(7)=row+3

		last=7

	! - - - - - - - GUIDE A - - - - - - - GUIDE A - - - - - - -
	CASE('Guide_a1')	!Guide_a1
		string(1)='♓           ♅'
		length(1)=13;	row_num(1)=3

		string(2)='    ₪'
		length(2)=5;	row_num(2)=4

		string(3)='✥         ☾☪☽'
		length(3)=13;	row_num(3)=5

		string(4)='♅      ♑        ♓'
		length(4)=17;	row_num(4)=7

		string(5)='          ◈     ✣'
		length(5)=17;	row_num(5)=9

		string(6)='♅   ♅   ⁙       ♅'
		length(6)=17;	row_num(6)=11

		string(7)='❈'
		length(7)=1;	row_num(7)=13

		string(8)='☋'
		length(8)=1;	row_num(8)=15

		string(9)='In Space Attack you must protect your colony'
		length(9)=44;	row_num(9)=17

		string(10)='from the invading space aliens!'
		length(10)=31;	row_num(10)=18

		IF (manim .EQV. .FALSE.) THEN
			string(11)='Press enter to continue ➤'
		ELSE
			string(11)='Press enter to continue ➢'
		END IF
		length(11)=25;	row_num(11)=row+3

		last=11

	CASE('Guide_a2')	!Guide_a2
		string(1)='♅    →    ♅    →    ♅    ↴'
		length(1)=26;	row_num(1)=3

		string(2)='                         ↓'
		length(2)=26;	row_num(2)=5

		string(3)='♅    ←    ♅    ←    ♅    ↲'
		length(3)=26;	row_num(3)=7

		string(4)='↓                        ′'
		length(4)=26;	row_num(4)=9

		string(5)='♅    →    ♅    →    ♅    ↴'
		length(5)=26;	row_num(5)=11

		string(6)='Enemy invaders will snake their way'
		length(6)=35;	row_num(6)=15

		string(7)='down the screen, trying to reach the'
		length(7)=36;	row_num(7)=16

		string(8)='bottom right corner.'
		length(8)=20;	row_num(8)=17

		IF (manim .EQV. .FALSE.) THEN
			string(9)='Press enter to continue ➤'
		ELSE
			string(9)='Press enter to continue ➢'
		END IF
		length(9)=25;	row_num(9)=row+3

		last=9

	CASE('Guide_a3')	!Guide_a3
		string(1)='LIVES:♥ ♡'
		length(1)=9;	row_num(1)=8

		string(2)='♅    →    ♅    →    ♅    ✘'
		length(2)=26;	row_num(2)=11

		string(3)='Each time an invader reaches the bottom'
		length(3)=39;	row_num(3)=15

		string(4)='right you will lose one life.'
		length(4)=29;	row_num(4)=16

		IF (manim .EQV. .FALSE.) THEN
			string(5)='Press enter to continue ➤'
		ELSE
			string(5)='Press enter to continue ➢'
		END IF
		length(5)=25;	row_num(5)=row+3

		last=5

	CASE('Guide_a4')	!Guide_a4
		string(1)='⁖    ⁙      ⁛       ∵'
		length(1)=21;	row_num(1)=3

		string(2)='∵    ∗        ჻     ⁙    ⁛'
		length(2)=26;	row_num(2)=5

		string(3)='⁙    ⁛   ∴          ※    ∵'
		length(3)=26;	row_num(3)=7

		string(4)='⁖         ⁙         ⁖'
		length(4)=21;	row_num(4)=9

		string(5)='∵           ∴'
		length(5)=13;	row_num(5)=11

		string(6)='∷    ※          ⁛        ⁙'
		length(6)=26;	row_num(6)=13

		string(7)='If you run out of lives . . .'
		length(7)=29;	row_num(7)=16

		string(8)='Game over! The colony is destroyed.'
		length(8)=35;	row_num(8)=17

		IF (manim .EQV. .FALSE.) THEN
			string(9)='Press enter to continue ➤'
		ELSE
			string(9)='Press enter to continue ➢'
		END IF
		length(9)=25;	row_num(9)=row+3

		last=9

	CASE('Guide_a5')	!Guide_a5
		string(1)='⁂'
		length(1)=1;	row_num(1)=4

		string(2)='❈'
		length(2)=1;	row_num(2)=6

		string(3)='☋'
		length(3)=1;	row_num(3)=8

		string(4)='▪▪▪▪▪▪▫▫▫'
		length(4)=9;	row_num(4)=10

		string(5)='↪ ▫▫▫▫▫▫▫▫▫'
		length(5)=11;	row_num(5)=11

		string(6)='To fight back you can blast away with your'
		length(6)=42;	row_num(6)=14

		string(7)='ship lasers and special ordinance.  But be'
		length(7)=42;	row_num(7)=15

		string(8)='careful, if you run out of weapon charge'
		length(8)=40;	row_num(8)=16

		string(9)='you will have to wait until you recharge'
		length(9)=40;	row_num(9)=17

		string(10)='to fire again.'
		length(10)=14;	row_num(10)=18

		IF (manim .EQV. .FALSE.) THEN
			string(11)='Press enter to continue ➤'
		ELSE
			string(11)='Press enter to continue ➢'
		END IF
		length(11)=25;	row_num(11)=row+3

		last=11


	CASE('Guide_a6')	!Guide_a6
		string(1)='✪ SCORE:00120'
		length(1)=13;	row_num(1)=3

		string(2)='♅       →      ⁛    +10 Points!'
		length(2)=31;	row_num(2)=5

		string(3)='❈'
		length(3)=1;	row_num(3)=8

		string(4)='☋'
		length(4)=1;	row_num(4)=11

		string(5)='While you are fending off the alien invasion'
		length(5)=44;	row_num(5)=14

		string(6)='you will be getting scored on your'
		length(6)=34;	row_num(6)=15

		string(7)='performance.  Your final score will depend'
		length(7)=42;	row_num(7)=16

		string(8)='on the number and type of enemies killed'
		length(8)=40;	row_num(8)=17

		string(9)='as well as your accuracy.'
		length(9)=25;	row_num(9)=18

		IF (manim .EQV. .FALSE.) THEN
			string(10)='Return to Guide ➤'
		ELSE
			string(10)='Return to Guide ➢'
		END IF
		length(10)=17;	row_num(10)=row+3

		last=10

	! - - - - - - - GUIDE B - - - - - - - GUIDE B - - - - - - -
	CASE('Guide_b1')	!Guide_b1
		string(1)='Invader'
		length(1)=7;	row_num(1)=3

		string(2)='♅'
		length(2)=1;	row_num(2)=4

		string(3)='1x♥     10x✪'
		length(3)=12;	row_num(3)=5

		string(4)='Most basic, mindless invader.'
		length(4)=29;	row_num(4)=6

		string(5)='Usually found in large swarms.'
		length(5)=30;	row_num(5)=7

		string(6)='Skirmisher'
		length(6)=10;	row_num(6)=9

		string(7)='♓'
		length(7)=1;	row_num(7)=10

		string(8)='1x♥     25x✪'
		length(8)=12;	row_num(8)=11

		string(9)='A step up from the Invader.'
		length(9)=27;	row_num(9)=12

		string(10)='Intermittently fires plasma.'
		length(10)=28;	row_num(10)=13

		string(11)='Bomber'
		length(11)=6;	row_num(11)=15

		string(12)='☫'
		length(12)=1;	row_num(12)=16

		string(13)='2x♥     75x✪'
		length(13)=12;	row_num(13)=17

		string(14)='Heavy firepower. Infrequently'
		length(14)=29;	row_num(14)=18

		string(15)='fires a triple blast of plasma.'
		length(15)=31;	row_num(15)=19	

		IF (manim .EQV. .FALSE.) THEN
			string(16)='Press enter to continue ➤'
		ELSE
			string(16)='Press enter to continue ➢'
		END IF
		length(16)=25;	row_num(16)=row+3

		last=16

	CASE('Guide_b2')	!Guide_b2
		string(1)='Gunner'
		length(1)=6;	row_num(1)=3

		string(2)='⇼'
		length(2)=1;	row_num(2)=4

		string(3)='2x♥    110x✪'
		length(3)=12;	row_num(3)=5

		string(4)='A fearsome opponent.'
		length(4)=20;	row_num(4)=6

		string(5)='Relentlessly launches plasma.'
		length(5)=29;	row_num(5)=7

		string(6)='Shielder'
		length(6)=8;	row_num(6)=9

		string(7)='۞'
		length(7)=1;	row_num(7)=10

		string(8)='3x♥    160x✪'
		length(8)=12;	row_num(8)=11

		string(9)='Heavilly armored and projects a'
		length(9)=31;	row_num(9)=12

		string(10)='small force field below itself.'
		length(10)=31;	row_num(10)=13

		string(11)='Warper'
		length(11)=6;	row_num(11)=15

		string(12)='♑'
		length(12)=1;	row_num(12)=16

		string(13)='1x♥     40x✪'
		length(13)=12;	row_num(13)=17

		string(14)='A small, agile foe. Capable of'
		length(14)=30;	row_num(14)=18

		string(15)='short range warp jumping.'
		length(15)=25;	row_num(15)=19	

		IF (manim .EQV. .FALSE.) THEN
			string(16)='Press enter to continue ➤'
		ELSE
			string(16)='Press enter to continue ➢'
		END IF
		length(16)=25;	row_num(16)=row+3

		last=16

	CASE('Guide_b3')	!Guide_b3
		string(1)='Carrier'
		length(1)=7;	row_num(1)=3

		string(2)='⋤'
		length(2)=1;	row_num(2)=4

		string(3)='4x♥    300x✪'
		length(3)=12;	row_num(3)=5

		string(4)='Destroy this ship immediately.'
		length(4)=30;	row_num(4)=6

		string(5)='Warps invaders into surrounding space.'
		length(5)=38;	row_num(5)=7

		string(6)='Mothership'
		length(6)=10;	row_num(6)=9

		string(7)='ↂ'
		length(7)=1;	row_num(7)=10

		string(8)='7x♥    650x✪'
		length(8)=12;	row_num(8)=11

		string(9)='A fiercely powerful opponent which'
		length(9)=34;	row_num(9)=12

		string(10)='is vital to the invasion.'
		length(10)=25;	row_num(10)=13

		string(11)='Equipped with plasma turrets and'
		length(11)=32;	row_num(11)=15

		string(12)='capable of heavy bombardment.'
		length(12)=29;	row_num(12)=16

		string(13)='Also warps invaders into the fight.'
		length(13)=35;	row_num(13)=17

		string(14)='Do not hesitate to obliterate this ship'
		length(14)=39;	row_num(14)=19

		string(15)='with all of your special ordinance.'
		length(15)=35;	row_num(15)=20	

		IF (manim .EQV. .FALSE.) THEN
			string(16)='Return to Guide ➤'
		ELSE
			string(16)='Return to Guide ➢'
		END IF
		length(16)=17;	row_num(16)=row+3

		last=16

	! - - - - - - - GUIDE C - - - - - - - GUIDE C - - - - - - -
	CASE('Guide_c1')	!Guide_c1
		string(1)='Laser'
		length(1)=5;	row_num(1)=3

		string(2)='❈ ❉ ❊'
		length(2)=5;	row_num(2)=4

		string(3)='1 Damage'
		length(3)=8;	row_num(3)=5

		string(4)='Uses 3 weapon charge.'
		length(4)=21;	row_num(4)=7

		string(5)='▪▪▪▪▪▪▪▪▪'
		length(5)=9;	row_num(5)=8

		string(6)='↪ ▪▪▪▪▪▪▫▫▫'
		length(6)=11;	row_num(6)=9

		string(7)='Scattershot'
		length(7)=11;	row_num(7)=12

		string(8)='⁂ ⁂ ⁂'
		length(8)=5;	row_num(8)=13

		string(9)='1 Damage'
		length(9)=8;	row_num(9)=14

		string(10)='Pierces through the first target.'
		length(10)=33;	row_num(10)=16

		string(11)='✫ Special Ordinance ✫'
		length(11)=21;	row_num(11)=17

		IF (manim .EQV. .FALSE.) THEN
			string(12)='Press enter to continue ➤'
		ELSE
			string(12)='Press enter to continue ➢'
		END IF
		length(12)=25;	row_num(12)=row+3

		last=12

	CASE('Guide_c2')	!Guide_c2
		string(1)='Vaporizer'
		length(1)=9;	row_num(1)=3

		string(2)='✸ ✹ ✺'
		length(2)=5;	row_num(2)=4

		string(3)='3 Damage'
		length(3)=8;	row_num(3)=5

		string(4)='High powered attack.'
		length(4)=20;	row_num(4)=7

		string(5)='✫ Special Ordinance ✫'
		length(5)=21;	row_num(5)=8

		string(6)='Missile'
		length(6)=7;	row_num(6)=11

		string(7)='❆ ❂ ☢'
		length(7)=5;	row_num(7)=12

		string(8)='2 Damage'
		length(8)=8;	row_num(8)=13

		string(9)='3x3 Area of Effect.'
		length(9)=19;	row_num(9)=15

		string(10)='✫ Special Ordinance ✫'
		length(10)=21;	row_num(10)=16

		IF (manim .EQV. .FALSE.) THEN
			string(11)='Return to Guide ➤'
		ELSE
			string(11)='Return to Guide ➢'
		END IF
		length(11)=17;	row_num(11)=row+3

		last=12

	! - - - - - - - GUIDE D - - - - - - - GUIDE D - - - - - - -
	CASE('Guide_d1')	!Guide_d1
		string(1)='Powerups will occasionally move through'
		length(1)=39;	row_num(1)=5

		string(2)='the battlefield, protected by their casing.'
		length(2)=43;	row_num(2)=6

		string(3)='If you destroy the casing before the invaders'
		length(3)=45;	row_num(3)=8

		string(4)='you will reap the benefits inside.'
		length(4)=34;	row_num(4)=9

		string(5)='An encased powerup looks like this:'
		length(5)=35;	row_num(5)=11

		string(6)='◆ ◈ ◇'
		length(6)=5;	row_num(6)=12

		string(7)='2x♥    100x✪'
		length(7)=12;	row_num(7)=13

		IF (manim .EQV. .FALSE.) THEN
			string(8)='Press enter to continue ➤'
		ELSE
			string(8)='Press enter to continue ➢'
		END IF
		length(8)=25;	row_num(8)=row+3

		last=8

	CASE('Guide_d2')	!Guide_d2

		string(1)='Destroyed by Invaders'
		length(1)=21;	row_num(1)=3

		string(2)='☾✖☽'
		length(2)=3;	row_num(2)=4

		string(3)='Life Boost'
		length(3)=10;	row_num(3)=6

		string(4)='☾♥☽'
		length(4)=3;	row_num(4)=7

		string(5)='Gives +1 additional ♥'
		length(5)=21;	row_num(5)=8

		string(6)='Score Boost'
		length(6)=11;	row_num(6)=10

		string(7)='☾✪☽'
		length(7)=3;	row_num(7)=11

		string(8)='Temporarily doubles the points earned'
		length(8)=37;	row_num(8)=12

		string(9)='Ammo Boost'
		length(9)=10;	row_num(9)=14

		string(10)='☾☢☽'
		length(10)=3;	row_num(10)=15

		string(11)='Bonus ✫ Special Ordinance ✫'
		length(11)=27;	row_num(11)=16

		string(12)='Damage Boost'
		length(12)=12;	row_num(12)=18

		string(13)='☾⋇☽'
		length(13)=3;	row_num(13)=19

		string(14)='Temporarily doubles standard laser damage'
		length(14)=41;	row_num(14)=20

		IF (manim .EQV. .FALSE.) THEN
			string(15)='Return to Guide ➤'
		ELSE
			string(15)='Return to Guide ➢'
		END IF
		length(15)=17;	row_num(15)=row+3

		last=15


	END SELECT menu_selector

RETURN
END SUBROUTINE select_menu

! ------- Which Show ------- Which Show ------- Which Show ------- Which Show -------
! This block is used to flag which scorings are shown and which aren't.  It also handles the rapid counting
! of points to simulate scrolling up to the scoring.
SUBROUTINE which_show(wshow,int_addcounter,float_addcounter,kills,shots,hits,score,difficulty,finalscore,&
								accuracy,accuracy_mod,difficulty_mod,ptime,pcount)
	IMPLICIT NONE
	LOGICAL, DIMENSION(2,15) :: wshow
	INTEGER :: k, int_addcounter, ptime, pcount
	INTEGER :: kills, shots, hits, score, difficulty, finalscore
	REAL :: float_addcounter, accuracy, accuracy_mod, difficulty_mod

k=1
DO WHILE (wshow(1,k) .EQV. .TRUE.)	!bring k up to proper index
k=k+1
END DO
IF (k==1) ptime=25			!account for first Win/Lose title

g_wshow: SELECT CASE(k-1)
	CASE(1)							!❈ SHOTS:
		IF (int_addcounter<shots) THEN
			IF (shots<=90) THEN
				ptime=3			!set number of pauses to 3
			ELSE
				ptime=2
			END IF
			IF (int_addcounter+2<shots) THEN
				int_addcounter=int_addcounter+2	!increase counter for printing
			ELSE
				int_addcounter=shots
				ptime=25		!set number of pauses to 10
			END IF
		ELSE
			int_addcounter=0	!reset counter to prime for next showing
			float_addcounter=0.
			wshow(1,k)=.TRUE.	!flag next showing
			wshow(2,k-1)=.TRUE.	!flag static full current
		END IF
	CASE(2)							!☄ HITS:
		IF (int_addcounter<hits) THEN
			IF (hits<=90) THEN
				ptime=3
			ELSE
				ptime=2
			END IF
			IF (int_addcounter+2<hits) THEN
				int_addcounter=int_addcounter+2
			ELSE
				int_addcounter=hits
				ptime=25
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(3)							!⌖ ACCURACY:
		IF (float_addcounter<accuracy) THEN
			ptime=3
			IF (float_addcounter+5<accuracy) THEN
				float_addcounter=float_addcounter+5.
			ELSE
				float_addcounter=accuracy
				ptime=25
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(4)							!☠ KILLS:
		IF (int_addcounter<kills) THEN
			IF (kills<=90) THEN
				ptime=3
			ELSE
				ptime=2
			END IF
			IF (int_addcounter+2<kills) THEN
				int_addcounter=int_addcounter+2
			ELSE
				int_addcounter=kills
				ptime=25
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(5)							!✪ SCORE:
		IF (int_addcounter<score) THEN
			ptime=1
			IF (int_addcounter+55<score) THEN
				int_addcounter=int_addcounter+55
			ELSE
				int_addcounter=score
				ptime=18
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(6)							!╳
		IF (int_addcounter<3) THEN
			ptime=4
			int_addcounter=int_addcounter+1
			IF (int_addcounter==2) ptime=18
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.	
		END IF
	CASE(7)							!⌖ ACCURACY MOD:
		IF (float_addcounter<accuracy_mod) THEN
			ptime=3
			IF (float_addcounter+0.15<accuracy_mod) THEN
				float_addcounter=float_addcounter+0.15
			ELSE
				float_addcounter=accuracy_mod
				ptime=18
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(8)							!╳
		IF (int_addcounter<3) THEN
			ptime=4
			int_addcounter=int_addcounter+1
			IF (int_addcounter==2) ptime=18
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.	
		END IF
	CASE(9)							!⌖☠ DIFFICULTY MOD:
		IF (float_addcounter<difficulty_mod) THEN
			ptime=3
			IF (float_addcounter+0.15<difficulty_mod) THEN
				float_addcounter=float_addcounter+0.15
			ELSE
				float_addcounter=difficulty_mod
				ptime=18
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(10)						!═
		wshow(1,k)=.TRUE.
		wshow(2,k-1)=.TRUE.
	CASE(11)						!✪ FINAL SCORE:
		IF (int_addcounter<finalscore) THEN
			ptime=1
			IF (int_addcounter+15<finalscore) THEN
				int_addcounter=int_addcounter+15
			ELSE
				int_addcounter=finalscore
				ptime=20
			END IF
		ELSE
			int_addcounter=0
			float_addcounter=0.
			wshow(1,k)=.TRUE.
			wshow(2,k-1)=.TRUE.
		END IF
	CASE(12)
		wshow(1,k)=.TRUE.
		wshow(1,k+1)=.TRUE.
		wshow(2,k-1)=.TRUE.
!	CASE(13)
!		wshow(1,13)=.TRUE.
!		wshow(1,14)=.TRUE.
	CASE DEFAULT
		ptime=3	
	END SELECT g_wshow

RETURN
END SUBROUTINE which_show

!------- SELECT MENU ------- SELECT MENU ------- SELECT MENU -------
! This block is used to load the appropriate strings before printing.  It uses "wmenu"
! and "wchoice" to determine the correct strings to load.
SUBROUTINE select_wshow(row,col,string,length,row_num,last,wshow,int_addcounter,float_addcounter,kills,shots,&
			hits,score,difficulty,finalscore,accuracy,accuracy_mod,difficulty_mod,endgame,maxgrade,ptime)
	IMPLICIT NONE
	CHARACTER(3*col), DIMENSION(row) :: string
	CHARACTER(LEN=5) :: Lint_addcounter
	CHARACTER(LEN=16) :: Lgrading
	INTEGER, intent(in) :: row, col
	INTEGER :: length(row), row_num(row), last, grading, maxgradedivision, maxgrade, k
	LOGICAL, DIMENSION(2,15) :: wshow
	INTEGER :: int_addcounter, kills, shots, hits, score, difficulty, finalscore, endgame, ptime
	REAL :: float_addcounter, accuracy, accuracy_mod, difficulty_mod

k=1
		IF (endgame==0) THEN
			string(k)='☠ You lose!☠'
			length(k)=12
		ELSE
			string(k)='✰ You Win!✰'
			length(k)=11
		END IF
		row_num(k)=3; k=k+1


		IF (wshow(1,1) .EQV. .FALSE.) THEN		!❈ SHOTS:
			string(k)=''			!Blank row if not flagged for animation
			length(k)=0
		ELSE
			IF (wshow(2,1) .EQV. .FALSE.) THEN	!flagged for animation
				WRITE(Lint_addcounter,10) int_addcounter	!Convert int_addcounter to a string
			ELSE
				WRITE(Lint_addcounter,10) shots			!Convert shots to string
			END IF
			string(k)='❈ SHOTS:' // Lint_addcounter
			length(k)=11
		END IF
		row_num(k)=5; k=k+1
		10 FORMAT(I3.3)

		IF (wshow(1,2) .EQV. .FALSE.) THEN		!☄ HITS:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,2) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,10) int_addcounter
			ELSE
				WRITE(Lint_addcounter,10) hits
			END IF
			string(k)='☄ HITS:' // Lint_addcounter
			length(k)=10
		END IF
		row_num(k)=6; k=k+1

		IF (wshow(1,3) .EQV. .FALSE.) THEN		!⌖ ACCURACY:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,3) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,20) float_addcounter
			ELSE
				WRITE(Lint_addcounter,20) accuracy
			END IF
			string(k)='⌖ ACCURACY:' // Lint_addcounter // '%'
			length(k)=17
		END IF
		row_num(k)=7; k=k+1
		20 FORMAT(F4.1)

		IF (wshow(1,4) .EQV. .FALSE.) THEN		!☠ KILLS:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,4) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,10) int_addcounter
			ELSE
				WRITE(Lint_addcounter,10) kills
			END IF
			string(k)='☠ KILLS:' // Lint_addcounter
			length(k)=11
		END IF
		row_num(k)=8; k=k+1

		IF ((wshow(1,4) .EQV. .TRUE.).AND.(wshow(2,4) .EQV. .TRUE.)) THEN
			string(k)='-----------------------------------'
			length(k)=35
			string(k+1)=')()()()()()()()()()()()()()()()()('
			length(k+1)=34
			string(k+2)='-----------------------------------'
			length(k+2)=35
		ELSE
			string(k:k+2)=''; length(k:k+2)=0
		END IF
		row_num(k)=9; row_num(k+1)=10; row_num(k+2)=11; k=k+3

		IF (wshow(1,5) .EQV. .FALSE.) THEN		!✪ SCORE:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,5) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,30) int_addcounter
			ELSE
				WRITE(Lint_addcounter,30) score
			END IF
			string(k)='✪ SCORE:' // Lint_addcounter
			length(k)=13
		END IF
		row_num(k)=12; k=k+1
		30 FORMAT(I5.5)

		IF (wshow(1,6) .EQV. .FALSE.) THEN		!╳
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,6) .EQV. .FALSE.) THEN
				IF (int_addcounter<2) THEN
					string(k)='╱'
				ELSE
					string(k)='╳'
				END IF
			ELSE
				string(k)='╳'
			END IF
			length(k)=1
		END IF
		row_num(10)=13; k=k+1

		IF (wshow(1,7) .EQV. .FALSE.) THEN		!⌖ ACCURACY MOD:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,7) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,40) float_addcounter
			ELSE
				WRITE(Lint_addcounter,40) accuracy_mod
			END IF
			string(k)='⌖ ACCURACY MOD:' // Lint_addcounter
			length(k)=19
		END IF
		row_num(k)=14; k=k+1
		40 FORMAT(F4.2)

		IF (wshow(1,8) .EQV. .FALSE.) THEN		!╳
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,8) .EQV. .FALSE.) THEN
				IF (int_addcounter<2) THEN
					string(k)='╱'
				ELSE
					string(k)='╳'
				END IF
			ELSE
				string(k)='╳'
			END IF
			length(k)=1
		END IF
		row_num(k)=15; k=k+1

		IF (wshow(1,9) .EQV. .FALSE.) THEN		!☠ DIFFICULTY MOD:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,9) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,40) float_addcounter
			ELSE
				WRITE(Lint_addcounter,40) difficulty_mod
			END IF
			string(k)='☠ DIFFICULTY MOD:' // Lint_addcounter
			length(k)=21
		END IF
		row_num(k)=16; k=k+1

		IF (wshow(1,10) .EQV. .FALSE.) THEN		!═
			string(k)=''
			length(k)=0
		ELSE
			string(k)='═'
			length(k)=1
		END IF
		row_num(k)=17; k=k+1

		IF (wshow(1,11) .EQV. .FALSE.) THEN		!✪ FINAL SCORE:
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,11) .EQV. .FALSE.) THEN
				WRITE(Lint_addcounter,30) int_addcounter
			ELSE
				WRITE(Lint_addcounter,30) finalscore
			END IF
			string(k)='✪ FINAL SCORE:' // Lint_addcounter
			length(k)=19
		END IF
		row_num(k)=18; k=k+1

		IF (wshow(1,12) .EQV. .FALSE.) THEN		!RANKING:
			string(k)=''
			length(k)=0
		ELSE
			maxgradedivision=maxgrade/24
			IF (finalscore<maxgrade/2) THEN
				Lgrading='F     Failure‼'
				grading=8
			ELSE IF (finalscore<maxgrade-maxgradedivision*11) THEN
				Lgrading='D-    Defeated!'
				grading=9
			ELSE IF (finalscore<maxgrade-maxgradedivision*10) THEN
				Lgrading='D     Deadbeat!'
				grading=9
			ELSE IF (finalscore<maxgrade-maxgradedivision*9) THEN
				Lgrading='D+    Deficient'
				grading=9
			ELSE IF (finalscore<maxgrade-maxgradedivision*8) THEN
				Lgrading='C-    Cruddy'
				grading=6
			ELSE IF (finalscore<maxgrade-maxgradedivision*7) THEN
				Lgrading='C     Crummy'
				grading=6
			ELSE IF (finalscore<maxgrade-maxgradedivision*6) THEN
				Lgrading='C+    Common'
				grading=6
			ELSE IF (finalscore<maxgrade-maxgradedivision*5) THEN
				Lgrading='B-    Bravo!'
				grading=6
			ELSE IF (finalscore<maxgrade-maxgradedivision*4) THEN
				Lgrading='B     Boss!'
				grading=5
			ELSE IF (finalscore<maxgrade-maxgradedivision*3) THEN
				Lgrading='B+    Brilliant!'
				grading=10
			ELSE IF (finalscore<maxgrade-maxgradedivision*2) THEN
				Lgrading='A-    Adept‼'
				grading=6
			ELSE IF (finalscore<maxgrade-maxgradedivision*1) THEN
				Lgrading='A     Awesome‼'
				grading=8
			ELSE IF (finalscore<maxgrade) THEN
				Lgrading='A+    Ace‼' 
				grading=4
			ELSE
				Lgrading='S     Super‼'
				grading=6
			END IF
			string(k)=Lgrading
			length(k)=6+grading
		END IF
		row_num(k)=19; k=k+1

		IF (wshow(1,13) .EQV. .FALSE.) THEN		!High Score
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,13) .EQV. .TRUE.) THEN
				string(k)='✰ ✰ (1) View/Enter High Scores ✰ ✰'
				length(k)=34
			ELSE
				string(k)='(1) View/Enter High Scores'
				length(k)=26
			END IF
		END IF
		row_num(k)=21; k=k+1

		IF (wshow(1,14) .EQV. .FALSE.) THEN		!Back to Menu
			string(k)=''
			length(k)=0
		ELSE
			IF (wshow(2,14) .EQV. .TRUE.) THEN
				string(k)='✰ ✰ (0) Back to Main Menu ✰ ✰'
				length(k)=29
			ELSE
				string(k)='(0) Back to Main Menu'
				length(k)=21
			END IF
		END IF
		row_num(k)=22; k=k+1

		last=k-1

RETURN
END SUBROUTINE select_wshow
