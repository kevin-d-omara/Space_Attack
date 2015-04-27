! This block initializes all the relevant variables before loading the menu sequence.
SUBROUTINE menu_initialize(wmenu,wchoice,endgame,manim,rblank,shoot,right,left,scattershot,vaporizer,missile,&
							difficulty,gametype,num_scat,num_vape,num_miss,ordinancepoints,yesno)
	IMPLICIT NONE
	CHARACTER(10) :: wmenu, difficulty, gametype
	CHARACTER(1) ::	shoot, right, left, scattershot, vaporizer, missile
	INTEGER :: wchoice, endgame, num_scat, num_vape, num_miss, ordinancepoints, yesno
	LOGICAL :: rblank, manim, cheat1, cheat2

wmenu='Intro'	!set to intro screen
wchoice=1	!set to default choice
endgame=0	!set to game off
manim=.FALSE.	!set menu animation flag off
rblank=.TRUE.	!set to read blank (i.e. press enter, no key required)
 cheat1=.FALSE.	!set Year o' Plenty to De-Active
 cheat2=.FALSE. !set Fiver to De-Active
shoot='w'; right='d'; left='a'; scattershot='1'; vaporizer='2'; missile='3'	!set default controls
difficulty='Normal'	!set difficulty
gametype='Standard'	!set gametype
num_scat=2; num_vape=2; num_miss=1	!set starting Special Ordinance
ordinancepoints=9	!set "Normal" allotment of ordinance points
yesno=1		!set game to ON

RETURN
END SUBROUTINE menu_initialize

!------- WHICH OPTION ------- WHICH OPTION ------- WHICH OPTION -------
! This block takes "wchoice" and "wmenu" and a) which menu player is in and b) what choice they selected
! It then decides the appropriate course of action, either a) flag animation for selection or b) set wmenu
! to the next menu selected.
SUBROUTINE which_option(command,wchoice,wmenu,manim,rblank,shoot,right,left,scattershot,&
			vaporizer,missile,difficulty,gametype,num_vape,num_scat,num_miss,total,ordinancepoints,yesno,lives,cheat1,cheat2)
	IMPLICIT NONE
	CHARACTER(1) :: command, shoot, right, left, scattershot, vaporizer, missile
	CHARACTER(10) :: wmenu, difficulty, gametype
	INTEGER :: wchoice, num_scat, num_vape, num_miss, total, ordinancepoints, yesno, lives
	LOGICAL :: manim, rblank, cheat1, cheat2

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

	CASE('Cheats')
		IF (command=='1') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=1	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (cheat1 .EQV. .FALSE.) THEN
					cheat1=.TRUE.
					ordinancepoints=99	!Year o' Plenty Cheat
				ELSE
					cheat1=.FALSE.
					IF (difficulty=='Brutal') THEN	!set difficulty
						ordinancepoints=3	!set allotment of ordinance points
						num_scat=0; num_vape=0; num_miss=1
					ELSE IF (difficulty=='Easy') THEN
						ordinancepoints=15	!set allotment of ordinance points
						num_scat=2; num_vape=2; num_miss=3
					ELSE
						ordinancepoints=9	!set allotment of ordinance points
						num_scat=2; num_vape=2; num_miss=1
					END IF
				END IF
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				IF (cheat2 .EQV. .FALSE.) THEN
					cheat2=.TRUE.		!Fiver Cheat
				ELSE
					cheat2=.FALSE.
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
				IF (difficulty=='Normal') THEN	!set difficulty
					difficulty='Brutal'	!set Brutal
					ordinancepoints=3	!set allotment of ordinance points
					num_scat=0; num_vape=0; num_miss=1
				ELSE IF (difficulty=='Brutal') THEN
					difficulty='Easy'	!set Easy
					ordinancepoints=15	!set allotment of ordinance points
					num_scat=2; num_vape=2; num_miss=3
				ELSE
					difficulty='Normal'	!set Normal
					ordinancepoints=9	!set allotment of ordinance points
					num_scat=2; num_vape=2; num_miss=1
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
				IF (num_scat<ordinancepoints) num_scat=num_scat+1	!+1 scattershot
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='5') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=5	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_scat>0) num_scat=num_scat-1		!-1 scattershot
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='6') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=6	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_vape<1+ordinancepoints/2) num_vape=num_vape+1	!+1 vaporizer			
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='7') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=7	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_vape>0) num_vape=num_vape-1		!-1 vaporizer			
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='8') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=8	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_miss<ordinancepoints/3) num_miss=num_miss+1	!+1 missile			
			ELSE
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='9') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=9	!flag choice
				manim=.TRUE.	!flag animation on
				IF (num_miss>0) num_miss=num_miss-1		!-1 missile			
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
				shoot=command	!read "shoot" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='2') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=2	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				right=command	!read "right" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='3') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=3	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				left=command	!read "left" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='4') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=4	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				scattershot=command!read "scattershot" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='5') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=5	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				vaporizer=command!read "vaporizer" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='6') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=6	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				READ(*,*) command!read command
				missile=command	!read "missile" key
				manim=.FALSE.	!de-flag menu animation
			END IF
		ELSE IF (command=='9') THEN
			IF (manim .EQV. .FALSE.) THEN
				wchoice=9	!flag choice
				manim=.TRUE.	!flag animation on
			ELSE
				shoot='w'; right='d'; left='a'
				scattershot='1'; vaporizer='2'; missile='3'
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
SUBROUTINE select_menu(string,length,row_num,last,row,col,wmenu,wchoice,manim,shoot,right,left,&
		scattershot,vaporizer,missile,difficulty,gametype,num_vape,num_scat,num_miss,total,ordinancepoints,lives,cheat1,cheat2)
	IMPLICIT NONE
	CHARACTER(3*col), DIMENSION(row) :: string
	CHARACTER(10) :: wmenu, difficulty, gametype
	CHARACTER(1) :: shoot, right, left, scattershot, vaporizer, missile, Lnum_scat, Lnum_vape, Lnum_miss
	CHARACTER(LEN=2) :: Ltotal, Lordinancepoints
	INTEGER, intent(in) :: row, col
	INTEGER :: length(row), row_num(row), last, wchoice, k, num_scat, num_vape, num_miss,total, ordinancepoints, lives
	LOGICAL :: manim, cheat1, cheat2

menu_selector: SELECT CASE(wmenu)
	CASE('Intro')		!INTRO
		string(1)='♅  ♓  ☫  ℵ  ۞  ♑  ⋤  ↂ  ♅'
		length(1)=25;	row_num(1)=3

		string(2)='♅     Space  Attack     ♅'
		length(2)=25;	row_num(2)=4

		string(3)='♅  ↂ  ⋥  ♑  ۞  ℵ  ☫  ♓  ♅'
		length(3)=25;	row_num(3)=5

		IF (manim .EQV. .FALSE.) THEN
			string(4)='✭  ✭  ✭  ✭  ✭  ✭  ✭  ✭  ✭'
		ELSE
			string(4)='✰  ✰  ✰  ✰  ✰  ✰  ✰  ✰  ✰'
		END IF
		length(4)=25;	row_num(4)=7

		string(5)='Please adjust the height of your terminal to'
		length(5)=44;	row_num(5)=9

		string(6)='fit the border now.'
		length(6)=19;	row_num(6)=10

		string(7)='If you have not already done so, please exit'
		length(7)=44;	row_num(7)=12

		string(8)='and type: export OMP_NUM_THREADS=16'
		length(8)=35;	row_num(8)=13

		string(9)='Press any key to continue.'
		length(9)=26;	row_num(9)=15

		string(10)="by Kevin O'Mara"
		length(10)=15;	row_num(10)=20

		string(11)='version 1.9.1'
		length(11)=13;	row_num(11)=row+3

		last=11

	CASE('Main')		!MAIN
		string(1)='⋲     MAIN MENU     ⋺'
		length(1)=21;	row_num(1)=3

		string(2)='♅  ♓  ♃  ♄  ☫  ♑  ♇  ☿  ♅'
		length(2)=25;	row_num(2)=5

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(3)='✰ ✰ ✰ ✰ (1) PLAY! ✰ ✰ ✰ ✰'
			length(3)=25
		ELSE
			string(3)='(1) PLAY!'
			length(3)=9
		END IF
		row_num(3)=7

		IF ((manim .EQV. .TRUE.).AND.(wchoice==2)) THEN
			string(4)='✰ ✰ ✰ ✰ (2) Controls ✰ ✰ ✰ ✰'
			length(4)=28
		ELSE
			string(4)='(2) Controls'
			length(4)=12
		END IF
		row_num(4)=9

		IF ((manim .EQV. .TRUE.).AND.(wchoice==3)) THEN
			string(5)='✰ ✰ ✰ ✰ (3) Guide ✰ ✰ ✰ ✰'
			length(5)=25
		ELSE
			string(5)='(3) Guide'
			length(5)=9
		END IF
		row_num(5)=11

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(6)='✰ ✰ ✰ ✰ (0) Exit Game ✰ ✰ ✰ ✰'
			length(6)=29
		ELSE
			string(6)='(0) Exit Game'
			length(6)=13
		END IF
		row_num(6)=row+3

		last=6

	CASE('Cheats')		!CHEATS
		string(1)='⋲         Cheats        ⋺'
		length(1)=25;	row_num(1)=3

		IF (cheat1 .EQV. .FALSE.) THEN
			string(2)="(1) Year o' Plenty: De-Active"
			length(2)=29

		ELSE
			string(2)="(1) Year o' Plenty: Active"
			length(2)=26
		END IF
		row_num(2)=6

		IF (cheat2 .EQV. .FALSE.) THEN
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

		IF (difficulty=='Normal') THEN
			string(3)='(2) Difficulty: Normal'
			length(3)=22
		ELSE IF (difficulty=='Brutal') THEN
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

		total=num_scat+2*num_vape+3*num_miss	!total ordinance points remaining
		IF (ordinancepoints<10) THEN
			WRITE(Lordinancepoints,10) ordinancepoints	!record ordinance points
			k=1	!numer of columns occupied by Lordinancepoints
		ELSE
			WRITE(Lordinancepoints,20) ordinancepoints
			k=2
		END IF

		IF (total<=ordinancepoints) THEN
			WRITE(Ltotal,10) ordinancepoints-total
			string(6)=trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
			length(6)=19+k
		ELSE IF (total<=ordinancepoints+9) THEN
			WRITE(Ltotal,10) total-ordinancepoints
			IF (wchoice==1) THEN		!if trying to start game and poitns are over -> flag
				string(6)='⇶ -' // trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=22+k
			ELSE
				string(6)='-' // trim(Ltotal) // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=20+k
			END IF
		ELSE
			WRITE(Ltotal,20) total-ordinancepoints
			IF (wchoice==1) THEN		!if trying to start game and poitns are over -> flag
				string(6)='⇶ -' // Ltotal // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=23+k
			ELSE
				string(6)='-' // Ltotal // '/' // trim(Lordinancepoints) // ' Ordinance Points'
				length(6)=21+k
			END IF
		END IF
		row_num(6)=10

		10 FORMAT(i1); 20 FORMAT(i2)
		WRITE(Lnum_scat,10) num_scat
		string(7)='⁂  Scattershot [1 Point]: ' // Lnum_scat
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

		WRITE(Lnum_vape,10) num_vape
		string(9)='✹  Vaporizer [2 Points]: ' // Lnum_vape
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

		WRITE(Lnum_miss,10) num_miss
		string(11)='☢  Missile [2 Points]: ' // Lnum_miss
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
		string(1)='⋲      CONTROLS     ⋺'
		length(1)=21;	row_num(1)=3

		string(2)='Select a Command below to enter a new key.'
		length(2)=42;	row_num(2)=5

		IF ((manim .EQV. .TRUE.).AND.(wchoice==1)) THEN
			string(3)='(1) Shoot: ' // shoot // '→ _'
			length(3)=15
		ELSE
			string(3)='(1) Shoot: ' // shoot
			length(3)=12
		END IF
		row_num(3)=7

		IF ((manim .EQV. .TRUE.).AND.(wchoice==2)) THEN
			string(4)='(2) Right: ' // right // '→ _'
			length(4)=15
		ELSE
			string(4)='(2) Right: ' // right
			length(4)=12
		END IF
		row_num(4)=8

		IF ((manim .EQV. .TRUE.).AND.(wchoice==3)) THEN
			string(5)='(3) Left: ' // left // '→ _'
			length(5)=14
		ELSE
			string(5)='(3) Left: ' // left
			length(5)=11
		END IF
		row_num(5)=9

		string(6)='✫ Special Ordinance ✫'
		length(6)=21
		row_num(6)=11

		IF ((manim .EQV. .TRUE.).AND.(wchoice==4)) THEN
			string(7)='(4) Scattershot: ' // scattershot // '→ _'
			length(7)=21
		ELSE
			string(7)='(4) Scattershot: ' // scattershot
			length(7)=18
		END IF
		row_num(7)=12

		IF ((manim .EQV. .TRUE.).AND.(wchoice==5)) THEN
			string(8)='(5) Vaporizer: ' // vaporizer // '→ _'
			length(8)=19
		ELSE
			string(8)='(5) Vaporizer: ' // vaporizer
			length(8)=16
		END IF
		row_num(8)=13

		IF ((manim .EQV. .TRUE.).AND.(wchoice==6)) THEN
			string(9)='(6) Missile: ' // missile // '→ _'
			length(9)=17
		ELSE
			string(9)='(6) Missile: ' // missile
			length(9)=14
		END IF
		row_num(9)=14

		IF ((manim .EQV. .TRUE.).AND.(wchoice==9)) THEN
			string(10)='✰ ✰ ✰ (9) Reset Controls ✰ ✰ ✰'
			length(10)=30
		ELSE
			string(10)='(9) Reset Controls'
			length(10)=18
		END IF
		row_num(10)=row+1

		IF ((manim .EQV. .TRUE.).AND.(wchoice==0)) THEN
			string(11)='✰ ✰ (0) Back to Main Menu ✰ ✰'
			length(11)=29
		ELSE
			string(11)='(0) Back to Main Menu'
			length(11)=21
		END IF
		row_num(11)=row+3

		last=11

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

		string(2)='ℵ'
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

		string(8)='9x♥    650x✪'
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
