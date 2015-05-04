!This whole file is dedicated to the major cogs that make Space Attack run.  This includes four primary
!subroutines as well as several secondary subroutines which are called upon by the primary subroutines.
!	PRIMARY SUBROUTINES
!	1) move_invader			!moves invaders & handles special abilities
!	2) move_powerup			!moves powerups
!	3) move_laser			!moves player lasers and handles collisions
!	4) move_enemy_laser		!moves enemy lasers and handles collisions
!
!	SECONDARY SUBROUTINES
!	1) projectile_type_i		!deals with collisions laser->invader
!	2) projectile_type_p		!deals with collisions laser->powerup
!	3) death_animation		!determines invader type->flags death & adds score
!	4) powerup_death_animation	!determines powerup type->flags death & triggers bonus
!
!	TYPE			ANIMATION INDEX		FLAG TIMING (before or after fill_animation)
!	Invader			+10,+20,+30,...		
!	Killed Invader		-10,-20,-30,...		pre
!	Powerup			+200, 	
!	Destroyed Powerup	-200,-210,-220,...	pre
!	Shield			+290
!	Destroyed Shield	-290
!	Player Laser		-1,-2,-3,...		
!	Enemy Laser		+1,+2,+3,...		
!	Explosion		601,602,603		pre
!	Warp			650			pre
!	Player			-777	
!	Life Loss Animation	+999
!	Loser Animation		+1000, +1001,...
!	Next Wave Animation	+2000, +2001,...
!	Combination Flag	-1010, -2010,...	pre
!	Kill Gcounter FLag	+6666
!				(-1)=player projectile; (1)=enemy projectile

!-------Gauntlet-------Gauntlet-------Gauntlet-------Gauntlet-------Gauntlet-------Gauntlet-------
SUBROUTINE gauntlet(row,col,invader,powerup,spawn,select_spawn,gcounter,x00,rate)
	IMPLICIT NONE
	REAL :: u, v, rate(8)
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION(row,col) :: invader, powerup
	INTEGER :: spawn(21), v_int, gcounter, k, x00
	LOGICAL :: select_spawn

IF (select_spawn .EQV. .TRUE.) THEN		!select spawn package
	CALL random_number(u)
	IF (u>.95+.41*rate(1)) THEN 	!1/25	4% chance to spawn package (3.5% Easy & 4.5% Brutal)
		CALL random_number(v)
		v_int=100*v
		package: SELECT CASE(v_int)
			CASE(0:10)		!10x Invaders
				spawn(1:10)=(/ (11, k=1,10) /)
				spawn(15)=6666				!6666 is end of package flag
			CASE(11:20)		!Invaders every other
				DO k=1,20,2
					spawn(k)=11
				END DO
			CASE(21:25)		! 5x Skirmisher
				DO k=1,20,4
					spawn(k)=21
				END DO
			CASE(26:30)		! 3x Bomber
				DO k=1,18,6
					spawn(k)=32
				END DO
			CASE(31:35)		! 3x Gunner
				DO k=1,18,6
					spawn(k)=42
				END DO
			CASE(36:40)		! 5x Warper
				DO k=1,20,4
					spawn(k)=61
				END DO
			CASE(41:45)		! 1x Shielder
				spawn(1)=52
				spawn(15)=6666
			CASE(46:50)		! 1x Carrier
				spawn(1)=74
				spawn(15)=6666
			CASE(51:55)		! 1x Shield escorting 4x Invaders
				spawn(1:2)=(/ (11, k=1,2) /)
				spawn(3)=52
				spawn(4:5)=(/ (11, k=1,2) /)
			CASE(56:60)		! 1x Carrier escorted by 2x Skirmishers
				spawn(1)=21; spawn(5)=21
				spawn(3)=74
			CASE(61:65)		! 3x Skirmisher and 5x Warper
				DO k=1,15,5
					spawn(k)=21
				END DO
				DO k=2,20,4
					spawn(k)=61
				END DO
			CASE(66:70)		! 10x Invader w/ random Warper
				DO k=1,10
					CALL random_number(u)
					IF (u<.201) THEN
						spawn(k)=61
					ELSE
						spawn(k)=11
					END IF
				END DO
			CASE(71:75)		! 10x Invader w/ random Skirmisher
				DO k=1,10
					CALL random_number(u)
					IF (u<.225) THEN
						spawn(k)=21
					ELSE
						spawn(k)=11
					END IF
				END DO
			CASE(76:81)		! 1x Invader
				spawn(1)=11
				spawn(2)=6666
			CASE(82:84)		! 1x Skirmisher
				spawn(1)=21
				spawn(2)=6666
			CASE(85:87)		! 1x Bomber
				spawn(1)=32
				spawn(2)=6666
			CASE(88:90)		! 1x Gunner
				spawn(1)=42
				spawn(2)=6666
			CASE(91:93)		! 1x Warper
				spawn(1)=61
				spawn(2)=6666
			CASE(94:95)		! 1x Shielder
				spawn(1)=52
				spawn(2)=6666
			CASE(96:97)		! 1x Carrier
				spawn(1)=74
				spawn(2)=6666
			CASE(98:100)		! 1x Mothership (get fucked!)
				spawn(1)=87
			END SELECT package
		select_spawn=.FALSE.	!trigger spawn sequence and pause select package
	END IF
ELSE						!spawning sequence
	gcounter=gcounter+1
	IF ((spawn(gcounter)/=6666).AND.(gcounter/=21)) THEN
	x00=spawn(gcounter)
	ELSE
		spawn=0
		gcounter=0
		select_spawn=.TRUE.
	END IF
END IF

CALL random_number(u)
IF (u<rate(1)/2) powerup(2,1)=202		!1/82 Normal powerup chance (1/62 Easy & 1/103 Brutal

RETURN
END SUBROUTINE gauntlet

!-------Move Invader-------Move Invader-------Move Invader-------Move Invader-------Move Invader-------
SUBROUTINE move_invader(x00,x,elaser,row,col,animation,powerup,rate)
	IMPLICIT NONE
	REAL :: u, v, rate(8)
	INTEGER :: x00, i, j, enemy_index, v_int, v_num
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: x, y, elaser, animation, powerup	!x==invader; y==temporary loading matrix

y=0	!clear temporary matrix before populating

DO i=1,row-2,4		!move down (right edge)
	y(i+2,col)=x(i,col)
END DO

DO i=3,row-2,4		!move down (left edge)
	y(i+2,1)=x(i,1)
END DO

DO i=1,row-2,4		!move right
	DO j=1,col-1
		y(i,j+1)=x(i,j)
	END DO
END DO

DO i=3,row-2,4		!move left
	DO j=1,col-1
		y(i,j)=x(i,j+1)
	END DO
END DO

DO j=1,col
	y(row,j)=x(row,j)
END DO

x=0	!clear invader matrix before updating with new locations

DO i=1,row,2		!update new locations
	DO j=1,col
		x(i,j)=y(i,j)
	END DO
END DO

DO i=1,row,2		!enemy SPECIAL ABILITIES
	DO j=1,col
		IF ((x(i,j)/=0).AND.(x(i,j)/=11)) THEN		!If non-invader enemy
			enemy_abilities: SELECT CASE (x(i,j)-MOD(x(i,j),10))	!determine enemy type
				CASE(20)	!skirmisher	#21
					CALL random_number(u)
					IF (u<rate(2)) elaser(i-1,j)=1	!fire laser every 1/10 turns

				CASE(30)	!bomber		#32
					CALL random_number(u)
					IF (u<rate(3)) THEN	!fire triple laser every 1/18 turns
						IF (j/=1) elaser(i-1,j-1)=1
						elaser(i-1,j)=1
						IF (j/=col) elaser(i-1,j+1)=1
					END IF

				CASE(40)	!gunner		#42
					CALL random_number(u)
					IF (u<rate(4)) elaser(i-1,j)=1	!fire laser every 1/6 turns

				CASE(50)	!shielder	#52
					CALL random_number(u)
					IF (u<rate(5)) THEN	!re-spawns forcefield every 1/10 turns
						CALL random_number(v)	!shields spawn displaced by 1 because powerups move immediately after
						v_int=100*v
						shield_spawn: SELECT CASE(v_int)		!which shield respawns?
							CASE(0:20)	!FAR LEFT
								IF (MOD(i+3,4)==0) THEN !moving right
									IF (j>3) THEN
										powerup(i+1,j-3)=291
									ELSE IF (j==3) THEN	!i/=1 clause to prevent targetting location
										IF (i/=1) powerup(i-1,j-2)=291	!outside of matrix bounds
									ELSE IF (j==2) THEN
										IF (i/=1) powerup(i-1,j)=291
									ELSE
										IF (i/=1) powerup(i-2,j+2)=291
									END IF
								ELSE			!moving left
									IF (j>1) THEN
										powerup(i+1,j-1)=291
									ELSE
										powerup(i+3,j+1)=291
									END IF
								END IF
							CASE(21:40)	!DIRECTLY LEFT
								IF (MOD(i+3,4)==0) THEN !moving right
									IF (j==1) THEN
										IF (i/=1) powerup(i-1,j+1)=291
									ELSE IF (j==2) THEN
										IF (i/=1) powerup(i-1,j-1)=291
									ELSE
										powerup(i+1,j-2)=291
									END IF
								ELSE			!moving left
									powerup(i+1,j)=291
								END IF
							CASE(41:60)	!DIRECTLY BELOW
								IF (MOD(i+3,4)==0) THEN	!moving right
									IF (j/=1) THEN
										powerup(i+1,j-1)=291
									ELSE
										IF (i/=1) powerup(i-1,j)=291
									END IF
								ELSE			!moving left
									IF (j/=col) THEN
										powerup(i+1,j+1)=291
									ELSE
										powerup(i-1,j)=291
									END IF
								END IF
							CASE(61:80)	!DIRECTLY RIGHT
								IF (MOD(i+3,4)==0) THEN !moving right
									powerup(i+1,j)=291
								ELSE			!moving left
									IF (j<col-1) THEN
										powerup(i+1,j+2)=291
									ELSE IF (j==col-1) THEN
										powerup(i-1,j+1)=291
									ELSE
										powerup(i-1,j-1)=291
									END IF
								END IF
							CASE(81:100)	!FAR RIGHT
								IF (MOD(i+3,4)==0) THEN !moving right
									IF (j<col) THEN
										powerup(i+1,j+1)=291
									ELSE
										IF (i/=row-2) powerup(i+3,j)=291
									END IF
								ELSE			!moving left
									IF (j<col-2) THEN
										powerup(i+1,j+3)=291
									ELSE IF (j==col-2) THEN
										powerup(i-1,j+2)=291
									ELSE IF (j==col-1) THEN
										powerup(i-1,j)=291
									ELSE
										powerup(i-1,j-2)=291
									END IF
								END IF
							END SELECT shield_spawn
					END IF

				CASE(60)	!warper		#61
					CALL random_number(u)		!warps 1/11 turns
					IF (u<rate(6)) THEN	!3% warp down
						IF ((x(i+2,j)==0).AND.(i/=row-2)) THEN
							x(i+2,j)=x(i,j)		!warp down
							x(i,j)=0		!clear old space
							animation(i,j)=650	!flag warp animation
						END IF
					ELSE IF (u<2*rate(6)) THEN	!3% warp right
						IF ((x(i,j+1)==0).AND.(j/=col)) THEN
							x(i,j+1)=x(i,j)		!warp right
							x(i,j)=0		!clear old space
							animation(i,j)=650	!flag warp animation
						END IF
					ELSE IF (u<3*rate(6)) THEN	!3% warp left
						IF ((x(i,j-1)==0).AND.(j/=1)) THEN
							x(i,j-1)=x(i,j)		!warp left
							x(i,j)=0		!clear old space
							animation(i,j)=650	!flag warp animation
						END IF
					END IF

				CASE(70)	!carrier	#74
					CALL random_number(u)		!spawns 1/25 turns
					IF (u<rate(7))	THEN	!1% spawn down
						IF ((x(i+2,j)==0).AND.(i/=row-2)) x(i+2,j)=11		!spawn invader down
					ELSE IF (u<2*rate(7)) THEN	!1% spawn up
						IF (i/=1) THEN
							IF (x(i-2,j)==0) x(i-2,j)=11			!spawn invader up
						END IF
					ELSE IF (u<3*rate(7)) THEN	!1% spawn right
						IF (j/=col) THEN
							IF (x(i,j+1)==0) x(i,j+1)=11			!spawn invader right
						END IF
					ELSE IF (u<4*rate(7)) THEN	!1% spawn left
						IF (j/=1) THEN
							IF (x(i,j-1)==0) x(i,j-1)=11			!spawn invader left
						END IF
					END IF

				CASE(80)	!mothership	#89
					CALL random_number(u)
					IF (u<rate(4)) elaser(i-1,j)=1	!fire laser every 1/6 turns
					CALL random_number(u)
					IF (u<rate(3)) THEN		!fire triple laser every 1/18 turns
						IF (j/=1) elaser(i-1,j-1)=1
						elaser(i-1,j)=1
						IF (j/=col) elaser(i-1,j+1)=1
					END IF
					CALL random_number(u)
					!SPAWNING
					IF (u<4*rate(8)) THEN		!spawns 1/20 turns
						CALL random_number(v)
						v_int=100*v
						mother_spawn: SELECT CASE(v_int)	!determine unit to be spawned
							CASE(0:60)
								v_num=11	!invader
							CASE(61:80)
								v_num=21	!skirmisher
							CASE(81:85)
								v_num=32	!bomber
							CASE(86:90)
								v_num=42	!gunner
							CASE(91:95)
								v_num=61	!warper
							CASE(96:98)
								v_num=52	!shielder
							CASE(99:100)
								v_num=74	!carrier
							END SELECT mother_spawn
						IF (u<rate(8))	THEN		!1.25% spawn down
							IF ((x(i+2,j)==0).AND.(i/=row-2)) x(i+2,j)=v_num	!spawn invader down
						ELSE IF (u<2*rate(8)) THEN	!1.25% spawn up
							IF (i/=1) THEN
								IF (x(i-2,j)==0) x(i-2,j)=v_num			!spawn invader up
							END IF
						ELSE IF (u<3*rate(8)) THEN	!1.25% spawn right
							IF (j/=col) THEN
								IF (x(i,j+1)==0) x(i,j+1)=v_num			!spawn invader right
							END IF
						ELSE IF (u<4*rate(8)) THEN	!1.25% spawn left
							IF (j/=1) THEN
								IF (x(i,j-1)==0) x(i,j-1)=v_num			!spawn invader left
							END IF
						END IF
					END IF
				END SELECT enemy_abilities
		END IF
	END DO
END DO

x(1,1)=x00	!Spawn new invaders
x00=0

RETURN
END SUBROUTINE move_invader

!-------Move Powerup-------Move Powerup-------Move Powerup-------Move Powerup-------Move Powerup-------
SUBROUTINE move_powerup(x,row,col)		!NOTE: powerups have the opposite movement flow of invaders
	IMPLICIT NONE
	INTEGER :: i, j
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: x, y		!x==powerup; y==temporary loading matrix

IF (MAXVAL(x)/=0) THEN					!Only do if powerups are in play
	y=0	!clear temporary matrix before populating

	DO i=2,row-5,4		!move down (right edge)
		y(i+2,col)=x(i,col)
	END DO

	DO i=4,row-3,4		!move down (left edge)
		y(i+2,1)=x(i,1)
	END DO

	DO i=2,row-1,4		!move right
		DO j=1,col-1
			y(i,j+1)=x(i,j)
		END DO
	END DO

	DO i=4,row-3,4		!move left
		DO j=1,col-1
			y(i,j)=x(i,j+1)
		END DO
	END DO

	x=0	!clear powerup matrix before updating with new locations

	DO i=2,row-1,2
		DO j=1,col
		x(i,j)=y(i,j)
		END DO
	END DO
END IF

RETURN
END SUBROUTINE move_powerup

!-------Move Laser-------Move Laser-------Move Laser-------Move Laser-------Move Laser-------
SUBROUTINE move_laser(laser,invader,powerup,animation,row,col,eplaser,kills,hits,score,lives,multiplier,damageboost,&
												num_ordinance)
	IMPLICIT NONE
	INTEGER :: i, j, eplaser, kills, hits, score, lives, num_ordinance(5)
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: laser, invader, powerup, animation
	LOGICAL :: multiplier, damageboost

eplaser=1		!set player laser index (for powerup kills)

DO i=0,row-3,2		!scale even rows (laser)
	DO j=1,col	!scale columns
		IF (laser(i+2,j)/=0) THEN	!IF LASER
			IF (i==0) THEN	!top row
				IF (invader(i+1,j)==0) THEN					!NONE invader
					laser(i+2,j)=0			!clear old space
				ELSE								!INVADER
					CALL projectile_type_i(laser,invader,animation,powerup,&
				row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,num_ordinance)
				END IF
			ELSE		!other rows
				IF ((invader(i+1,j)==0).AND.(powerup(i,j)==0)) THEN		!NONE invader/powerup
					laser(i,j)=laser(i+2,j)		!move laser up
					laser(i+2,j)=0			!clear old space
				ELSE IF (invader(i+1,j)/=0) THEN				!INVADER
					CALL projectile_type_i(laser,invader,animation,powerup,&
				row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,num_ordinance)
				ELSE								!POWERUP
					CALL projectile_type_p(laser,powerup,animation,invader,&
				row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,num_ordinance)
				END IF
			END IF
		END IF

	END DO
END DO

RETURN
END SUBROUTINE move_laser

!-------Projectile Type_Invader-------Projectile Type_Invader-------Projectile Type_Invader-------
SUBROUTINE projectile_type_i(laser,invader,animation,powerup,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
												num_ordinance)
	IMPLICIT NONE
	INTEGER :: k, h, l, kills, hits, score, eplaser, lives, num_ordinance(5)
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: laser, invader, animation, powerup
	LOGICAL :: multiplier, damageboost

hits=hits+1	!update hits

IF (laser(i+2,j)==-1) THEN		!IF LASER	#-1
	IF (damageboost .EQV. .FALSE.) THEN	!NO damageboost
		invader(i+1,j)=invader(i+1,j)-1		!damage invader
	ELSE					!YES damageboost
		k=mod(invader(i+1,j),10)		!determine HP
		IF (k<3) THEN				!determine Damage
			invader(i+1,j)=invader(i+1,j)-k		!damage invader
		ELSE
			invader(i+1,j)=invader(i+1,j)-2		!damage invader
		END IF
	END IF
	laser(i+2,j)=0				!clear old laser space
	IF (mod(invader(i+1,j),10)==0) THEN	!if killed
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score,powerup)	!determine enemy type
		invader(i+1,j)=0		!clear dead invader space
	END IF

ELSE IF (laser(i+2,j)==-2) THEN		!IF PIERCING	#-2
	invader(i+1,j)=invader(i+1,j)-1		!damage invader
	laser(i+2,j)=0				!clear old laser space
	laser(i,j)=-1				!propagate standard laser
	IF (mod(invader(i+1,j),10)==0) THEN	!if killed
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score,powerup)	!determine enemy type
		invader(i+1,j)=0		!clear dead invader space
	END IF

ELSE IF (laser(i+2,j)==-3) THEN		!IF VAPORIZER	#-3
	k=mod(invader(i+1,j),10)		!determine HP
	IF (k<3) THEN				!determine Damage
		invader(i+1,j)=invader(i+1,j)-k		!damage invader
	ELSE
		invader(i+1,j)=invader(i+1,j)-3		!damage invader
	END IF
	laser(i+2,j)=0				!clear old laser space
	IF (mod(invader(i+1,j),10)==0) THEN	!if killed
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score,powerup)	!determine enemy type
		invader(i+1,j)=0		!clear dead invader space
	END IF

ELSE IF (laser(i+2,j)==-4) THEN		!IF MISSILE	#-4
	DO h=j-1,j+1	!Damage Invaders in AOE 3x3
		IF ((h/=0).AND.(h/=col+1)) THEN
			k=mod(invader(i+1,h),10)	!determine HP
			IF (k<2) THEN			!determine Damage
				invader(i+1,h)=invader(i+1,h)-k	!damage invader
			ELSE
				invader(i+1,h)=invader(i+1,h)-2	!damage invader
			END IF
			IF ((mod(invader(i+1,h),10)==0).AND.(invader(i+1,h)/=0)) THEN	!if killed
				CALL death_animation(invader,animation,row,col,i,h,kills,hits,score,powerup)	!determine enemy type
				invader(i+1,h)=0		!clear dead invader space
			END IF

			IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
				animation(i,h)=603
			ELSE
				animation(i,h)=601
			END IF
		END IF
	END DO

	DO l=i,i+2,2	!Damage Powerups in AOE 3x3
		DO h=j-1,j+1
			IF (i/=0) THEN
				IF ((h/=0).AND.(h/=col+1)) THEN
					IF (h==j) THEN
						animation(l,h)=602
					ELSE
						animation(l,h)=603
					END IF
					IF (powerup(l,h)/=0) THEN		!check for powerup
						k=mod(powerup(l,h),10)		!determine HP
						IF (k<2) THEN			!determine Damage
							powerup(l,h)=powerup(l,h)-k	!damage powerup
						ELSE
							powerup(l,h)=powerup(l,h)-2	!damage powerup
						END IF	
						IF ((mod(powerup(l,h),10)==0).AND.(powerup(l,h)/=0)) THEN	!if killed
							CALL powerup_death(powerup,animation,row,col,i,h,kills,hits,score&
						,eplaser,lives,multiplier,damageboost,num_ordinance)!determine powerup type
							powerup(l,h)=0		!clear dead powerup space
						END IF
					END IF
				END IF
			END IF
		END DO
	END DO
	laser(i+2,j)=0				!clear old missile space
END IF

RETURN
END SUBROUTINE projectile_type_i

!-------Projectile Type_Powerup-------Projectile Type_Powerup-------Projectile Type_Powerup-------
SUBROUTINE projectile_type_p(laser,powerup,animation,invader,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
													num_ordinance)
	IMPLICIT NONE
	INTEGER :: k, h, l, kills, hits, score, eplaser, lives, num_ordinance(5)
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: laser, powerup, animation, invader
	LOGICAL :: multiplier, damageboost

hits=hits+1	!update hits

IF (laser(i+2,j)==-1) THEN		!IF LASER	#-1
	IF (damageboost .EQV. .FALSE.) THEN	!NO damageboost
		powerup(i,j)=powerup(i,j)-1		!damage powerup
	ELSE					!YES damageboost
		k=mod(powerup(i,j),10)		!determine HP
		IF (k<3) THEN				!determine Damage
			powerup(i,j)=powerup(i,j)-k		!damage powerup
		ELSE
			powerup(i,j)=powerup(i,j)-3		!damage powerup
		END IF
	END IF
	laser(i+2,j)=0				!clear old laser space
	IF (mod(powerup(i,j),10)==0) THEN	!if destroyed
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
										num_ordinance)!determine powerup type
		powerup(i,j)=0		!clear dead powerup space
	END IF

ELSE IF (laser(i+2,j)==-2) THEN		!IF PIERCING	#-2
	powerup(i,j)=powerup(i,j)-1		!damage powerup
	laser(i+2,j)=0				!clear old laser space
	laser(i,j)=-1				!propagate standard laser
	IF (mod(powerup(i,j),10)==0) THEN	!if destroyed
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
										num_ordinance)!determine powerup type
		powerup(i,j)=0		!clear dead powerup space
	END IF
ELSE IF (laser(i+2,j)==-3) THEN		!IF VAPORIZER	#-3
	k=mod(powerup(i,j),10)		!determine HP
	IF (k<3) THEN				!determine Damage
		powerup(i,j)=powerup(i,j)-k		!damage powerup
	ELSE
		powerup(i,j)=powerup(i,j)-3		!damage powerup
	END IF
	laser(i+2,j)=0				!clear old laser space
	IF (mod(powerup(i,j),10)==0) THEN	!if destroyed
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
										num_ordinance)!determine powerup type
		powerup(i,j)=0		!clear dead powerup space
	END IF
ELSE IF (laser(i+2,j)==-4) THEN		!IF MISSILE	#-4
	DO h=j-1,j+1	!Damage powerups in AOE 3x3
		IF ((h/=0).AND.(h/=col+1)) THEN
			IF (h==j) THEN		!Animation Flag for Explosion
				animation(i,h)=601
			ELSE
				animation(i,h)=603
			END IF
			k=mod(powerup(i,h),10)	!determine HP
			IF (k<2) THEN			!determine Damage
				powerup(i,h)=powerup(i,h)-k	!damage powerup
			ELSE
				powerup(i,h)=powerup(i,h)-2	!damage powerup
			END IF
			IF ((mod(powerup(i,h),10)==0).AND.(powerup(i,h)/=0)) THEN	!if destroyed
				CALL powerup_death(powerup,animation,row,col,i,h,kills,hits,score,eplaser,lives,&
								multiplier,damageboost,num_ordinance) !determine powerup type
				powerup(i,h)=0		!clear dead powerup space
			END IF
		END IF
	END DO

	DO l=i-1,i+1,3	!Damage Invaders in AOE 3x3
		DO h=j-1,j+1
			IF ((h/=0).AND.(h/=col+1)) THEN
				IF (invader(l,h)/=0) THEN		!check for powerup
					k=mod(invader(l,h),10)		!determine HP
					IF (k<2) THEN			!determine Damage
						invader(l,h)=invader(l,h)-k	!damage invader
					ELSE
						invader(l,h)=invader(l,h)-2	!damage invader
					END IF	
					IF ((mod(invader(l,h),10)==0).AND.(invader(l,h)/=0)) THEN	!if destroyed
						CALL death_animation(invader,animation,row,col,l-1,h,kills,hits,score,powerup)
						invader(l,h)=0		!clear dead invader space
					END IF
				END IF
				IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
					animation(l,h)=602
				ELSE
					animation(l,h)=603
				END IF
			END IF
		END DO
	END DO
	laser(i+2,j)=0				!clear old missile space
END IF

RETURN
END SUBROUTINE projectile_type_p

!-------Move Enemy Laser-------Move Enemy Laser-------Move Enemy Laser-------Move Enemy Laser-------
SUBROUTINE move_enemy_laser(elaser,invader,powerup,animation,row,col,lives,eplaser,kills,hits,score,multiplier,damageboost,&
												num_ordinance)
	IMPLICIT NONE
	INTEGER :: i, j, l, k, lives, eplaser, kills, hits, score, num_ordinance(5)
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: elaser, invader, powerup,animation
	LOGICAL :: multiplier, damageboost

eplaser=0		!set enemy laser index (for powerup kills)

DO l=2,row-1,2		!scale even rows (elaser)
	DO j=1,col	!scale columns
		i=row+1-l	!reverse row order (bottom to top)
		IF (elaser(i,j)/=0) THEN	!IF ELASER
			IF (i==row-1) THEN	!bottom row				!PLAYER
				IF (invader(row,j)==-777) lives=lives-elaser(i,j)	!damage player
				elaser(i,j)=0				!clear old elaser space
			ELSE			!other rows
				IF (powerup(i+2,j)/=0) THEN				!POWERUP
					k=mod(powerup(i+2,j),10)		!determine HP
					IF (k<(-elaser(i,j))) THEN		!determine Damage
						powerup(i+2,j)=powerup(i+2,j)-k		!damage powerup
					ELSE
						powerup(i+2,j)=powerup(i+2,j)-elaser(i,j)	!damage powerup
					END IF
					elaser(i,j)=0					!clear old elaser space
					IF (mod(powerup(i+2,j),10)==0) THEN		!if destroyed
						CALL powerup_death(powerup,animation,row,col,i+2,j,kills,&
						hits,score,eplaser,lives,multiplier,damageboost,num_ordinance)
						powerup(i+2,j)=0			!clear dead powerup space
					END IF
				ELSE							!NO powerup
					elaser(i+2,j)=elaser(i,j)	!move elaser up
					elaser(i,j)=0			!clear elaser old space
				END IF
			END IF
		END IF
	END DO
END DO

RETURN
END SUBROUTINE move_enemy_laser

!-------Death Animation-------Death Animation-------Death Animation-------Death Animation-------
SUBROUTINE death_animation(invader,animation,row,col,i,j,kills,hits,score,powerup)
	IMPLICIT NONE
	INTEGER :: kills, hits, score, l
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: invader, animation, powerup

animation(i+1,j)=-invader(i+1,j)	!flag invader index
kills=kills+1				!update kills

enemy_index_score: SELECT CASE(invader(i+1,j))	!update score
	CASE(10)
		score=score+10	!kill invader		#11
	CASE(20)
		score=score+25	!kill skirmisher	#21
	CASE(30)
		score=score+75	!kill bomber		#32
	CASE(40)
		score=score+110	!kill gunner		#42
	CASE(50)
		score=score+160	!kill shielder		#52
		l=i+1
		!FAR LEFT					!DESTROY REMAINING FORCE FIELDS
		IF (MOD(l+3,4)==0) THEN !moving right
			IF (j>3) THEN
				IF (powerup(l+1,j-3)==291) THEN		!***In progress -> make sure powerups are killed, just force fields
					powerup(l+1,j-3)=0
					animation(l+1,j-3)=-290
				END IF
			ELSE IF (j==3) THEN	!i/=1 clause to prevent targetting location
				IF (powerup(l+1,j-3)==291) THEN		!outside of matrix bounds
					IF (l/=1) powerup(l-1,j-2)=0
					animation(l+1,j-3)=-290
				END IF
			ELSE IF (j==2) THEN
				IF (l/=1) powerup(l-1,j)=0
			ELSE
				IF (l/=1) powerup(l-2,j+2)=0
			END IF
		ELSE			!moving left
			IF (j>1) THEN
				powerup(l+1,j-1)=0
			ELSE
				powerup(l+3,j+1)=0
			END IF
		END IF
		!DIRECTLY LEFT
		IF (MOD(l+3,4)==0) THEN !moving right
			IF (j==1) THEN
				IF (l/=1) powerup(l-1,j+1)=0
			ELSE IF (j==2) THEN
				IF (l/=1) powerup(l-1,j-1)=0
			ELSE
				powerup(l+1,j-2)=0
			END IF
		ELSE			!moving left
			powerup(l+1,j)=0
		END IF
		!DIRECTLY BELOW
		IF (MOD(l+3,4)==0) THEN	!moving right
			IF (j/=1) THEN
				powerup(l+1,j-1)=0
			ELSE
				IF (l/=1) powerup(l-1,j)=0
			END IF
		ELSE			!moving left
			IF (j/=col) THEN
				powerup(l+1,j+1)=0
			ELSE
				powerup(l-1,j)=0
			END IF
		END IF
		!DIRECTLY RIGHT
		IF (MOD(l+3,4)==0) THEN !moving right
			powerup(l+1,j)=0
		ELSE			!moving left
			IF (j<col-1) THEN
				powerup(l+1,j+2)=0
			ELSE IF (j==col-1) THEN
				powerup(l-1,j+1)=0
			ELSE
				powerup(l-1,j-1)=0
			END IF
		END IF
		!FAR RIGHT
		IF (MOD(l+3,4)==0) THEN !moving right
			IF (j<col) THEN
				powerup(l+1,j+1)=0
			ELSE
				IF (l/=row-2) powerup(l+3,j)=0
			END IF
		ELSE			!moving left
			IF (j<col-2) THEN
				powerup(l+1,j+3)=0
			ELSE IF (j==col-2) THEN
				powerup(l-1,j+2)=0
			ELSE IF (j==col-1) THEN
				powerup(l-1,j)=0
			ELSE
				powerup(l-1,j-2)=0
			END IF
		END IF
	CASE(60)
		score=score+40	!kill warper		#61
	CASE(70)
		score=score+300	!kill carrier		#74
	CASE(80)
		score=score+650	!kill mothership	#87
	END SELECT enemy_index_score

RETURN
END SUBROUTINE death_animation

!-------Powerup Death Animation-------Powerup Death Animation-------Powerup Animation-------
SUBROUTINE powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser,lives,multiplier,damageboost,&
											num_ordinance)
	IMPLICIT NONE
	REAL :: u
	INTEGER :: kills, hits, score, eplaser, u_int, lives, num_ordinance(5)		!powerup 	#202 (i.e. 2 Health)
	INTEGER, intent(in) :: row, col, i, j						!force field	#291
	INTEGER, DIMENSION(row,col) :: powerup, animation
	LOGICAL :: multiplier, damageboost

IF (eplaser==1) THEN	!if player destroyed powerup
	IF (powerup(i,j)==200) THEN
		score=score+100			!update score
		CALL random_number(u)		!determine powerup type
		u_int=100*u
		powerup_type: SELECT CASE(u_int)
			CASE(0:25)
				animation(i,j)=-210		!flag destroyed +1 Life		#-210
				lives=lives+1
			CASE(26:50)
				animation(i,j)=-220		!flag destroyed 2x Score	#-220
				multiplier=.TRUE.
			CASE(51:75)
				animation(i,j)=-230		!flag destroyed + Ammo		#-230
				CALL random_number(u)
				IF (u<0.331) THEN
					num_ordinance(1)=num_ordinance(1)+3
				ELSE IF (u<0.661) THEN
					num_ordinance(2)=num_ordinance(2)+2
				ELSE
					num_ordinance(3)=num_ordinance(3)+1
				END IF
			CASE(76:100)
				animation(i,j)=-240		!flag destroyed 2x Damage	#-240
				damageboost=.TRUE.
	!		CASE DEFAULT
	!			animation(i,j)=-250		!flag destroyed Weapon Up	#-250
			END SELECT powerup_type
	ELSE
		score=score+5
		animation(i,j)=-290
	END IF
ELSE			!invader destroyed powerup
	IF (powerup(i,j)==200) THEN
		animation(i,j)=-200
	ELSE
		animation(i,j)=-290
	END IF
END IF

RETURN
END SUBROUTINE powerup_death
