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
!	TYPE			ANIMATION INDEX		FLAG TIMING
!	Invader			+10,+20,+30,...		
!	Killed Invader		-10,-20,-30,...		pre
!	Powerup			+200,+210,+220,...	
!	Destroyed Powerup	-200,-210,-220,...	pre
!	Player Laser		-1,-2,-3,...		
!	Enemy Laser		+1,+2,+3,...		
!	Explosion		601,602,603		pre
!	Combination Flag	666			pre
!	Player			-777			

!-------Move Invader-------Move Invader-------Move Invader-------Move Invader-------Move Invader-------
SUBROUTINE move_invader(x00,x,row,col)
	IMPLICIT NONE
	INTEGER :: x00, i, j
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: x, y		!x==invader; y==temporary loading matrix

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

DO i=1,row,2
	DO j=1,col
		x(i,j)=y(i,j)
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

y=0	!clear temporary matrix before populating

DO i=4,row-1,4		!move down (right edge)
	y(i+2,col)=x(i,col)
END DO

DO i=2,row-1,4		!move down (left edge)
	y(i+2,1)=x(i,1)
END DO

DO i=4,row-1,4		!move right
	DO j=2,col-1
		y(i,j+1)=x(i,j)
	END DO
END DO

DO i=2,row-1,4		!move left
	DO j=2,col-1
		y(i,j)=x(i,j+1)
	END DO
END DO

x=0	!clear powerup matrix before updating with new locations

DO i=1,row-2
	DO j=1,col
	x(i,j)=y(i,j)
	END DO
END DO

RETURN
END SUBROUTINE move_powerup

!-------Move Laser-------Move Laser-------Move Laser-------Move Laser-------Move Laser-------
SUBROUTINE move_laser(laser,invader,powerup,animation,row,col,eplaser,kills,hits,score)
	IMPLICIT NONE
	INTEGER :: i, j, eplaser, kills, hits, score
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: laser, invader, powerup, animation

eplaser=1		!set player laser index (for powerup kills)

DO i=0,row-3,2		!scale even rows (laser)
	DO j=1,col	!scale columns
		IF (laser(i+2,j)/=0) THEN	!IF LASER
			IF (i==0) THEN	!top row
				IF (invader(i+1,j)==0) THEN				!NO invader/powerup
					laser(i+2,j)=0			!clear old space
				ELSE							!INVADER
					CALL projectile_type_i(laser,invader,animation,powerup,row,col,i,j,kills,hits,score,eplaser)
				END IF
			ELSE		!other rows
				IF ((invader(i+1,j)==0).AND.(powerup(i,j)==0)) THEN		!NO invader/powerup
					laser(i,j)=laser(i+2,j)		!move laser up
					laser(i+2,j)=0			!clear old space
				ELSE IF (invader(i+1,j)/=0) THEN				!INVADER
					CALL projectile_type_i(laser,invader,animation,powerup,row,col,i,j,kills,hits,score,eplaser)
				ELSE								!POWERUP
					CALL projectile_type_p(laser,powerup,animation,invader,row,col,i,j,kills,hits,score,eplaser)
				END IF
			END IF
		END IF

	END DO
END DO

RETURN
END SUBROUTINE move_laser

!-------Projectile Type_Invader-------Projectile Type_Invader-------Projectile Type_Invader-------
SUBROUTINE projectile_type_i(laser,invader,animation,powerup,row,col,i,j,kills,hits,score,eplaser)
	IMPLICIT NONE
	INTEGER :: k, h, l, kills, hits, score, eplaser
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: laser, invader, animation, powerup

hits=hits+1	!update hits

IF (laser(i+2,j)==-1) THEN		!IF LASER	#-1
	invader(i+1,j)=invader(i+1,j)-1		!damage invader
	laser(i+2,j)=0				!clear old laser space
	IF (mod(invader(i+1,j),10)==0) THEN	!if killed
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score)	!determine enemy type
		invader(i+1,j)=0		!clear dead invader space
	END IF
ELSE IF (laser(i+2,j)==-2) THEN		!IF PIERCING	#-2
	invader(i+1,j)=invader(i+1,j)-1		!damage invader
	laser(i+2,j)=0				!clear old laser space
	laser(i,j)=-1				!propagate standard laser
	IF (mod(invader(i+1,j),10)==0) THEN	!if killed
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score)	!determine enemy type
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
		CALL death_animation(invader,animation,row,col,i,j,kills,hits,score)	!determine enemy type
		invader(i+1,j)=0		!clear dead invader space
	END IF
ELSE IF (laser(i+2,j)==-4) THEN		!IF MISSILE	#-4
	DO h=j-1,j+1	!Damage Invaders in AOE 3x3
		k=mod(invader(i+1,h),10)	!determine HP
		IF (k<2) THEN			!determine Damage
			invader(i+1,h)=invader(i+1,h)-k	!damage invader
		ELSE
			invader(i+1,h)=invader(i+1,h)-2	!damage invader
		END IF
		IF (mod(invader(i+1,h),10)==0) THEN	!if killed
			CALL death_animation(invader,animation,row,col,i,h,kills,hits,score)	!determine enemy type
			invader(i+1,h)=0		!clear dead invader space
		END IF

		IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
			animation(h,j)=603
		ELSE
			animation(h,j)=601
		END IF
	END DO

	DO l=i,i+2,2	!Damage Powerups in AOE 3x3
		DO h=j-1,j+1
			IF (powerup(l,h)/=0) THEN		!check for powerup
				k=mod(powerup(l,h),10)		!determine HP
				IF (k<2) THEN			!determine Damage
					powerup(l,h)=powerup(l,h)-k	!damage powerup
				ELSE
					powerup(l,h)=powerup(l,h)-2	!damage powerup
				END IF	
				IF (mod(powerup(l,h),10)==0) THEN	!if killed
					CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)!determine powerup type
					powerup(l,h)=0		!clear dead powerup space
				END IF
			END IF
			IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
				animation(h,j)=602
			ELSE
				animation(h,j)=603
			END IF
		END DO
	END DO
END IF

RETURN
END SUBROUTINE projectile_type_i

!-------Projectile Type_Powerup-------Projectile Type_Powerup-------Projectile Type_Powerup-------
SUBROUTINE projectile_type_p(laser,powerup,animation,invader,row,col,i,j,kills,hits,score,eplaser)
	IMPLICIT NONE
	INTEGER :: k, h, l, kills, hits, score, eplaser
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: laser, powerup, animation, invader

hits=hits+1	!update hits

IF (laser(i+2,j)==-1) THEN		!IF LASER	#-1
	powerup(i,j)=powerup(i,j)-1		!damage powerup
	laser(i+2,j)=0				!clear old laser space
	IF (mod(powerup(i,j),10)==0) THEN	!if destroyed
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)	!determine powerup type
		powerup(i,j)=0		!clear dead powerup space
	END IF
ELSE IF (laser(i+2,j)==-2) THEN		!IF PIERCING	#-2
	powerup(i,j)=powerup(i,j)-1		!damage powerup
	laser(i+2,j)=0				!clear old laser space
	laser(i,j)=-1				!propagate standard laser
	IF (mod(powerup(i,j),10)==0) THEN	!if destroyed
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)	!determine powerup type
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
		CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)	!determine powerup type
		powerup(i,j)=0		!clear dead powerup space
	END IF
ELSE IF (laser(i+2,j)==-4) THEN		!IF MISSILE	#-4
	DO h=j-1,j+1	!Damage powerups in AOE 3x3
		k=mod(powerup(i,h),10)	!determine HP
		IF (k<2) THEN			!determine Damage
			powerup(i,h)=powerup(i,h)-k	!damage powerup
		ELSE
			powerup(i,h)=powerup(i,h)-2	!damage powerup
		END IF
		IF (mod(powerup(i,h),10)==0) THEN	!if destroyed
			CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)	!determine powerup type
			powerup(i,h)=0		!clear dead powerup space
		END IF

		IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
			animation(h,j)=603
		ELSE
			animation(h,j)=601
		END IF
	END DO

	DO l=i-1,i+1,3	!Damage Invaders in AOE 3x3
		DO h=j-1,j+1
			IF (invader(l,h)/=0) THEN		!check for powerup
				k=mod(invader(l,h),10)		!determine HP
				IF (k<2) THEN			!determine Damage
					invader(l,h)=invader(l,h)-k	!damage invader
				ELSE
					invader(l,h)=invader(l,h)-2	!damage invader
				END IF	
				IF (mod(invader(l,h),10)==0) THEN	!if destroyed
					CALL death_animation(invader,animation,row,col,l-1,h,kills,hits,score)	!determine invader type
					invader(l,h)=0		!clear dead invader space
				END IF
			END IF
			IF ((h==j-1).OR.(h==j+1)) THEN		!Animation Flag for Explosion
				animation(h,j)=602
			ELSE
				animation(h,j)=603
			END IF
		END DO
	END DO
END IF

RETURN
END SUBROUTINE projectile_type_p

!-------Move Enemy Laser-------Move Enemy Laser-------Move Enemy Laser-------Move Enemy Laser-------
SUBROUTINE move_enemy_laser(elaser,invader,powerup,animation,row,col,lives,eplaser,kills,hits,score)
	IMPLICIT NONE
	INTEGER :: i, j, l, k, lives, eplaser, kills, hits, score
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION (row,col) :: elaser, invader, powerup,animation

eplaser=0		!set enemy laser index (for powerup kills)

DO l=2,row-1,2		!scale even rows (elaser)
	DO j=1,col	!scale columns
		i=row+1-l	!reverse row order (bottom to top)
		IF (elaser(i,j)/=0) THEN	!IF ELASER
			IF (i==row-1) THEN	!bottom row				!PLAYER
				IF (invader(row,j)==4) lives=lives-elaser(i,j)	!damage player
				elaser(i,j)=0				!clear old elaser space
			ELSE			!other rows
				IF (powerup(i+1,j)/=0) THEN				!POWERUP
					k=mod(powerup(i+1,j),10)		!determine HP
					IF (k<(-elaser(i,j))) THEN		!determine Damage
						powerup(i,j)=powerup(i,j)-k		!damage powerup
					ELSE
						powerup(i,j)=powerup(i,j)-elaser(i,j)	!damage powerup
					END IF
					elaser(i,j)=0					!clear old elaser space
					IF (mod(powerup(i+1,j),10)==0) THEN		!if destroyed
						CALL powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)!determine p_up type
						powerup(i+1,j)=0			!clear dead powerup space
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
SUBROUTINE death_animation(invader,animation,row,col,i,j,kills,hits,score)
	IMPLICIT NONE
	INTEGER :: enemy_index, kills, hits, score
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: invader, animation

enemy_index=invader(i+1,j)		!determine enemy type
animation(i+1,j)=-enemy_index		!print invader index
kills=kills+1				!update kills

enemy_index_score: SELECT CASE(enemy_index)	!update score
	CASE(10)
		score=score+10	!kill invader		#11
	CASE(20)
		score=score+25	!kill skirmisher	#21
	CASE(30)
		score=score+60	!kill bomber		#32
	CASE(40)
		score=score+100	!kill gunner		#42
	CASE(50)
		score=score+150	!kill shielder		#55
	CASE(60)
		score=score+75	!kill warper		#61
	CASE(70)
		score=score+250	!kill carrier		#74
	CASE(80)
		score=score+500	!kill mothership	#89
	END SELECT enemy_index_score

RETURN
END SUBROUTINE death_animation

!-------Powerup Death Animation-------Powerup Death Animation-------Powerup Animation-------
SUBROUTINE powerup_death(powerup,animation,row,col,i,j,kills,hits,score,eplaser)
	IMPLICIT NONE
	INTEGER :: kills, hits, score, eplaser, powerup_index
	INTEGER, intent(in) :: row, col, i, j
	INTEGER, DIMENSION(row,col) :: powerup, animation

powerup_index=powerup(i+1,j)		!determine powerup type
animation(i+1,j)=-powerup_index		!print powerup index
IF (eplaser==1) THEN	!if player destroyed powerup
	score=score+10			!update score

powerup_index_score: SELECT CASE(powerup_index)	!update score
	CASE(200)
		!CALL life_up()		!print destroyed +1 Life	#201
	CASE(210)
		!CALL score_up()	!print destroyed 2x Score	#211
	CASE(220)
		!CALL ammo_up()		!print destroyed + Ammo		#221
	CASE(230)
		!CALL charge_up()	!print destroyed 2x Charge	#231
	CASE(240)
		!CALL weapon_up()	!print destroyed Weapon Up	#241
	END SELECT powerup_index_score
END IF

RETURN
END SUBROUTINE powerup_death
