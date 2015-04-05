!This whole file is dedicated to most of the graphics in Space Attack.  Each animation is called in the main
!game through a subroutine found here.  Notice that each animation has 3 "frames".  This allows simulation of
!animation/movement and gives a nicer flow of graphical elements.

!-------INVADER-------INVADER-------INVADER-------INVADER-------	!Overarching category (Invader)
SUBROUTINE invader_r(frame)	!Right				!Individual subroutine (movement to the right)
	INTEGER :: frame

g_invader_r: SELECT CASE(frame)					!Case is cleaner than an IF BLOCK for this situation
	CASE(1)						!frame=1
		WRITE(*,9001, advance='no') '♅  '
	CASE(2)						!frame=2
		WRITE(*,9001, advance='no') ' ♅ '
	CASE DEFAULT					!frame=3
		WRITE(*,9001, advance='no') '  ♅'
	END SELECT g_invader_r
9001 FORMAT(A)	!Format label		!Note the "unnescessary" (A) format, which is implicitly implied, is actually wholly
RETURN					!necessary.  For some reason, if this was removed the spacing gets messed up.
END SUBROUTINE invader_r

SUBROUTINE invader_l(frame)	!Left
	INTEGER :: frame

g_invader_l: SELECT CASE(frame)
	CASE(1)
		WRITE(*,9001, advance='no') '  ♅'
	CASE(2)
		WRITE(*,9001, advance='no') ' ♅ '
	CASE DEFAULT
		WRITE(*,9001, advance='no') '♅  '
	END SELECT g_invader_l
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE invader_l

!-------PLAYER-------PLAYER-------PLAYER-------PLAYER-------
SUBROUTINE player(frame)	!No Move
	INTEGER :: frame

WRITE(*,9001, advance='no') ' ☋ '
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE player

SUBROUTINE player_r(frame)	!Right
	INTEGER :: frame

g_player_r: SELECT CASE(frame)
	CASE(1)
		WRITE(*,9001, advance='no') ' ☋  '
	CASE(2)
		WRITE(*,9001, advance='no') '  ☋ '
	CASE DEFAULT
		WRITE(*,9001, advance='no') '   ☋'
	END SELECT g_player_r
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE player_r

SUBROUTINE player_l(frame)	!Left
	INTEGER :: frame

g_player_l: SELECT CASE(frame)
	CASE(1)
		WRITE(*,9001, advance='no') '  ☋ '
	CASE(2)
		WRITE(*,9001, advance='no') ' ☋  '
	CASE DEFAULT
		WRITE(*,9001, advance='no') '☋   '
	END SELECT g_player_l
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE player_l

!-------LASER-------LASER-------LASER-------LASER-------LASER-------
SUBROUTINE laser_move(frame)
	INTEGER :: frame
glaser_move: SELECT CASE(frame)
	CASE(1)
		WRITE(*,9001, advance='no') ' ❈ '
	CASE(2)
		WRITE(*,9001, advance='no') ' ❉ '
	CASE DEFAULT
		WRITE(*,9001, advance='no') ' ❊ '
	END SELECT glaser_move
				!Other possible projectiles ⇡↑↾↿❈ ❉ ❊☱☲☴○ ◎ ●
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE laser_move

!-------OPEN SPACE-------OPEN SPACE-------OPEN SPACE-------
SUBROUTINE space(frame)
	INTEGER :: frame

WRITE(*,9001, advance='no') '   '
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE space

!-------WEAPON CHARGE UP-------WEAPON CHARGE UP-------WEAPON CHARGE UP-------
SUBROUTINE wcharge_up(frame,charge)	!Charging Up
	INTEGER :: frame, charge
IF (charge==1) THEN		!Charge 0->1
	wcharge_up1: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▫▫▫▫▫▫▫▫'	!▮ ▯
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▫▫▫▫▫▫▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▪▪▫▫▫▫▫▫'
		END SELECT wcharge_up1
ELSE IF (charge==2) THEN	!Charge 1->2
	wcharge_up2: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▪▪▪▫▫▫▫▫'
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▫▫▫▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▫▫▫'
		END SELECT wcharge_up2
ELSE !IF(charge==3) THEN	!Charge 2->3
	wcharge_up3: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▫▫'
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▪▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▪▪'
		END SELECT wcharge_up3
END IF
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE wcharge_up

!-------WEAPON CHARGE DOWN-------WEAPON CHARGE DOWN-------WEAPON CHARGE DOWN-------
SUBROUTINE wcharge_down(frame,charge)	!Charging Down
	INTEGER :: frame, charge
IF (charge==0) THEN		!Charge 1->0
	wcharge_down1: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▪▪▫▫▫▫▫▫'
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▫▫▫▫▫▫▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▫▫▫▫▫▫▫▫'
		END SELECT wcharge_down1
ELSE IF (charge==1) THEN	!Charge 2->1
	wcharge_down2: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▫▫▫'
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▫▫▫▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▪▪▪▫▫▫▫▫'
		END SELECT wcharge_down2
ELSE !IF (charge==2) THEN	!Charge 3->2
	wcharge_down3: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▪▪'
		CASE(2)
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▪▫'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▫▫'
		END SELECT wcharge_down3
END IF
9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE wcharge_down

!-------WEAPON CHARGE IDLE-------WEAPON CHARGE IDLE-------WEAPON CHARGE IDLE-------
SUBROUTINE wcharge_idle(frame,charge)	!Charge Idle
	INTEGER :: frame, charge

IF (charge==0) THEN		!Charge idle == 0
	WRITE(*,9001, advance='no') '▫▫▫▫▫▫▫▫▫'
ELSE IF (charge==1) THEN	!Charge idle == 1
	WRITE(*,9001, advance='no') '▪▪▪▫▫▫▫▫▫'
ELSE IF (charge==2) THEN	!Charge idle == 2
	WRITE(*,9001, advance='no') '▪▪▪▪▪▪▫▫▫'
ELSE	!charge==3		!Charge idle == 3
	WRITE(*,9001, advance='no') '▪▪▪▪▪▪▪▪▪'
END IF

9001 FORMAT(A)	!Format label
RETURN
END SUBROUTINE wcharge_idle
