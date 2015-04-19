!This whole file is dedicated to most of the graphics in Space Attack.  Each animation is called in the main
!game through a subroutine found here.  Notice that each animation has 3 "frames".  This allows simulation of
!animation/movement and gives a nicer flow of graphical elements.

!-------INVADER-------INVADER-------INVADER-------INVADER-------	!Overarching category (Invader)
SUBROUTINE invader_move(frame,rl)					!Individual subroutine (movement to the right)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_invader_r: SELECT CASE(frame)					!Case is cleaner than an IF BLOCK for this situation
		CASE(1)
			WRITE(*,9001, advance='no') '♅  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ♅ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ♅'	!frame=3
		END SELECT g_invader_r
	9001 FORMAT(A)	!Format label		!Note the "unnescessary" (A) format, which is implicitly implied, is actually wholly
						!necessary.  For some reason, if this was removed the spacing gets messed up.
ELSE			!rl==0 -> moving left
	g_invader_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ♅'
		CASE(2)
			WRITE(*,9001, advance='no') ' ♅ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '♅  '
		END SELECT g_invader_l
END IF
RETURN
END SUBROUTINE invader_move

!-------SKIRMISHER-------SKIRMISHER-------SKIRMISHER-------SKIRMISHER-------
SUBROUTINE skirmisher_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_skirmisher_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '♓  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ♓ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ♓'	!frame=3
		END SELECT g_skirmisher_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_skirmisher_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ♓'
		CASE(2)
			WRITE(*,9001, advance='no') ' ♓ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '♓  '
		END SELECT g_skirmisher_l
END IF
RETURN
END SUBROUTINE skirmisher_move

!-------BOMBER-------BOMBER-------BOMBER-------BOMBER-------
SUBROUTINE bomber_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_bomber_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '☫  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ☫ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ☫'	!frame=3
		END SELECT g_bomber_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_bomber_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ☫'
		CASE(2)
			WRITE(*,9001, advance='no') ' ☫ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '☫  '
		END SELECT g_bomber_l
END IF
RETURN
END SUBROUTINE bomber_move

!-------GUNNER-------GUNNER-------GUNNER-------GUNNER-------
SUBROUTINE gunner_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_gunner_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') 'ℵ  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ℵ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ℵ'	!frame=3
		END SELECT g_gunner_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_gunner_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ℵ'
		CASE(2)
			WRITE(*,9001, advance='no') ' ℵ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') 'ℵ  '
		END SELECT g_gunner_l
END IF
RETURN
END SUBROUTINE gunner_move

!-------SHIELDER-------SHIELDER-------SHIELDER-------SHIELDER-------
SUBROUTINE shielder_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_shielder_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '۞  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ۞ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ۞'	!frame=3
		END SELECT g_shielder_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_shielder_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ۞'
		CASE(2)
			WRITE(*,9001, advance='no') ' ۞ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '۞  '
		END SELECT g_shielder_l
END IF
RETURN
END SUBROUTINE shielder_move

!-------WARPER------WARPER-------WARPER-------WARPER-------
SUBROUTINE warper_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_warper_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '♑  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ♑ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ♑'	!frame=3
		END SELECT g_warper_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_warper_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ♑'
		CASE(2)
			WRITE(*,9001, advance='no') ' ♑ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '♑  '
		END SELECT g_warper_l
END IF
RETURN
END SUBROUTINE warper_move

!-------CARRIER------CARRIER-------CARRIER-------CARRIER-------
SUBROUTINE carrier_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_carrier_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '⋤  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ⋤ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ⋤'	!frame=3
		END SELECT g_carrier_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_carrier_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ⋥'
		CASE(2)
			WRITE(*,9001, advance='no') ' ⋥ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '⋥  '
		END SELECT g_carrier_l
END IF
RETURN
END SUBROUTINE carrier_move

!-------MOTHERSHIP------MOTHERSHIP-------MOTHERSHIP-------MOTHERSHIP-------
SUBROUTINE mothership_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_mothership_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') 'ↂ  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ↂ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ↂ'	!frame=3
		END SELECT g_mothership_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_mothership_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ↂ'
		CASE(2)
			WRITE(*,9001, advance='no') ' ↂ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') 'ↂ  '
		END SELECT g_mothership_l
END IF
RETURN
END SUBROUTINE mothership_move

!-------INVADER DEATH------INVADER DEATH-------INVADER DEATH-------INVADER DEATH-------
SUBROUTINE invader_killed(frame)
	INTEGER :: frame

	g_killed: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ⁙ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ⁛ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ∴ '	!frame=3
		END SELECT g_killed
	9001 FORMAT(A)

RETURN
END SUBROUTINE invader_killed

!-------EXPLOSION 1------EXPLOSION 1-------EXPLOSION 1-------EXPLOSION 1-------
SUBROUTINE explosion_1(frame)
	INTEGER :: frame

	g_explosion_1: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ⁜ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ⁛ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ⁘ '	!frame=3
		END SELECT g_explosion_1
	9001 FORMAT(A)

RETURN
END SUBROUTINE explosion_1

!-------EXPLOSION 2------EXPLOSION 2-------EXPLOSION 2-------EXPLOSION 2-------
SUBROUTINE explosion_2(frame)
	INTEGER :: frame

	g_explosion_2: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '   '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ⁛ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '   '	!frame=3
		END SELECT g_explosion_2
	9001 FORMAT(A)

RETURN
END SUBROUTINE explosion_2

!-------EXPLOSION 3------EXPLOSION 3-------EXPLOSION 3-------EXPLOSION 3-------
SUBROUTINE explosion_3(frame)
	INTEGER :: frame

	g_explosion_3: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '   '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ∴ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '   '	!frame=3
		END SELECT g_explosion_3
	9001 FORMAT(A)

RETURN
END SUBROUTINE explosion_3

!-------LASER MOVE------LASER MOVE-------LASER MOVE-------LASER MOVE-------
SUBROUTINE laser_move(frame)
	INTEGER :: frame

	g_laser_move: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ❈ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ❉ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ❊ '	!frame=3
		END SELECT g_laser_move
	9001 FORMAT(A)

RETURN
END SUBROUTINE laser_move

!-------ELASER MOVE------ELASER MOVE-------ELASER MOVE-------ELASER MOVE-------
SUBROUTINE elaser_move(frame)
	INTEGER :: frame

	g_elaser_move: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ✣ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ✥ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ✤ '	!frame=3
		END SELECT g_elaser_move
	9001 FORMAT(A)

RETURN
END SUBROUTINE elaser_move

!-------PIERCING MOVE------PIERCING MOVE-------PIERCING MOVE-------PIERCING MOVE-------
SUBROUTINE piercing_move(frame)
	INTEGER :: frame

	g_piercing_move: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ⁂ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ⁂ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ⁂ '	!frame=3
		END SELECT g_piercing_move
	9001 FORMAT(A)

RETURN
END SUBROUTINE piercing_move

!-------VAPORIZER MOVE------VAPORIZER MOVE-------VAPORIZER MOVE-------VAPORIZER MOVE-------
SUBROUTINE vaporizer_move(frame)
	INTEGER :: frame

	g_vaporizer_move: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ✸ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ✹ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ✺ '	!frame=3
		END SELECT g_vaporizer_move
	9001 FORMAT(A)

RETURN
END SUBROUTINE vaporizer_move

!-------MISSILE MOVE------MISSILE MOVE-------MISSILE MOVE-------MISSILE MOVE-------
SUBROUTINE missile_move(frame)
	INTEGER :: frame

	g_missile_move: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ❆ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ❂ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ☢ '	!frame=3
		END SELECT g_missile_move
	9001 FORMAT(A)

RETURN
END SUBROUTINE missile_move

!-------LIVE_UP_MOVE-------LIVE_UP_MOVE-------LIVE_UP_MOVE-------LIVE_UP_MOVE-------
SUBROUTINE live_up_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_live_up_move_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '♥  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ♥ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ♡'	!frame=3
		END SELECT g_live_up_move_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_live_up_move_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ♥'
		CASE(2)
			WRITE(*,9001, advance='no') ' ♥ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '♡  '
		END SELECT g_live_up_move_l
END IF
RETURN
END SUBROUTINE live_up_move

!-------SCORE UP MOVE-------SCORE UP MOVE-------SCORE UP MOVE-------SCORE UP MOVE-------
SUBROUTINE score_up_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_score_up_move_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '○  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ◎ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ●'	!frame=3
		END SELECT g_score_up_move_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_score_up_move_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ○'
		CASE(2)
			WRITE(*,9001, advance='no') ' ◎ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '●  '
		END SELECT g_score_up_move_l
END IF
RETURN
END SUBROUTINE score_up_move

!-------AMMO UP MOVE-------AMMO UP MOVE-------AMMO UP MOVE-------AMMO UP MOVE-------
SUBROUTINE ammo_up_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_ammo_up_move_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '✦  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ✧ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ✦'	!frame=3
		END SELECT g_ammo_up_move_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_ammo_up_move_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ✦'
		CASE(2)
			WRITE(*,9001, advance='no') ' ✧ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '✦  '
		END SELECT g_ammo_up_move_l
END IF
RETURN
END SUBROUTINE ammo_up_move

!-------CHARGE UP MOVE-------CHARGE UP MOVE-------CHARGE UP MOVE-------CHARGE UP MOVE-------
SUBROUTINE charge_up_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_charge_up_move_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') 'ℂ  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ₡ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ₢'	!frame=3
		END SELECT g_charge_up_move_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_charge_up_move_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ℂ'
		CASE(2)
			WRITE(*,9001, advance='no') ' ₡ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '₢  '
		END SELECT g_charge_up_move_l
END IF
RETURN
END SUBROUTINE charge_up_move

!-------WEAPON UP MOVE-------WEAPON UP MOVE-------WEAPON UP MOVE-------WEAPON UP MOVE-------
SUBROUTINE weapon_up_move(frame,rl)
	INTEGER :: frame, rl

IF (rl==1) THEN		!rl==1 -> moving right
	g_weapon_up_move_r: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '◆  '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ◈ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ◇'	!frame=3
		END SELECT g_weapon_up_move_r
	9001 FORMAT(A)

ELSE			!rl==0 -> moving left
	g_weapon_up_move_l: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ◆'
		CASE(2)
			WRITE(*,9001, advance='no') ' ◈ '
		CASE DEFAULT
			WRITE(*,9001, advance='no') '◇  '
		END SELECT g_weapon_up_move_l
END IF
RETURN
END SUBROUTINE weapon_up_move

!-------POWERUP DESTROYED------POWERUP DESTROYED-------POWERUP DESTROYED-------POWERUP DESTROYED-------
SUBROUTINE powerup_destroyed(frame)
	INTEGER :: frame

	g_powerup_destroyed: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '☾☽ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') '☾☆☽'	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '☾★☽'	!frame=3
		END SELECT g_powerup_destroyed
	9001 FORMAT(A)

RETURN
END SUBROUTINE powerup_destroyed

!-------COMBINATION------COMBINATION-------COMBINATION-------COMBINATION-------
SUBROUTINE combination(frame)
	INTEGER :: frame

	g_combination: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') ' ♅ '	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') ' ♅ '	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') ' ♅ '	!frame=3
		END SELECT g_combination
	9001 FORMAT(A)

RETURN
END SUBROUTINE combination

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

!-------OPEN SPACE-------OPEN SPACE-------OPEN SPACE-------
SUBROUTINE space()

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

!-------PRINT MENU-------PRINT MENU-------PRINT MENU-------PRINT MENU-------
SUBROUTINE print_menu(row,col,string,buffer,length,row_num,last)
IMPLICIT NONE
INTEGER :: buffer, length(10), row_num(10), last, i, j , k
INTEGER, intent(in) :: row, col
 CHARACTER (3*col), DIMENSION(10) :: string

k=1		!initialize index (for row, length, and string)
DO i=1,row+5
	IF (i==1) THEN		!Top border
		WRITE(*,9000, advance='no') '┌'
		DO j=1,3*col+2
			WRITE(*,9000, advance='no') '─'
		END DO
		WRITE(*,9000) '┐'

	ELSE IF (i==row_num(k)) THEN
		buffer=(3*col-length(k))/2
		WRITE(*,9000, advance='no') '│ '
		DO j=1,buffer			!left side buffer
			WRITE(*,9000, advance='no') ' '
		END DO
		WRITE(*,9000, advance='no') trim(string(k))
		IF (mod(length(k),2)>0) buffer=buffer-1
		DO j=3*col-buffer,3*col		!right side buffer
			WRITE(*,9000, advance='no') ' '
		END DO
		WRITE(*,9000) ' │'
		IF (k<last) k=k+1	!update indices

	ELSE IF (i<row+5) THEN	!Fills in empty rows
		WRITE(*,9000,advance='no') '│ '
		DO j=1,col
			call space()
		END DO
		WRITE(*,9000) ' │'

	ELSE			!Bottom border
		WRITE(*,9000, advance='no') '└'
		DO j=1,3*col+2
			WRITE(*,9000, advance='no') '─'
		END DO
		WRITE(*,9000) '┘'
	END IF
END DO

9000 FORMAT(A)	!Format label
END SUBROUTINE print_menu

!-------MAIN MENU-------MAIN MENU-------MAIN MENU-------MAIN MENU-------
SUBROUTINE menu_main(string,length,row_num,last,col)
	IMPLICIT NONE
	CHARACTER(3*col), DIMENSION(10) :: string
	INTEGER, intent(in) :: col
	INTEGER :: length(10), row_num(10), last

	string(1)='✭✭✭ Welcome to Space Attack! ✭✭✭'
	length(1)=32;	row_num(1)=2

	string(2)='♅  ♓  ♃  ♄  ☫  ♑  ♇  ☿  ♅'
	length(2)=25;	row_num(2)=4

	string(3)='Please adjust the height of your terminal to'
	length(3)=44;	row_num(3)=6

	string(4)='fit the border now.'
	length(4)=19;	row_num(4)=7

	string(5)='Press any key to continue.'
	length(5)=26;	row_num(5)=9

	last=5

RETURN
END SUBROUTINE menu_main

!-------LIFE LOSS------LIFE LOSS-------LIFE LOSS-------LIFE LOSS-------
SUBROUTINE life_loss(frame)				!	#+999
	INTEGER :: frame

	g_life_loss: SELECT CASE(frame)
		CASE(1)
			WRITE(*,9001, advance='no') '  ✖'	!frame=1
		CASE(2)	
			WRITE(*,9001, advance='no') '  ✘'	!frame=2
		CASE DEFAULT
			WRITE(*,9001, advance='no') '  ✗'	!frame=3
		END SELECT g_life_loss
	9001 FORMAT(A)

RETURN
END SUBROUTINE life_loss
