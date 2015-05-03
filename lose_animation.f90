!-------NEXT WAVE ANIMATION-------NEXT WAVE ANIMATION-------NEXT WAVE ANIMATION-------NEXT WAVE ANIMATION
SUBROUTINE nextwave_2000(dframe,anim_index)
	INTEGER :: dframe, anim_index

g_nextwave_2000: SELECT CASE(anim_index)
	CASE(2000)
		WRITE(*,9001, advance='no') 'NEX' 	!Next
	CASE(2001)
		WRITE(*,9001, advance='no') 'T W' 
	CASE(2002)
		WRITE(*,9001, advance='no') 'AVE'
	CASE(2003)
		WRITE(*,9001, advance='no') 'LAS'	!Last 
	CASE(2004)
		WRITE(*,9001, advance='no') 'VIC'	!Victory
	CASE(2005)
		WRITE(*,9001, advance='no') 'TOR'
	CASE(2006)
		WRITE(*,9001, advance='no') 'Y‼‼' 
	END SELECT g_nextwave_2000
9001 FORMAT(A)

RETURN 
END SUBROUTINE nextwave_2000

SUBROUTINE nextwave_2010(dframe,anim_index)
	INTEGER :: dframe, anim_index

IF (MOD(dframe,2)/=0) THEN
	g_nextwave_2010: SELECT CASE(anim_index)
		CASE(2010)
			WRITE(*,9001, advance='no') 'INC' 
		CASE(2011)
			WRITE(*,9001, advance='no') 'OMI' 
		CASE(2012)
			WRITE(*,9001, advance='no') 'NG‼'
		CASE(2013)
			WRITE(*,9001, advance='no') '★ ★' 	!✰
		CASE(2014)
			WRITE(*,9001, advance='no') ' ★ '
		END SELECT g_nextwave_2010
ELSE
	WRITE(*,9001, advance='no') '   '
END IF
9001 FORMAT(A)

RETURN 
END SUBROUTINE nextwave_2010


!-------ENDING 1000------ENDING 1000-------ENDING 1000-------ENDING 1000-------
SUBROUTINE ending_1000(dframe)				!	#+1000
	INTEGER :: dframe

	g_ending_1000: SELECT CASE(dframe)	!Length 16
		CASE(1)
			WRITE(*,9001, advance='no') ' ‼◌' 
		CASE(2)
			WRITE(*,9001, advance='no') ' ‼○'  
		CASE(3)	
			WRITE(*,9001, advance='no') ' ‼◌'
		CASE(4)	
			WRITE(*,9001, advance='no') ' ‼○'
		CASE(5)	
			WRITE(*,9001, advance='no') ' ‼◎'
		CASE(6)	
			WRITE(*,9001, advance='no') ' ‼○'
		CASE(7)	
			WRITE(*,9001, advance='no') '  ◎'
		CASE(8)	
			WRITE(*,9001, advance='no') '  ◉'
		CASE(9)	
			WRITE(*,9001, advance='no') '  ◎'
		CASE(10)
			WRITE(*,9001, advance='no') '  ◉'
		CASE(11)
			WRITE(*,9001, advance='no') '  ◍'
		CASE(12)
			WRITE(*,9001, advance='no') '  ◉'
		CASE(13)
			WRITE(*,9001, advance='no') '  ◍'
		CASE(14)
			WRITE(*,9001, advance='no') '  ●'
		CASE(15)
			WRITE(*,9001, advance='no') '  ◍'
		CASE(16)
			WRITE(*,9001, advance='no') '  ●'
		CASE DEFAULT
			WRITE(*,9001, advance='no') '   '
		END SELECT g_ending_1000
	9001 FORMAT(A)

RETURN
END SUBROUTINE ending_1000

!-------ENDING A------ENDING A-------ENDING A-------ENDING A-------
SUBROUTINE ending_A(dframe,animation,i,j,row,col)	!	#+1001
	INTEGER :: dframe, i, j
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION(row,col) :: animation

IF (MOD(dframe-4,12)==0) THEN
	WRITE(*,9001, advance='no') ' ∗ '
ELSE IF (MOD(dframe-4,12)==1) THEN
	WRITE(*,9001, advance='no') ' ※ '
ELSE IF (MOD(dframe-4,12)==2) THEN
	WRITE(*,9001, advance='no') ' ⁙ '
ELSE IF (MOD(dframe-4,12)==3) THEN
	WRITE(*,9001, advance='no') '⁖ ჻'
ELSE IF (MOD(dframe-4,12)==4) THEN
	WRITE(*,9001, advance='no') '჻ ⁖'
ELSE IF (MOD(dframe-4,12)==5) THEN
	WRITE(*,9001, advance='no') ' ∷ '
ELSE IF (MOD(dframe-4,12)==6) THEN
	WRITE(*,9001, advance='no') ' ⁛ '
ELSE IF (MOD(dframe-4,12)==7) THEN
	WRITE(*,9001, advance='no') ' ⁙ '
ELSE IF (MOD(dframe-4,12)==8) THEN
	WRITE(*,9001, advance='no') ' ⁛ '
ELSE IF (MOD(dframe-4,12)==9) THEN
	WRITE(*,9001, advance='no') '∴ ∵'
ELSE IF (MOD(dframe-4,12)==10) THEN
	WRITE(*,9001, advance='no') '∵ ∴'
ELSE IF (MOD(dframe-4,12)==11) THEN
	WRITE(*,9001, advance='no') ' ⁖ '
	!animation(i,j)=0
ELSE
	WRITE(*,9001, advance='no') '   '
END IF

9001 FORMAT(A)
RETURN
END SUBROUTINE ending_A

!-------ENDING B------ENDING B-------ENDING B-------ENDING B-------
SUBROUTINE ending_B(dframe,animation,i,j,row,col)	!	#+1002
	INTEGER :: dframe, i, j
	INTEGER, intent(in) :: row, col
	INTEGER, DIMENSION(row,col) :: animation

IF (MOD(dframe-10,12)==0) THEN
	WRITE(*,9001, advance='no') ' ∗ '
ELSE IF (MOD(dframe-10,12)==1) THEN
	WRITE(*,9001, advance='no') ' ※ '
ELSE IF (MOD(dframe-10,12)==2) THEN
	WRITE(*,9001, advance='no') ' ⁙ '
ELSE IF (MOD(dframe-10,12)==3) THEN
	WRITE(*,9001, advance='no') '⁖ ჻'
ELSE IF (MOD(dframe-10,12)==10) THEN
	WRITE(*,9001, advance='no') '჻ ⁖'
ELSE IF (MOD(dframe-10,12)==5) THEN
	WRITE(*,9001, advance='no') ' ∷ '
ELSE IF (MOD(dframe-10,12)==6) THEN
	WRITE(*,9001, advance='no') ' ⁛ '
ELSE IF (MOD(dframe-10,12)==7) THEN
	WRITE(*,9001, advance='no') ' ⁙ '
ELSE IF (MOD(dframe-10,12)==8) THEN
	WRITE(*,9001, advance='no') ' ⁛ '
ELSE IF (MOD(dframe-10,12)==9) THEN
	WRITE(*,9001, advance='no') '∴ ∵'
ELSE IF (MOD(dframe-10,12)==10) THEN
	WRITE(*,9001, advance='no') '∵ ∴'
ELSE IF (MOD(dframe-10,12)==11) THEN
	WRITE(*,9001, advance='no') ' ⁖ '
	!animation(i,j)=0
ELSE
	WRITE(*,9001, advance='no') '   '
END IF

9001 FORMAT(A)
RETURN
END SUBROUTINE ending_B
