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
