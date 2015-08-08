CC = gfortran
P1 = Engine/
P2 = Graphics/
P3 = Settings/
name = Space_Attack.x

Spacemake: $(P1)Space_Attack.f90 $(P1)primaries_sub.f90 $(P1)sys_keyin.c $(P1)menus.f90 $(P2)lose_animation.f90 $(P2)graphics_sub.f90
	@echo building object files
	$(CC) -fopenmp -c $(P1)Space_Attack.f90 -o $(P1)Space_Attack.o
	$(CC) -c $(P1)primaries_sub.f90 -o $(P1)primaries_sub.o
	$(CC) -c $(P1)sys_keyin.c -o $(P1)sys_keyin.o
	$(CC) -c $(P1)menus.f90 -o $(P1)menus.o
	$(CC) -c $(P2)lose_animation.f90 -o $(P2)lose_animation.o
	$(CC) -c $(P2)graphics_sub.f90 -o $(P2)graphics_sub.o
	@echo compiling executable
	$(CC) -fopenmp $(P1)Space_Attack.o $(P1)primaries_sub.o $(P1)sys_keyin.o $(P1)menus.o $(P2)lose_animation.o $(P2)graphics_sub.o -o $(name)
	$ mv settings.mod $(P3)settings.mod
	@echo ...
	@echo complete!
	@echo
	@echo --------------------------------------
	@echo Please type: export OMP_NUM_THREADS=2
	@echo --------------------------------------
	@echo
	@echo --------------------------------------
	@echo To play, type: ./$(name)
	@echo --------------------------------------
	@echo

clean:
	$(RM) $(P1)*.o
	$(RM) $(P2)*.o
	$(RM) $(P3)settings.mod

cleaner:
	$(RM) $(P1)*.o
	$(RM) $(P2)*.o
	$(RM) $(P3)settings.mod
	$(RM) $(name)
