CC=gfortran

Spacemake: Space_Attack.f90 graphics_sub.f90 primaries_sub.f90 lose_animation.f90 sys_keyin.c menus.f90
	@echo building object files
	$(CC) -fopenmp -c sys_keyin.c graphics_sub.f90 primaries_sub.f90 lose_animation.f90 menus.f90 Space_Attack.f90
	@echo compiling executable
	$(CC) -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o lose_animation.o menus.o Space_Attack.o -o Space_Attack
	@echo ...
	@echo complete!
	@echo
	@echo --------------------------------------
	@echo Please type: export OMP_NUM_THREADS=2
	@echo --------------------------------------
	@echo
	@echo --------------------------------------
	@echo To play, type: ./Space_Attack
	@echo --------------------------------------
	@echo
