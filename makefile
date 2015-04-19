CC=gfortran

Spacemake: Space_Attack_v1_8.f90 graphics_sub.f90 primaries_sub.f90 lose_animation.f90 sys_keyin.c
	@echo building object files
	$(CC) -fopenmp -c sys_keyin.c graphics_sub.f90 primaries_sub.f90 lose_animation.f90 Space_Attack_v1_8.f90
	@echo compiling executable
	$(CC) -fopenmp sys_keyin.o graphics_sub.o primaries_sub.o lose_animation.o Space_Attack_v1_8.o -o Space_Attack
	@echo ...
	@echo complete!
	@echo
	@echo --------------------------------------
	@echo Please type: export OMP_NUM_THREADS=16
	@echo --------------------------------------
	@echo
	@echo --------------------------------------
	@echo To play, type: ./Space_Attack
	@echo --------------------------------------
	@echo
