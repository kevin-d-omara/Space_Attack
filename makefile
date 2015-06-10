CC = gfortran

Spacemake: Engine/Space_Attack.f90 Engine/primaries_sub.f90 Engine/sys_keyin.c Engine/menus.f90 Graphics/lose_animation.f90 Graphics/graphics_sub.f90
	@echo building object files
	$(CC) -fopenmp -c Engine/Space_Attack.f90 -o Engine/Space_Attack.o
	$(CC) -c Engine/primaries_sub.f90 -o Engine/primaries_sub.o
	$(CC) -c Engine/sys_keyin.c -o Engine/sys_keyin.o
	$(CC) -c Engine/menus.f90 -o Engine/menus.o
	$(CC) -c Graphics/lose_animation.f90 -o Graphics/lose_animation.o
	$(CC) -c Graphics/graphics_sub.f90 -o Graphics/graphics_sub.o
	@echo compiling executable
	$(CC) -fopenmp Engine/Space_Attack.o Engine/primaries_sub.o Engine/sys_keyin.o Engine/menus.o Graphics/lose_animation.o Graphics/graphics_sub.o -o Space_Attack
	$ mv settings.mod Settings/settings.mod
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

clean:
	$(RM) Engine/*.o
	$(RM) Graphics/*.o
	$(RM) Settings/settings.mod
