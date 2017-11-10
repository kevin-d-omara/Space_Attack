Space Attack
======================
A high-octane bullet-hell shooter, inspired by Space Invaders.
-----------------------------------------------------------------------------------

## Description

You are an ace gunner operating a mobile defense laser.
Swarms of voracious alien space craft are bearing down on Earth.
Numerous Invaders, fearsome Bombers, armored Carriers, and more dizzyingly devious death-machines.
Engage the maurading alien space craft, but watch out for incoming plasma!
Use your powerful, limited ordinance to high effect.
Survive 9 waves of onslaught and destroy the mothership to save Earth!

After halting the invasion, gear up for the endless gauntlet.
Pick your loadout, survive as long as you can, and set the highest score.
Then, grin as your friends fail to usurp your glory!

## Controls (default):
Command | Key
--- | :---:
Fire Laser | W
Move Left/Right | A/D
Pause | P

Special Ordinance | Key | Damage | Effect
--- | :---: | :---: |---
Fire Scattershot | 1 | 1 | Pierces through the first target.
Fire Vaporizer | 2 | 3 | High powered attack.
Fire Missile | 3 | 2 | 3x3 Area of Effect.

*Controls can be changed in-game.*
*For more gameplay information, see the in-game Guide.*

## Gameplay Video
*--The game has no sound.--*
[![Image](<http://i.imgur.com/8yRfVcb.png>)](https://www.youtube.com/watch?v=TSOmxRPeW0w)

## Screenshots
![Image](<http://i.imgur.com/qNdxfxM.png>)
![Image](<http://i.imgur.com/a8E6ww0.png>)
![Image](<http://i.imgur.com/x0Sbp4n.png>)
*Screen shots were taken playing in the [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term).*

## Build Instructions (Linux)
### Dependencies
Make sure to install `gfortran` first.
```
sudo apt-get install gfortran
```

### Compile
```sh
# Get it from GitHub.
git clone https://github.com/kevin-d-omara/Space_Attack.git

# Compile it.
cd Space_Attack
make

# Have fun!
export OMP_NUM_THREADS=2
./Space_Attack.x
```
