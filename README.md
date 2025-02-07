N-Body (Arbitrary) gravity simulation in Haskell.

The simulation happens on a toroidal world (wrap-around, i.e. Pacman). It's meant to be a sandbox, but I need to work on more user-friendly customization. For more complex gravity functions my mac was able to handle about 40-50 objects smoothly. However, it seems that drawing objects is very expensive, as the data analysis mode (which skips the rendering and just focuses on comparing the total energy of the system) is much, much faster.


* TODO: Possibly implement a way to detect when it is valid to use Barnes-Hutt to a reasonable accuracy

In a sense, ![image](https://github.com/user-attachments/assets/a31e2f9b-8e86-4ed3-ad80-dcd3a944f688) and ![image](https://github.com/user-attachments/assets/515619fe-89b6-48c6-a3a4-8913c8deae2c) must hold for whenever the Barnes-Hutt approximation is used.

It might be possible to do this, but doing it efficiently enough to where the payoff of being able to do Barnes-Hutt sometimes outweighs the work to check when I can do it feels very hard.

Also, if anyone was genuinely curious(!!), sorry for the mess. I made these under the assumption that only I would see the code. However, I do intend to clean this up.
