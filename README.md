N-Body (Arbitrary) gravity simulation in Haskell.

* Toroidal world (wrap-around, i.e. Pacman)
* Very sandbox-y, but I need to work on more user-friendly customization.
* Possibly implement a way to detect when it is valid to use Barnes-Hutt to a reasonable accuracy

In a sense, ![image](https://github.com/user-attachments/assets/a31e2f9b-8e86-4ed3-ad80-dcd3a944f688) and ![image](https://github.com/user-attachments/assets/515619fe-89b6-48c6-a3a4-8913c8deae2c) must hold for whenever the approximation is used.

It might be possible to do this, but doing it efficiently enough to where the payoff of being able to do Barnes-Hutt sometimes outweighs the work to check when I can do it feels very hard.

Also, if anyone was genuinely curious(!!), sorry for the mess. I made these under the assumption that only I would see the code.
