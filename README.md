# CHIP-8 Emulator... but in Haskell !

[CHIP-8 on wikipedia](https://en.wikipedia.org/wiki/CHIP-8)  

Run (and build if not already): `cabal run Chip8`  
Run with arguments (and build if not already) : `cabal run Chip8 -- path.ch8`

Note about SFML:

Hackage's SFML has a build error, so I was forced to use its repository to make it build.  
The submodule `Haskell-SFML` contains SFML source code, and Cabal can build it thanks to [cabal.project](cabal.project) which adds SFML directory to the Chip8 project.  
Be warned, for now putting `build-depends: SFML` in your project's cabal file will cause a compilation error as it fetches Hackage.

Moreover, when building SFML, you'll see some <ins>warnings</ins> shown as <ins>errors</ins>, they look like this :
```log
cbits/SFML/Graphics/Shader_helper.c:6:5: error:
     warning: ‘sfShader_setVector2Parameter’ is deprecated [-Wdeprecated-declarations]
        6 |     sfShader_setVector2Parameter (shader, name, *vector);
          |     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
  |
6 |     sfShader_setVector2Parameter (shader, name, *vector);
  |     ^

In file included from cbits/SFML/Graphics/Shader_helper.c:1:0: error:
```

**They are not errors**, SFML successfully builds despite these "error" messages, so do not worry.