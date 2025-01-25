# CHIP-8 Emulator... but in Haskell !

## CHIP-8

CHIP-8 is an old interpreted programming language, and I decided to create yet another interpreter for it, but in Haskell for fun !  
I first learned about it on this [French forum](https://zestedesavoir.com/tutoriels/1148/introduction-a-lemulation-console/) a while ago, there are a lot of C or C++ implementations online, and I thought it will be interesting to make it in Haskell instead.  

The [CHIP-8 wikipedia page](https://en.wikipedia.org/wiki/CHIP-8) has a lot of information about how to proceed.  

## SFML binding

I chose the SFML library (more precisely, its Haskell binding as it's a C++ library) because I have used it many times in the past.  
Any other graphic library will fit, as only very basic features are needed, even ncurses would be suitable...

At the time I'm writing this, the latest SFML 2.x version is 2.6.2 and 3.0.0 came out recently, but the Haskell binding is quite late being 2.3 only.  
Here's the [SFML bindings download page](https://www.sfml-dev.org/download/bindings/) and the [SFML haskell binding page on Hackage](https://hackage.haskell.org/package/SFML).  

## Build and run

Run (and build if not already) : `cabal run Chip8`  
Run with arguments (and build if not already) : `cabal run Chip8 -- path.ch8`  
Close the window either with the cross or by hitting Escape key.  

## ROMs

The `roms/` submodule contains an extensive collection of ROMs.  
Its `demos/` subdirectory contains basic programs, requiring little to no input, such as logos or a maze generator.  
If you want more, CHIP-8 ROMs are very easy to find online.  

## SFML errors ?

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

**They are not errors**, SFML successfully builds despite these scary-looking "error" messages, so do not worry.  

## SFML demos

The `Haskell-SFML/demos` subdirectory is contains some examples to show how to use the binding, but they don't build.  
To fix the issue, go to the repository root directory and execute the [setup.sh] script, it will create a `cabal.project` config file to link the SFML directory to the demos.  
Then, just do `cabal build` in the `demos` subdirectory and every demo will compile.  

## Testing

During the instructions implementation, I used various test ROMs from these two repositories :  
- `testROM/` : a general test ROM file by [BestCoder](https://zestedesavoir.com/@BestCoder) (reuploaded by [Karnaj](https://zestedesavoir.com/@Karnaj))
- `chip8-test-suite/` : a collection of small test ROMs to test many instructions (especially details like carry flag handling)
