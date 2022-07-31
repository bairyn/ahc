# Overview

WIP scratch: Haskell2010-compliant Haskell compiler in asm for supported
platforms with minimal dependencies.  Suitable for bootstrapping Haskell.

Scrapped: actually it's probably easier overall to just add an existing
Haskell2010 implementation to auto-generate, say, `x86_64-linux` assembler or
JVM code or whatever, where the output then can bootstrap  itself; rather than
starting from the assembler from scratch, without an implementation dependency.
We can and will support multiple platforms.  So then if you need to port to a
new platform, just add a new platform to support, and then use an existing
supported implementation to cross-compile for that platform, to start
bootstrapping compiling on the new platform.  You probably could manually write
e.g. assembler for it, but having an existing Haskell2010 implementation
bootstrap it for you seems more efficient.

Probably also we'll have minimal dependencies, with just a working Haskell2010
implementation.  We can provide our own libraries e.g. with a Haskell2010 AST,
and if needed a bridge package can be created to cast between equivalent ASTs
from separate packages.

(I guess also I'll need a new name or ‘ahc’.  Amazing Haskell Compiler?  Wasn't
there a third one?  ‘Alternative Haskell Compiler’?)

## Scratch

AHC aims to be
- Bootstrappable from Haskell2010 alone.
- To require no RTS or for it to at least be optional.
- TODO

TODO:
- two ahc variants: one with minimal dependencies except just Haskell2010 with
  the ‘haskell2010’ package (not even base), perhaps with its own simple
  implementations for various components like parsing, and then a fuller
  variant with more dependencies.   They work the same if you ignore those
  dependencies, but having 2 variants gives you one option with better build
  complexity, perhaps useful for porting and bootstrapping.  In case of
  porting, essentially, you just need a working Haskell2010 implementation
  (possibly ahc itself, but possibly another) to build ahc, but once you build
  ahc, you can cross-combine ahc to another platform, and that ahc
  implementation can enable you to bootstrap an entire Haskell2010 setup.
