```
$ nix develop .. --command cabal build thinning-bug
Configuration is affected by cabal.project at '/home/shlevy/public/master'.
Resolving dependencies...
Build profile: -w ghc-9.12.2 -O1
In order, the following will be built (use -v for more details):
 - thinning-bug-0.1.0.0 (lib:signature) (first run)
 - thinning-bug-0.1.0.0 (lib) (first run)
Configuring library 'signature' for thinning-bug-0.1.0.0...
Preprocessing library 'signature' for thinning-bug-0.1.0.0...
Building library 'signature' instantiated with Signature = <Signature>
for thinning-bug-0.1.0.0...
[1 of 1] Compiling Signature[sig]   ( signature/Signature.hsig, nothing )
Configuring library for thinning-bug-0.1.0.0...
Preprocessing library for thinning-bug-0.1.0.0...
Building library instantiated with Signature = <Signature>
for thinning-bug-0.1.0.0...
<no location info>: warning: []8;;https://errors.haskell.org/messages/GHC-42258\GHC-42258]8;;\] [-Wunused-packages]
    The following packages were specified via -package or -package-id flags,
    but were not needed for compilation:
      - thinning-bug-0.1.0.0 (exposed by flag -package-id thinning-bug-0.1.0.0-inplace-signature[Signature=<Signature>])

[1 of 3] Compiling Signature[sig]   ( Signature.hsig, nothing )
[2 of 3] Compiling Module           ( Module.hs, nothing )
Module.hs:1:1: error: []8;;https://errors.haskell.org/messages/GHC-83249\GHC-83249]8;;\]
    Can't find interface-file declaration for variable {Signature.$tcT}
      Probable cause: bug in .hi-boot file, or inconsistent .hi file
      Use -ddump-if-trace to get an idea of which file caused the error
  |
1 | module Module where
  | ^

[3 of 3] Instantiating thinning-bug-0.1.0.0-inplace-signature
Error: [Cabal-7125]
Failed to build thinning-bug-0.1.0.0.
```
