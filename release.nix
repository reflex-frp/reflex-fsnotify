{ p ? import ./reflex-platform { __useNewerCompiler = true; }
}:
let
  inherit (p.nixpkgs) lib;
  ghc = p.ghc.override {
    overrides = self: super: {
      fsnotify = p.nixpkgs.haskell.lib.dontCheck (self.callHackage "fsnotify" "0.4.1.0" {});
    };
  };
in ghc.callCabal2nix "reflex-fsnotify" ./. {}
