# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  name = "jambda-project";

  packages = {
  # This has to match the name of the cabal package
    Jambda = ./app;
  };

  shells = {
    ghc = ["Jambda"];
  };

  overrides = self: super: {
    sdl2 = pkgs.haskell.lib.dontCheck (self.callHackage "sdl2" "2.4.1.0" {});
  };
})
