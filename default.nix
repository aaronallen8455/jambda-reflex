# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  name = "jambda-project";

  packages = {
    app = ./app;
  };

  shells = {
    ghc = ["app"];
  };
})
