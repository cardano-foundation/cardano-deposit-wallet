{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }:
        let
          runmd = self'.packages.runmd;
          check-docs = { recipe }:
            import ./test/test-docs.nix { inherit self pkgs runmd recipe; };
        in {
          checks.docs-simple = check-docs { recipe = "README-simple.yml"; };
          checks.docs-logging = check-docs { recipe = "README-logging.yml"; };
          checks.docs-echoing = check-docs { recipe = "README-echoing.yml"; };
          packages.default = runmd;
          haskellProjects.default = {
            devShell = {
              tools = hp: {
                fourmolu = hp.fourmolu;
                cabal-fmt = hp.cabal-fmt;
              };
            };
          };
        };
    };
}
