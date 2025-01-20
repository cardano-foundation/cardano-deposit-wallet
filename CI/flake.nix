{
  description = "Cardano Deposit Wallet CI";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ ];
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      # https://flake.parts/index.html
      perSystem =
        { system, ... }:
        {
          devShells.docs =
            with import nixpkgs { inherit system; };
            mkShell {
              packages = [ mdbook tree gnused ];
            };
        };

    };

}
