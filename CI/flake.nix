{
  description = "Cardano Deposit Wallet CI";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11"; };

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { system, ... }:
        with import nixpkgs { inherit system; };
        let shell = ps: mkShell { packages = ps; };
        in {
          devShells = {
            docs = shell [ gnused gnutar jq mdbook tree wget ];
            tests = shell [ gnutar jq screen wget curl ];
            checks = shell [ nixfmt-classic shfmt ];
            release = shell [ gnused gh jq curl ];
            default = shell [ just ];
          };
        };

    };

}
