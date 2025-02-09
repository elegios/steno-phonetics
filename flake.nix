{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
      shell = pkgs.mkShell {
        name = "dev shell";
        buildInputs = [
          nixpkgs.legacyPackages.x86_64-linux.openssl
          nixpkgs.legacyPackages.x86_64-linux.pkg-config
          nixpkgs.legacyPackages.x86_64-linux.cargo
          nixpkgs.legacyPackages.x86_64-linux.rust-analyzer
          nixpkgs.legacyPackages.x86_64-linux.rustc
          nixpkgs.legacyPackages.x86_64-linux.rustfmt
        ];
      };
    in {
      devShells.x86_64-linux.default = shell;
    };
}
