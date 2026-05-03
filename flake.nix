{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [hpkgs.hspec]);
      in
      {
        devShell = pkgs.mkShell {
          name = "skully-dev-shell";
          buildInputs = [
            pkgs.cabal-install
            ghc
            pkgs.wget
          ];
        };
      }
    );
}
