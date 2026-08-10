{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    roc-overlay.url = "github:roc-lang/roc-overlay";
    roc-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = {
    self,
    nixpkgs,
    roc-overlay,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
  in {
    packages = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      roc-pkgs = roc-overlay.packages.${system};
      self-pkgs = self.packages.${system};
    in {
      fetch-roms = pkgs.callPackage ./nix/fetch-roms.nix {};
      run-blargg = pkgs.callPackage ./nix/run-blargg.nix {
        inherit (self-pkgs) fetch-roms;
        roc = roc-pkgs.nightly;
      };
      check-acid2 = pkgs.callPackage ./nix/check-acid2.nix {
        inherit (self-pkgs) fetch-roms;
        roc = roc-pkgs.nightly;
      };
      check-sound = pkgs.callPackage ./nix/check-sound.nix {
        inherit (self-pkgs) fetch-roms;
        roc = roc-pkgs.nightly;
      };
      run-ladder = pkgs.callPackage ./nix/run-ladder.nix {
        inherit (self-pkgs) fetch-roms;
        roc = roc-pkgs.nightly;
      };
    });
    devShells = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      roc-pkgs = roc-overlay.packages.${system};
      self-pkgs = self.packages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = builtins.attrValues {
          inherit (pkgs) nixd alejandra;
          inherit (roc-pkgs) nightly;
          inherit (self-pkgs) fetch-roms run-blargg check-acid2 check-sound run-ladder;
        };
      };
    });
  };
}
