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
      roc = roc-pkgs.nightly;
    in {
      check-blargg = pkgs.callPackage ./check/blargg/package.nix {inherit roc;};
      check-mooneye = pkgs.callPackage ./check/mooneye/package.nix {inherit roc;};
      check-acid2 = pkgs.callPackage ./check/acid2/package.nix {inherit roc;};
      check-sound = pkgs.callPackage ./check/sound/package.nix {inherit roc;};
    });
    devShells = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      roc-pkgs = roc-overlay.packages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = builtins.attrValues {
          inherit (pkgs) nixd alejandra python3;
          inherit (roc-pkgs) nightly;
        };
      };
    });
  };
}
