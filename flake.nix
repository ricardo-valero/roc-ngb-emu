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
      check-blargg = pkgs.callPackage ./checks/blargg/package.nix {inherit roc;};
      check-mooneye = pkgs.callPackage ./checks/mooneye/package.nix {inherit roc;};
      check-acid2 = pkgs.callPackage ./checks/acid2/package.nix {inherit roc;};
      check-sound = pkgs.callPackage ./checks/sound/package.nix {inherit roc;};
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
