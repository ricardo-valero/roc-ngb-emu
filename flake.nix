{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # Pinned rev = roc nightly-2026-08-20-9e3980a (roc-overlay PR #8 head);
    # paired fleet-wide 2026-08-22: roc-web v0.4.0, roc-ray file-io
    # .roc-version, and the sibling repos' flakes. Keep them in sync.
    roc-overlay.url = "github:roc-lang/roc-overlay/c4408620942a1d5fe397c044ce0619feb84750de";
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
