{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # SPIKE BRANCH (nightly-iter): pinned to roc-overlay PR #8 head as of
    # 2026-08-20 — the latest nightly, intentionally ahead of the platform
    # pairing (roc-web, roc-nes-emu, roc-ray's .roc-version). Do not merge
    # this pin to main without paired platform releases.
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
