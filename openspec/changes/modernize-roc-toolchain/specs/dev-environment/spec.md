# dev-environment

## ADDED Requirements

### Requirement: Devshell provides the new Roc compiler
The Nix flake SHALL provide a default devshell whose `roc` binary is the new Zig-based compiler, sourced from `github:roc-lang/roc-overlay` pinned to a dated nightly release tag (not the floating `nightly` attribute).

#### Scenario: Entering the devshell
- **WHEN** a developer runs `nix develop` (or direnv activates `.envrc`) on a supported system (x86_64/aarch64 linux or darwin)
- **THEN** `roc version` succeeds and reports the pinned new-compiler nightly

#### Scenario: Reproducible pin
- **WHEN** the flake is evaluated on a fresh machine with only `flake.nix` and `flake.lock`
- **THEN** the same Roc nightly build is provided, with no dependency on the removed `github:roc-lang/roc` flake

### Requirement: Devshell provides nixd as the Nix language server
The devshell SHALL provide `nixd` (replacing `nil`), and the flake SHALL NOT define a `formatter` output.

#### Scenario: Language server available
- **WHEN** a developer enters the devshell
- **THEN** `nixd` is on PATH and `nil` is not provided by the shell

#### Scenario: No formatter output
- **WHEN** the flake outputs are inspected
- **THEN** no `formatter` attribute is exposed

### Requirement: No editor-specific settings in the repo
The repository SHALL NOT contain the `.zed/` settings directory.

#### Scenario: Fresh checkout
- **WHEN** the repository is cloned after this change
- **THEN** no `.zed/` directory exists

### Requirement: Legacy state preserved on a branch
The repository SHALL retain the pre-migration state (old flake, old syntax) on a branch named `legacy` pointing at the last pre-migration commit of `main`.

#### Scenario: Checking out the old toolchain
- **WHEN** a developer runs `git checkout legacy`
- **THEN** the working tree contains the 2024-era flake and un-migrated Roc sources
