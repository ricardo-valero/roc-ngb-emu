## 1. Upstream contribution

- [ ] 1.1 Open an issue on lukewilliamboswell/roc-ray proposing `read_bytes!` (binary sibling of `read_file!`), mentioning the save-persistence case for a future `write_bytes!`
- [ ] 1.2 Implement in a roc-ray checkout: host effect in the Zig layer, `HostHost.roc` transport, `Host.roc` public method with docs; follow their contribution/CI conventions
- [ ] 1.3 Review the diff together, then submit the PR and track review

## 2. Adoption (after a roc-ray release ships it)

- [ ] 2.1 Author design.md and the `play-app` delta spec against the released API shape
- [ ] 2.2 Bump flake nightly + roc-ray platform URL as a pair; `example/play.roc` loads the ROM at runtime; drop the `rom/play.gb` seeding from `fetch-roms`
- [ ] 2.3 Suites stay green (Blargg, check-acid2); README play instructions updated
