# ppu-verification Delta Spec

## MODIFIED Requirements

### Requirement: dmg-acid2 rendering oracle
Rendering SHALL be verified against the dmg-acid2 test ROM: a fetch script obtains the ROM (untracked, alongside the Blargg ROMs), and the rendered frame SHALL match the published reference image — confirmed visually once, then checked as a framebuffer digest by the suite. The digest check SHALL use compare-or-create semantics: when no golden digest exists, the check writes one (blessing a new golden = delete and re-run) together with a human-viewable image of the blessed frame; when the digest exists and the render mismatches, the check fails and writes the actual frame as an image for visual comparison against the blessed one.

#### Scenario: Reference render
- **WHEN** dmg-acid2 runs to its stable frame and the framebuffer is captured with a golden digest present
- **THEN** the output matches the frozen reference digest

#### Scenario: Blessing a golden
- **WHEN** the check runs with no golden digest present
- **THEN** it writes the digest and a viewable image of the frame, and reports that a new golden was created rather than passing silently

#### Scenario: Mismatch is reviewable
- **WHEN** the check fails against an existing golden
- **THEN** the actual frame is written as a viewable image alongside the blessed one
