# play-app Delta: PCM Audio Streaming

## ADDED Requirements

### Requirement: Game audio reaches the speakers
The play app SHALL create a PCM output stream matching the APU's native format (48 kHz, stereo, interleaved F32) during initialization, and SHALL pace emulation by the audio clock: each render tick runs emulated frames (bounded per tick) until the stream holds a target depth of queued samples, draining and pushing every sample each emulated frame produces. This supersedes the fixed one-emulated-frame-per-render-tick cadence — a tick that finds the queue at target runs zero frames, so the render-rate vs Game-Boy-rate mismatch surfaces as an occasionally repeated video frame, never as audio drops. Buffering between pushes and the audio device SHALL be host-owned: underrun SHALL play silence (no crash, no pitch or tempo artifacts, clean resume), and sustained overrun SHALL drop the oldest buffered samples rather than grow without bound.

#### Scenario: Game sound is audible
- **WHEN** a game ROM enables an APU channel (e.g. the Pokémon Crystal title fanfare) while the play app runs
- **THEN** the corresponding audio plays through the system output in real time alongside the video

#### Scenario: Stalled emulation recovers silently
- **WHEN** sample production pauses or falls behind real time (window drag, debugger stall)
- **THEN** playback degrades to silence without crashing and resumes cleanly once samples flow again, at correct pitch

#### Scenario: Sample production outpaces playback
- **WHEN** the emulator sustainedly produces samples faster than the device consumes them
- **THEN** the host drops the oldest buffered samples, keeping memory bounded and audio near real time
