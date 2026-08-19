# Move the Access Trace from Bus to Cpu

## Why

The access trace is a CPU conformance probe wearing a bus costume. It exists
for exactly one consumer — the single-step harness, whose vectors specify the
ordered memory accesses *each instruction* performs — and the decision to
record already lives entirely on the CPU side: `Bus.read` does **not** trace;
appends happen only when the CPU-path accessors (`Cpu.mem_read`/`mem_write`)
explicitly call `trace_access`/`read_traced`. Only the storage (`Bus.trace`)
sits in the bus, and that split is what produces the asymmetry in `Cpu.roc`:
`mem_write` hides the trace branch inside `Bus.trace_access` while `mem_read`
exposes it in a visible `match st.bus.trace`.

A bus-side trace is also the wrong instrument by construction: the PPU's tile
fetches, the debug renders, `GameBoy.peek`, and `run_until`'s LY polling all
go through the same `Bus.read`. A genuine bus-level recorder would capture
them; ours must not, and only the CPU knows which reads are its own
architectural accesses. In the emulator there are no pins — an instruction
fetch and a frontend peek are the same function call — so CPU-side is the
only place this trace *can* be authored correctly. Moving the storage to the
CPU makes the code's ownership match what the code already does, completes
what extract-cpu-module's design D4 half-admitted ("the trace records the
*CPU's* accesses … it belongs on the CPU side of the boundary even though the
ring lives in Bus"), and leaves `Bus` memoryless: it maps addresses to
devices for whoever asks, with no record of who asked.

## What Changes

- `Cpu` gains a `trace` field
  (`[NoTrace, Trace(List({ addr : U16, val : U8, dir : [Read, Write] }))]`),
  carried through the threaded `St` record. `Cpu.init` starts `NoTrace`.
- `mem_read` and `mem_write` become symmetric — both match on `st.trace` and
  append at the CPU's two chokepoints:

  ```
  mem_read = |st, addr|
      match st.trace {
          NoTrace => { st: st, value: st.bus.read(addr) }
          Trace(list) => {
              value = st.bus.read(addr)
              { st: { ..st, trace: Trace(list.append({ addr: addr, val: value, dir: Read })) }, value: value }
          }
      }
  ```

- `Bus` sheds the instrument entirely: the `trace` field, `trace_access`, and
  `read_traced` are deleted. `Bus.flat` stops secretly turning tracing on —
  it builds flat memory, nothing more.
- `GameBoy.from_raw` turns the probe on explicitly (`trace: Trace([])` on the
  `Cpu` it builds) — the coupling moves to the one constructor that wants it.
  `GameBoy.access_trace` reads `gb.cpu.trace`; its signature is unchanged, so
  `check/single-step` needs no edits.
- Comment updates where the old split was documented (`Bus.roc` trace-field
  comment, `Cpu.roc` `mem_read` header).
- No behavior change: trace contents, ordering, and every gate stay
  byte-identical.

## Capabilities

### New Capabilities

None — structural refactor.

### Modified Capabilities

None. `core-debug`'s access-trace requirement keeps its entry points
(`from_raw`, `step_instruction`, `access_trace` on `GameBoy`); only which
record stores the list moves. This change sets `skip_specs: true` in its
`.openspec.yaml` accordingly.

## Impact

- `package/Cpu.roc` — `trace` on the nominal record and `St`; symmetric
  `mem_read`/`mem_write`; `to_st`/`of_st` carry the field.
- `package/Bus.roc` — field and two methods deleted; `flat` simplified.
- `package/GameBoy.roc` — `from_raw` sets the trace; `access_trace` reads
  from `gb.cpu`.
- No callers outside the package change (`check/single-step` consumes the
  unchanged `GameBoy` surface).
- Gates (must stay green, unchanged): `roc check` / `roc test`, the SM83
  single-step suite (the trace's only consumer — it compares access placement
  per opcode, so it gates this move exactly), Blargg passlist with a
  wall-clock comparison (the hot `St`/`Cpu` records grow by one tag word),
  mooneye, acid2, sound, battery, both app builds.

## Non-Goals

- No PPU trace. By the same ownership principle a PPU fetch probe would be
  PPU-owned state — but it has no comparator in the check suite, and the
  scanline-at-once renderer would transcribe implementation order, not the
  hardware fetcher sequence. Revisit only alongside a fetcher-accurate
  renderer.
- No whole-system bus analyzer (record all masters inside `Bus.read`/`write`
  with caller attribution). That is a different instrument with a different
  owner — it *would* belong in `Bus` — and nothing consumes it today.
- No change to what is traced: dispatch's stack pushes stay traced, the
  interrupt poll's IE/IF reads stay untraced, exactly as before.
