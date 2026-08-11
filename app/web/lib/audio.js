// Audio path: drain the host's F32 ring each frame and post transferable
// chunks to the AudioWorklet (no SharedArrayBuffer, no special headers —
// see the roc-web design notes). Resumes the context on first gesture.
export async function createAudio() {
  let ctx = null, node = null, read = 0, queuedMs = 0;

  try {
    ctx = new AudioContext({ sampleRate: 48000 });
    await ctx.audioWorklet.addModule(new URL('./audio-worklet.js', import.meta.url));
    node = new AudioWorkletNode(ctx, 'roc-web-audio', { outputChannelCount: [2] });
    node.port.onmessage = (e) => { queuedMs = e.data; };
    node.connect(ctx.destination);
    const resume = () => { ctx.resume(); };
    window.addEventListener('keydown', resume, { once: true });
    window.addEventListener('pointerdown', resume, { once: true });
  } catch (e) {
    console.warn('[roc-web] audio unavailable:', e.message);
  }

  return {
    pump(memory, exports) {
      if (!node) return;
      const size = exports.audio_ring_samples();
      const write = exports.audio_write_index();
      let pending = (write - read) >>> 0;       // wrapping u32 distance
      if (pending === 0) return;
      if (pending > size) {                     // overrun: skip to freshest
        read = write - size;
        pending = size;
      }
      const ring = new Float32Array(memory.buffer, exports.audio_ring_ptr(), size);
      const out = new Float32Array(pending);
      for (let i = 0; i < pending; i += 1) out[i] = ring[(read + i) % size];
      read = write;
      node.port.postMessage(out, [out.buffer]);
    },
    health() {
      if (!node) return 'no audio';
      return ctx.state === 'running' ? `audio ${queuedMs | 0}ms` : 'audio: press a key';
    },
  };
}
