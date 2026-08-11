// AudioWorklet processor: queues interleaved stereo F32 chunks posted from
// the main thread and feeds the output; underflow plays silence. Reports
// queued milliseconds back for the status line.
class RocWebAudio extends AudioWorkletProcessor {
  constructor() {
    super();
    this.chunks = [];
    this.offset = 0;
    this.queued = 0; // interleaved samples across all chunks
    this.port.onmessage = (e) => {
      this.chunks.push(e.data);
      this.queued += e.data.length;
    };
  }

  process(_inputs, outputs) {
    const left = outputs[0][0];
    const right = outputs[0][1] ?? left;
    for (let i = 0; i < left.length; i += 1) {
      const chunk = this.chunks[0];
      if (chunk === undefined) {
        left[i] = 0;
        right[i] = 0;
        continue;
      }
      left[i] = chunk[this.offset];
      right[i] = chunk[this.offset + 1] ?? 0;
      this.offset += 2;
      this.queued -= 2;
      if (this.offset >= chunk.length) {
        this.chunks.shift();
        this.offset = 0;
      }
    }
    this.port.postMessage((this.queued / 2 / 48000) * 1000);
    return true;
  }
}

registerProcessor('roc-web-audio', RocWebAudio);
