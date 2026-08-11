// WebGPU backend: per-frame writeTexture + fullscreen triangle, nearest
// sampling. Two-method seam shared by all backends: uploadTexture/renderTexture.
export async function create(canvas, w, h) {
  const adapter = await navigator.gpu?.requestAdapter();
  if (!adapter) throw new Error('no WebGPU adapter');
  const device = await adapter.requestDevice();
  const context = canvas.getContext('webgpu');
  const format = navigator.gpu.getPreferredCanvasFormat();
  context.configure({ device, format, alphaMode: 'opaque' });

  const texture = device.createTexture({
    size: [w, h],
    format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  });
  const sampler = device.createSampler({ magFilter: 'nearest', minFilter: 'nearest' });
  const shader = device.createShaderModule({ code: `
    struct VSOut { @builtin(position) pos: vec4f, @location(0) uv: vec2f }
    @vertex fn vs(@builtin(vertex_index) i: u32) -> VSOut {
      let xy = vec2f(f32((i << 1u) & 2u), f32(i & 2u));
      var out: VSOut;
      out.pos = vec4f(xy * 2.0 - 1.0, 0.0, 1.0);
      out.uv = vec2f(xy.x, 1.0 - xy.y);
      return out;
    }
    @group(0) @binding(0) var samp: sampler;
    @group(0) @binding(1) var tex: texture_2d<f32>;
    @fragment fn fs(in: VSOut) -> @location(0) vec4f {
      return textureSample(tex, samp, in.uv);
    }`});
  const pipeline = device.createRenderPipeline({
    layout: 'auto',
    vertex: { module: shader, entryPoint: 'vs' },
    fragment: { module: shader, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  });
  const bindGroup = device.createBindGroup({
    layout: pipeline.getBindGroupLayout(0),
    entries: [
      { binding: 0, resource: sampler },
      { binding: 1, resource: texture.createView() },
    ],
  });

  return {
    uploadTexture(buffer) {
      device.queue.writeTexture({ texture }, buffer, { bytesPerRow: w * 4, rowsPerImage: h }, [w, h]);
    },
    renderTexture() {
      const encoder = device.createCommandEncoder();
      const pass = encoder.beginRenderPass({ colorAttachments: [{
        view: context.getCurrentTexture().createView(),
        loadOp: 'clear', clearValue: { r: 0, g: 0, b: 0, a: 1 }, storeOp: 'store',
      }]});
      pass.setPipeline(pipeline);
      pass.setBindGroup(0, bindGroup);
      pass.draw(3);
      pass.end();
      device.queue.submit([encoder.finish()]);
    },
  };
}
