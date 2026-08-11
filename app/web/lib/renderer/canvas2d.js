// Canvas2D backend: putImageData. The last-resort fallback.
export function create(canvas, w, h) {
  const ctx = canvas.getContext('2d');
  if (!ctx) throw new Error('no 2d context');
  const imageData = ctx.createImageData(w, h);
  return {
    uploadTexture(buffer) { imageData.data.set(buffer); },
    renderTexture() { ctx.putImageData(imageData, 0, 0); },
  };
}
