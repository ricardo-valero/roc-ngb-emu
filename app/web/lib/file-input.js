// Runtime file loading: drag-and-drop anywhere, or a picker button.
// Hands raw bytes to the callback — what they mean is the app's business.
export function attachFileInput(onBytes) {
  const pick = document.body.appendChild(Object.assign(document.createElement('input'), {
    type: 'file',
    className: 'roc-web-file',
  }));
  pick.addEventListener('change', async () => {
    if (pick.files[0]) onBytes(new Uint8Array(await pick.files[0].arrayBuffer()));
  });

  window.addEventListener('dragover', (e) => e.preventDefault());
  window.addEventListener('drop', async (e) => {
    e.preventDefault();
    const file = e.dataTransfer?.files?.[0];
    if (file) onBytes(new Uint8Array(await file.arrayBuffer()));
  });
}
