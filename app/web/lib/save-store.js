// IndexedDB store for battery-backed cartridge saves (`.sav` bytes).
//
// Keys are cartridge identity — header title + global checksum — not
// filenames, so the default fetched ROM and a dropped copy of the same
// game share one save, and dropped files (which have no stable path)
// still key stably.

const DB_NAME = 'roc-web-saves';
const STORE = 'saves';

export function romKey(bytes) {
  let title = '';
  for (let i = 0x134; i <= 0x143 && i < bytes.length; i++) {
    const c = bytes[i];
    if (c === 0) break;
    title += c >= 0x20 && c < 0x7f ? String.fromCharCode(c) : '_';
  }
  const checksum = bytes.length > 0x14f ? (bytes[0x14e] << 8) | bytes[0x14f] : 0;
  return `${title}-${checksum.toString(16).padStart(4, '0')}`;
}

function open() {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open(DB_NAME, 1);
    req.onupgradeneeded = () => req.result.createObjectStore(STORE);
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

export async function createSaveStore() {
  const db = await open();
  return {
    async load(key) {
      return new Promise((resolve) => {
        const req = db.transaction(STORE).objectStore(STORE).get(key);
        req.onsuccess = () => resolve(req.result instanceof Uint8Array ? req.result : null);
        req.onerror = () => resolve(null); // a failed read is a clean start
      });
    },
    save(key, bytes) {
      // Fire-and-forget: a failed persist must not disturb gameplay
      try {
        db.transaction(STORE, 'readwrite').objectStore(STORE).put(bytes, key);
      } catch (e) {
        console.warn('[roc-web] save persist failed:', e);
      }
    },
  };
}
