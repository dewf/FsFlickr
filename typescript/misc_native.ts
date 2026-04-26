async function hmacSha1(key: string, message: string) {
  const enc = new TextEncoder();

  const cryptoKey = await crypto.subtle.importKey(
    "raw",
    enc.encode(key),
    { name: "HMAC", hash: "SHA-1" },
    false,
    ["sign"]
  );

  const signature = await crypto.subtle.sign(
    "HMAC",
    cryptoKey,
    enc.encode(message)
  );

  // convert ArrayBuffer → base64 (OAuth expects base64)
  return btoa(String.fromCharCode(...new Uint8Array(signature)));
}

export { hmacSha1 };
