"""Generate source-derived legacy faucet vectors without using the Rust SDK.

Requires Python cryptography. This is a fixture authoring tool, not a CI oracle.
See ../faucet.md for the serialization authorities and pending review.
"""
from hashlib import sha256
from pathlib import Path
from struct import pack
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey

key = Ed25519PrivateKey.from_private_bytes(bytes.fromhex(
    "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
sender, recipient = bytes([7]) * 32, bytes([8]) * 32
amount, now, nonce = 2_000_000_001, 1000, 10
for name in ("normal", "scheduled"):
    if name == "normal":
        payload = bytes([3]) + recipient + pack(">Q", amount)
        energy = 60 + len(payload) + 100 + 300
    else:
        count = 4
        portion, remainder = divmod(amount, count)
        payload = bytes([19]) + recipient + bytes([count])
        for index in range(1, count + 1):
            payload += pack(">QQ", (now + (index + 1) * 300) * 1000,
                            portion + (remainder if index == 1 else 0))
        energy = 60 + len(payload) + 100 + count * 364
    header = sender + pack(">QQIQ", nonce, energy, len(payload), now + 300)
    signature = key.sign(sha256(header + payload).digest())
    # Credential count, credential index, key count, key index, u16 signature length.
    transaction = bytes([1, 0, 1, 0]) + pack(">H", len(signature)) + signature + header + payload
    Path(__file__).with_name(name + ".bin").write_bytes(transaction)
    print(name, len(transaction), sha256(bytes([0]) + transaction).hexdigest())
