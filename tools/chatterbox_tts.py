# /// script
# requires-python = ">=3.11,<3.12"
# dependencies = [
#     "chatterbox-tts",
#     # resemble-perth, which Chatterbox uses to watermark its output, needs
#     # pkg_resources and silently disables itself without it.
#     "setuptools<81",
# ]
# ///
"""Speech synthesis worker for cl-sip's voice agent, using Chatterbox Turbo.

Loads the model once, then serves requests on standard input, one JSON
object per line:

    {"text": "Hello there.", "out": "/tmp/x.wav"}

and answers each with one JSON line on standard output:

    {"ok": true, "seconds": 0.8, "audio_seconds": 1.2}
    {"ok": false, "error": "..."}

The first line written is {"ready": true, ...} once the model is loaded.
Output files are 16-bit mono WAV at --rate (8000 by default, for telephony).

With --voice, the model speaks in the voice of that reference recording
(a clean clip of ten seconds or so works well); without it, in its built-in
voice.

    uv run tools/chatterbox_tts.py --voice me.wav
"""

import argparse
import json
import os
import sys
import time
import wave

# The protocol owns standard output; anything libraries print goes to stderr.
protocol = os.fdopen(os.dup(sys.stdout.fileno()), "w", buffering=1)
os.dup2(sys.stderr.fileno(), sys.stdout.fileno())
sys.stdout = sys.stderr


def reply(**fields):
    protocol.write(json.dumps(fields) + "\n")
    protocol.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--voice", help="reference recording of the voice to imitate")
    parser.add_argument("--rate", type=int, default=8000, help="output sample rate")
    parser.add_argument("--device", help="torch device (default: mps if available, else cpu)")
    args = parser.parse_args()

    import torch
    import torchaudio.functional as F
    from chatterbox.tts_turbo import ChatterboxTurboTTS

    device = args.device or ("mps" if torch.backends.mps.is_available() else "cpu")
    model = ChatterboxTurboTTS.from_pretrained(device=device)
    if args.voice:
        model.prepare_conditionals(args.voice)

    def synthesize(text, out):
        audio = model.generate(text)  # float tensor, shape (1, n), at model.sr
        audio = F.resample(audio.detach().cpu(), model.sr, args.rate)
        pcm = (audio.squeeze(0).clamp(-1, 1) * 32767).to(torch.int16).numpy()
        with wave.open(out, "wb") as f:
            f.setnchannels(1)
            f.setsampwidth(2)
            f.setframerate(args.rate)
            f.writeframes(pcm.tobytes())
        return len(pcm) / args.rate

    # The first generation is many times slower than the rest; get it out of
    # the way before announcing readiness.
    model.generate("Hello.")

    reply(ready=True, device=device, rate=args.rate, voice=args.voice)

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            request = json.loads(line)
            started = time.monotonic()
            audio_seconds = synthesize(request["text"], request["out"])
            reply(ok=True, seconds=round(time.monotonic() - started, 3),
                  audio_seconds=round(audio_seconds, 3))
        except Exception as error:  # report and keep serving
            reply(ok=False, error=f"{type(error).__name__}: {error}")


if __name__ == "__main__":
    main()
