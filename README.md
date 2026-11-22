# cl-sip

A SIP user agent in Common Lisp, with SDP, enough RTP for G.711 audio and
DTMF, and a phone server that answers calls with a Claude voice agent and
can act as a small PBX.

The stack follows RFC 3261 (with the transaction fixes of RFC 6026 and
RFC 4320, and `rport` from RFC 3581), digest authentication (RFC 7616,
RFC 8760), SDP offer/answer (RFC 8866, RFC 3264) and RTP (RFC 3550,
RFC 3551, RFC 4733). The RFC texts are in `docs/rfcs/`.

It can register, place and receive calls, cancel, hang up, re-INVITE, send
UPDATE and REFER, and subscribe to and send event notifications (RFC 6665),
over UDP or TCP. It is not a proxy and has no TLS or SRTP.
[`docs/implementation-plan.md`](docs/implementation-plan.md) covers the
design and what is left out.

## Loading

Requires SBCL and Quicklisp. Dependencies: `alexandria`, `babel`,
`bordeaux-threads`, `ironclad`, `usocket`; `com.inuoe.jzon` for
`cl-sip/voice`; `fiveam` for the tests.

```lisp
(push #p"/path/to/cl-sip/" asdf:*central-registry*)
(ql:quickload "cl-sip")
(asdf:test-system "cl-sip")
```

| System | Contents |
|---|---|
| `cl-sip` | The SIP stack, SDP (`cl-sip.sdp`) and RTP (`cl-sip.rtp`) |
| `cl-sip/voice` | The phone server and voice agent (`cl-sip.voice`) |
| `cl-sip/examples` | Example programs (`cl-sip.examples`) |
| `cl-sip/test` | The test suite |

## Using the stack

```lisp
(defvar *ua*
  (cl-sip:make-user-agent
   :aor "sip:alice@example.com"
   :transports '((:udp :host "0.0.0.0" :port 5060))
   :credentials '(("example.com" "alice" "secret"))))   ; (realm user password)

(cl-sip:start *ua*)

;; Register. The registration refreshes itself.
(defvar *registration*
  (cl-sip:register *ua* :expires 3600
                        :on-event (lambda (registration event &key &allow-other-keys)
                                    (print (list registration event)))))

;; Answer incoming calls.
(setf (cl-sip:ua-on-incoming-call *ua*)
      (lambda (call)
        (let ((offer (cl-sip.sdp:parse-sdp (cl-sip:call-remote-sdp call))))
          (cl-sip:ring call)
          (cl-sip:answer call :sdp (cl-sip.sdp:sdp-string
                                    (cl-sip.sdp:make-answer offer :address "192.0.2.10"
                                                                  :port 40000))))))

;; Place a call.
(defvar *call*
  (cl-sip:invite *ua* "sip:bob@example.com"
                 :sdp (cl-sip.sdp:sdp-string
                       (cl-sip.sdp:make-audio-offer :address "192.0.2.10" :port 40000))
                 :on-event (lambda (call event &key &allow-other-keys)
                             ;; :progress :ringing :established :terminated ...
                             (print (list event (cl-sip:call-state call))))))

(cl-sip:hangup *call*)            ; sends CANCEL, a rejection or BYE, as needed
(cl-sip:unregister *registration*)
(cl-sip:stop *ua*)
```

Callbacks run on the stack's event-loop thread. Keep them short and don't
block in them. They may call any `cl-sip` function. Calls made from other
threads are passed to the loop automatically. `cl-sip:wait-until` waits for
a condition from an application thread.

### Examples

`(ql:quickload "cl-sip/examples")` loads three programs:

- `register-example` registers with a server.
- `start-echo-uas` answers calls and echoes the caller's RTP.
- `call-example` calls a URI, sends a tone and a DTMF digit, and hangs up.

```lisp
(defvar *uas* (cl-sip.examples:start-echo-uas :port 5070))
(cl-sip.examples:call-example "sip:echo@127.0.0.1:5070")
;; => :LOCAL-HANGUP, 57
```

## The phone server

`cl-sip/voice` is a phone server. Softphones register with it or dial it
directly. A call to the agent goes like this:

1. whisper.cpp transcribes the caller's speech locally.
2. The text goes to Claude (Haiku 5.5 by default) through the Messages API.
3. The reply is spoken back one sentence at a time as it streams in.

If the caller talks over the agent, the agent stops speaking.

### Running it

The Nix flake's dev shell provides `whisper-cli`, `curl`, the whisper model
(`$WHISPER_MODEL`) and `pjsua` for testing. Speech is synthesized with the
macOS `say` command by default. The server needs `ANTHROPIC_API_KEY`.

```sh
bin/voice-server            # check the configuration, then run
bin/voice-server --check    # check only
bin/voice-server --help     # list the settings
```

The script enters the dev shell if needed, loads `.envrc.local` if it
exists, and reports each setting as ok or as a problem without printing
values. It then starts the server on 127.0.0.1:5060 (UDP and TCP). Ctrl-C
stops it and removes any trunk registration.

From a REPL inside the dev shell:

```lisp
(ql:quickload "cl-sip/voice")
(defvar *server* (cl-sip.voice:start-server))
(cl-sip.voice:stop-server *server*)
```

### Connecting a softphone

| Setting | Value |
|---|---|
| SIP server / domain | `127.0.0.1`, port 5060 |
| User name | anything, e.g. `100` |
| Password | anything (unless you configure extensions or `:users`) |
| Transport | UDP or TCP |
| SRTP / TLS | off |

| Dial | Result |
|---|---|
| `600` | Echo test |
| `700` | A spoken test message, then hang-up |
| anything else | The voice agent |

The `:dialplan` argument controls routing (default: `*default-dialplan*`).
Each entry maps a pattern to `:agent`, `:echo`, `(:say "text")`,
`(:play "file.wav")`, `(:reject status)` or a function.

By default only the local machine can register and call. Requests from
other addresses are dropped without a response. To allow devices on your
network:

```sh
VOICE_HOST=0.0.0.0 VOICE_ALLOW=192.168.1.0/24 bin/voice-server
```

`start-server` takes `:stt`, `:llm` and `:tts` objects, so each stage can be
replaced. `make-claude-llm` accepts `:model`, `:system` and `:effort`;
`make-say-tts` accepts `:voice`.

### A phone number (Flowroute)

If Flowroute SIP credentials are set, the server registers with Flowroute
and the agent answers calls to your number:

```sh
export FLOWROUTE_USERNAME=...     # the tech prefix (Interconnections > Registration)
export FLOWROUTE_PASSWORD=...     # the SIP password shown there
export FLOWROUTE_POP=us-west-or   # or us-east-va
bin/voice-server
```

In the Flowroute portal, route the number to "SIP registration" on the same
point of presence.

With a trunk configured, the server:

- listens on all interfaces,
- learns its public address from Flowroute's responses and re-registers
  with it,
- sends a keep-alive every 25 seconds to hold the NAT mapping open,
- accepts requests only from the local machine, `VOICE_ALLOW` networks and
  Flowroute's signalling addresses.

Flowroute does not relay audio. Audio travels directly between the
carrier's gateway and your machine, so your router must let the RTP port
through:

| Option | Setting | Notes |
|---|---|---|
| Port forwarding | `VOICE_RTP_PORTS=40000-40019`, with that UDP range forwarded to this machine | Most reliable. Each call uses one port. |
| STUN | `VOICE_STUN=host:port` | No router changes. Fails behind symmetric NAT. |
| Neither | | Works only if the router preserves port numbers. |

Every agent call costs API tokens. Use `VOICE_ALLOWED_CALLERS` to restrict
who the agent answers, and `VOICE_MAX_CALLS` (default 2) to limit
concurrent agent calls.

Trunk calls are routed by the called number, so a dialplan entry like
`("12065550100" :agent)` applies to them.

The same configuration from Lisp:

```lisp
(cl-sip.voice:start-server
 :host "0.0.0.0"
 :trunk '(:registrar "sip:us-west-or.sip.flowroute.com"
          :username "12345678" :password "...")
 :rtp-port-range '(40000 . 40019)
 :allowed-callers '("2065550199"))
```

To debug a trunk, set `VOICE_SIP_TRACE=trunk` to log the SIP messages
exchanged with the provider, or `all` to log every message. Credentials are
removed from the log.

### Extensions

With an extension list, the server works as a small PBX. Phones register
under their extension numbers and call each other, and the server relays
the audio.

Put the list in `extensions.lisp` in the repository root, or name a
different file with `VOICE_EXTENSIONS`. `extensions.lisp` is git-ignored
because it contains passwords.

```lisp
("100" :password "s3cret" :name "Alice")
("101" :password "hunter2" :name "Bob" :ring-seconds 20)
("200" :ring ("100" "101") :name "Front desk")   ; a group: both phones ring
```

Each softphone uses its extension number as the user name, its password,
and the server's address. Once a list exists:

- only listed extensions can register, each only as itself;
- every call a phone places must authenticate;
- dialling an extension rings all phones registered to it (or to each
  member of a group), and the first to answer takes the call;
- numbers that aren't extensions still go through the dialplan.

To send trunk calls to phones, list your Flowroute number as a group:
`("12065550100" :ring ("100"))`.

**Forwarding.** `:forward` forwards every call. `:busy` applies when the
phone is busy or declines. `:no-answer` applies when nobody answers within
`:ring-seconds` (default `VOICE_RING_SECONDS`, 30) or no phone is
registered. The target is an extension number or `:agent`.

```lisp
("101" :password "hunter2" :name "Bob" :busy "100" :no-answer :agent)
```

The phone that finally rings gets the call's forwarding history in
`History-Info`. A call forwarded more than five times is refused as a loop.

**Hold and transfer.** These work from the phone's own buttons. The server
carries out blind and attended transfers itself. It joins the remaining
parties and releases the transferring phone. If the transfer target doesn't
answer, the original call continues. You can transfer a call to the agent.

**Attendant.** The agent can connect a caller to an extension. The caller
either types the number on the keypad (`#` to finish, `*` to start over) or
asks for a person, group or number. The agent knows the names and numbers
in the list and connects the call with its `transfer_call` tool. The caller
hears ringing. If someone answers, the agent drops off; if not, the agent
says so and continues the conversation. The agent can only reach listed
extensions and groups, never outside numbers.

To make the agent greet outside callers as an attendant:

```sh
export VOICE_GREETING="Hello. Which extension should I dial?"
```

**Outbound calls.** Set `VOICE_OUTBOUND_CALLER_ID` to one of your Flowroute
numbers and mark the extensions allowed to dial out:

```lisp
("100" :password "s3cret" :name "Alice" :outbound t)
```

Ten- and eleven-digit numbers dialled from those phones go to the provider,
with that number as the caller ID. Calls that arrived from the trunk are
never sent back out. A forwarding target may be an outside number if the
forwarding extension is `:outbound`. So far this has only been tested
against a stand-in provider, not against Flowroute.

**Busy lamps.** Softphones can subscribe to an extension's `dialog` event.
The server reports the extension as ringing, in a call, or idle.

[`docs/pbx-plan.md`](docs/pbx-plan.md) describes the design and which RFCs
each feature follows. It also lists what isn't implemented: voicemail and
message waiting, call pickup, conferences, and music on hold.

### Wiki lookups

With a token for the LLM-wiki MCP server, the agent can look things up in
the wiki during a call. Put these in `.envrc.local`:

```sh
VOICE_WIKI_TOKEN=...                           # must carry the mcp capability
VOICE_WIKI_URL=http://localhost:8788/mcp       # the default
```

`bin/voice-server` checks that the wiki accepts the token.

The agent can use all of the wiki's tools except `ask_wiki`, which is too
slow for a phone call. Set `VOICE_WIKI_EXCLUDE` (comma-separated) to change
which tools are withheld. Instead of `ask_wiki`, the agent does the lookup
itself on each turn:

1. It searches for what the caller said (about 0.2 s).
2. It reads the most relevant pages, searching again if needed.
3. It answers briefly from those pages.

The wiki's `index` is included in the system prompt with prompt caching, so
it costs little after the first turn of a call. Before calling a tool, the
agent says something short like "Let me check". Each tool call is logged as
a `tool:` line. Pages the agent reads are kept for one reply only; the
conversation itself is kept for the whole call.

If the wiki is down or rejects the token, the agent continues without it
and logs the reason. If the wiki doesn't respond, the agent leaves it alone
for a minute, so a caller waits five seconds once rather than on every turn.

Anyone who reaches the agent can ask it about the wiki. Set
`VOICE_ALLOWED_CALLERS` when a trunk is connected.

From Lisp, any MCP server over HTTP works, and so do plain functions:

```lisp
(cl-sip.voice:start-server
 :llm (cl-sip.voice:make-claude-llm
       :tools (list (cl-sip.voice:make-mcp-server
                     :url "http://localhost:8788/mcp" :token "...")
                    (cl-sip.voice:make-function-tool
                     "current_time" "The current time."
                     (lambda (input) (declare (ignore input)) "half past nine")))))
```

### Voice cloning

`make-chatterbox-tts` uses the Chatterbox Turbo model, which can imitate a
voice from about ten seconds of clean recorded speech. It runs
`tools/chatterbox_tts.py` under `uv` as a long-running worker. The first run
downloads the model (4 GB) unless it's already in the Hugging Face cache.

```sh
VOICE_TTS=chatterbox VOICE_REFERENCE=me.wav bin/voice-server
```

```lisp
(cl-sip.voice:start-server
 :tts (cl-sip.voice:make-chatterbox-tts :voice "me.wav"))
```

On an Apple Silicon laptop, synthesizing a sentence takes about as long as
speaking it. Replies start two to three seconds later than with `say`, and
the model takes about 30 seconds to load.

Other speech programs can be used too. `make-worker-tts` runs a long-lived
program that speaks the worker protocol described in the script's header.
`make-command-tts` runs a program once per sentence:

```lisp
(cl-sip.voice:make-command-tts '("my-tts" "--text" :text "--out" :output))
```

### Settings

`bin/voice-server` reads these from the environment or `.envrc.local`.

| Variable | Meaning |
|---|---|
| `ANTHROPIC_API_KEY` | Claude API key (required) |
| `VOICE_HOST`, `VOICE_PORT` | Listen address. Default 127.0.0.1 (0.0.0.0 with a trunk), port 5060 |
| `VOICE_ALLOW` | Comma-separated networks allowed to register and call |
| `VOICE_GREETING` | What the agent says when it answers |
| `VOICE_EXTENSIONS` | Extension list file. Default `extensions.lisp` if present |
| `VOICE_RING_SECONDS` | How long an extension rings. Default 30 |
| `FLOWROUTE_USERNAME`, `FLOWROUTE_PASSWORD` | Trunk credentials. Set both or neither |
| `FLOWROUTE_POP` | `us-west-or` (default) or `us-east-va` |
| `VOICE_RTP_PORTS` | UDP port range forwarded to this machine, e.g. `40000-40019` |
| `VOICE_STUN` | STUN server, `host:port` |
| `VOICE_ALLOWED_CALLERS` | Comma-separated numbers the agent answers |
| `VOICE_MAX_CALLS` | Maximum concurrent agent calls. Default 2 |
| `VOICE_OUTBOUND_CALLER_ID` | Your Flowroute number. Lets `:outbound` extensions dial out |
| `VOICE_SIP_TRACE` | `trunk` or `all`. Logs SIP messages, without credentials |
| `VOICE_WIKI_TOKEN` | LLM-wiki MCP token |
| `VOICE_WIKI_URL` | Wiki server address. Default `http://localhost:8788/mcp` |
| `VOICE_WIKI_EXCLUDE` | Wiki tools to withhold. Default `ask_wiki` |
| `VOICE_TTS` | `chatterbox` to use the voice-cloning model |
| `VOICE_REFERENCE` | Voice recording for Chatterbox to imitate |

## Layout

| Path | Contents |
|---|---|
| `src/uri.lisp`, `headers.lisp`, `message.lisp`, `parser.lisp`, `serializer.lisp` | Message parsing and serialization |
| `src/auth.lisp` | Digest authentication, client and server |
| `src/scheduler.lisp` | Event loop with a real or virtual clock |
| `src/transport*.lisp` | In-memory, UDP and TCP transports |
| `src/transaction.lisp` | Client and server transaction state machines |
| `src/dialog.lisp`, `ua.lisp`, `registration.lisp`, `call.lisp` | Dialogs and the user agent core |
| `src/events.lisp` | SUBSCRIBE and NOTIFY |
| `src/sdp/` | SDP and offer/answer |
| `src/rtp/` | RTP packets, G.711, DTMF, STUN, a simple RTP session |
| `src/voice/` | Phone server, voice agent, speech backends, MCP client, call bridging |
| `t/` | Tests |
| `bin/voice-server` | Launcher for the phone server |
| `docs/` | Design notes and RFC texts |

## Tests

```lisp
(asdf:test-system "cl-sip")
```

The protocol tests don't sleep. User agents share a virtual clock and an
in-memory network, so a test can drop or duplicate specific messages and run
through 32 seconds of retransmission timers instantly. `t/harness.lisp`
shows how. The same functions (`make-scheduler :virtual t`,
`make-memory-network`, `make-memory-transport`, `pump`, `advance-time`) are
exported for use at the REPL.

## License

MIT.
