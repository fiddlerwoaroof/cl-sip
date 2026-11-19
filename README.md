# cl-sip

A basic SIP user-agent stack in Common Lisp, written from the RFCs in
`docs/rfcs/`: RFC 3261 (with the transaction fixes of RFC 6026 and RFC 4320
and `rport` from RFC 3581), digest authentication (RFC 7616, RFC 8760), SDP
and offer/answer (RFC 8866, RFC 3264), and enough RTP to move G.711 audio
and DTMF events (RFC 3550, RFC 3551, RFC 4733).

It can register, place and receive calls, cancel, hang up, and re-INVITE,
over UDP or TCP. It is not a proxy and has no TLS; see
[`docs/implementation-plan.md`](docs/implementation-plan.md) for the design,
the full scope, and the list of what is left out.

## Loading

Dependencies: `alexandria`, `babel`, `bordeaux-threads`, `ironclad`,
`usocket` (plus `com.inuoe.jzon` for `cl-sip/voice` and `fiveam` for the
tests), all in Quicklisp. Developed on SBCL.

```lisp
(push #p"/path/to/cl-sip/" asdf:*central-registry*)
(ql:quickload "cl-sip")
(asdf:test-system "cl-sip")      ; runs the test suite
```

## Quick start

```lisp
(defvar *ua*
  (cl-sip:make-user-agent
   :aor "sip:alice@example.com"
   :transports '((:udp :host "0.0.0.0" :port 5060))
   :credentials '(("example.com" "alice" "secret"))))   ; (realm user password)

(cl-sip:start *ua*)

;; Register, refreshing automatically.
(defvar *registration*
  (cl-sip:register *ua* :expires 3600
                        :on-event (lambda (registration event &key &allow-other-keys)
                                    (print (list registration event)))))

;; Receive calls.
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

(cl-sip:hangup *call*)            ; CANCEL, rejection or BYE as the state requires
(cl-sip:unregister *registration*)
(cl-sip:stop *ua*)
```

Callbacks run on the stack's event-loop thread, so keep them short and do
not block in them; they may call any `cl-sip` function. Calls from other
threads are handed to the loop for you. `cl-sip:wait-until` polls for a
condition from an application thread.

`examples/` has three runnable programs (`(ql:quickload "cl-sip/examples")`):
`register-example`, `start-echo-uas` (answers and echoes the caller's RTP),
and `call-example` (calls a URI, sends a tone and a DTMF digit).

```lisp
(defvar *uas* (cl-sip.examples:start-echo-uas :port 5070))
(cl-sip.examples:call-example "sip:echo@127.0.0.1:5070")
;; => :LOCAL-HANGUP, 57
```

## Calling a model from a softphone

`cl-sip/voice` is a small phone server. A softphone registers with it (or
just dials it), and the call is answered by a voice agent: the caller's
speech is transcribed locally with whisper.cpp, sent to Claude Haiku 5.5
through the Messages API, and the streamed reply is spoken back sentence by
sentence. Talking over the agent interrupts it.

The tools come from the flake's dev shell (`whisper-cli`, `curl`, the whisper
model as `$WHISPER_MODEL`, and `pjsua` for testing); speech is synthesized
with the macOS `say` command. The server needs `ANTHROPIC_API_KEY` in its
environment.

```sh
bin/voice-server            # check the configuration, then run
bin/voice-server --check    # check only
```

The script loads the dev shell itself, reads `.envrc.local` if present,
reports each setting as ok or as a problem (never printing values), and
starts the server on 127.0.0.1:5060 (UDP and TCP) once everything required
is there. Ctrl-C stops it and removes any trunk registration.

or from a REPL started inside the dev shell:

```lisp
(ql:quickload "cl-sip/voice")
(defvar *server* (cl-sip.voice:start-server))
(cl-sip.voice:stop-server *server*)
```

Then point a softphone on the same Mac at it:

| Setting | Value |
|---|---|
| SIP server / domain | `127.0.0.1` (port 5060) |
| User name | anything, e.g. `100` |
| Password | anything (registration is open unless `:users` is given) |
| Transport | UDP or TCP |
| Encryption (SRTP, TLS) | off |

and dial:

| Extension | What answers |
|---|---|
| `600` | Echo test: you hear yourself |
| `700` | A spoken test message, then it hangs up |
| anything else | The voice agent |

The routing is the `:dialplan` argument (see `*default-dialplan*`): patterns
map to `:agent`, `:echo`, `(:say "text")`, `(:play "file.wav")`,
`(:reject status)` or a function of your own.

Only this machine may register and call by default; requests from anywhere
else are dropped without an answer. To let other devices on your network
in, listen on all interfaces and name the network:

```sh
VOICE_HOST=0.0.0.0 VOICE_ALLOW=192.168.1.0/24 bin/voice-server
```

Each stage is replaceable: `start-server` takes `:stt`, `:llm` and `:tts`
objects. `make-claude-llm` accepts `:model`, `:system` and `:effort`, and
`make-say-tts` accepts `:voice`.

### Connecting a phone number (Flowroute)

With SIP credentials in the environment the server registers with Flowroute,
and calls to your number are answered by the agent:

```sh
export FLOWROUTE_USERNAME=...     # the tech prefix (Interconnections > Registration)
export FLOWROUTE_PASSWORD=...     # the SIP password shown there
export FLOWROUTE_POP=us-west-or   # or us-east-va
bin/voice-server
```

In the Flowroute portal, route the number to "SIP registration" on the same
point of presence. The server then listens on all interfaces, registers,
learns its public address from Flowroute's replies, re-registers with it,
and sends a keep-alive every 25 seconds so the router keeps the path open.
Requests are accepted only from this machine, `VOICE_ALLOW` networks, and
Flowroute's signalling addresses.

Flowroute does not relay audio: it flows directly between the carrier's
gateway and this machine, so the audio port has to be reachable through your
router. Pick one:

| Option | Setting | Notes |
|---|---|---|
| Port forwarding | `VOICE_RTP_PORTS=40000-40019`, and forward that UDP range to this machine | Dependable. Each call uses one port from the range. |
| STUN | `VOICE_STUN=host:port` | No router changes; does not work behind routers that give each destination a different port. |
| Neither | | Works only if the router keeps port numbers unchanged. |

Other settings: `VOICE_ALLOWED_CALLERS=2065550199,...` answers only those
callers (each call spends API tokens), and `VOICE_MAX_CALLS` (default 2)
caps simultaneous agent calls. A trunk call is routed by the number that was
called, so a dialplan entry such as `("12065550100" :agent)` applies to it;
the default dialplan sends everything to the agent.

From Lisp the same thing is the `:trunk` argument:

```lisp
(cl-sip.voice:start-server
 :host "0.0.0.0"
 :trunk '(:registrar "sip:us-west-or.sip.flowroute.com"
          :username "12345678" :password "...")
 :rtp-port-range '(40000 . 40019)
 :allowed-callers '("2065550199"))
```

Placing calls out through the trunk is not implemented yet.

### Extensions

Give the server a list of extensions and it becomes a small PBX: phones
register under their numbers and call each other, with the server relaying
the audio. Put the list in `extensions.lisp` next to the flake (it is
git-ignored, since it holds passwords), or name another file with
`VOICE_EXTENSIONS`:

```lisp
("100" :password "s3cret" :name "Alice")
("101" :password "hunter2" :name "Bob" :ring-seconds 20)
("200" :ring ("100" "101") :name "Front desk")   ; a group: both phones ring
```

Each softphone then uses its number as the user name and its password, with
the server's address as the SIP server. With a list in place only those
extensions can register, each only as itself, and every call a phone places
is authenticated. Dialling an extension rings every phone registered for it
(or for each member of a group); the first to answer gets the call. Numbers
that are not extensions go to the dialplan as before, so the agent still
answers anything else, and trunk callers can be routed to phones by listing
your Flowroute number as a group, e.g. `("12065550100" :ring ("100"))`.

An extension can send its calls elsewhere: `:forward` always, `:busy` when
its phone is busy or declines, `:no-answer` when nobody picks up within
`:ring-seconds` (default `VOICE_RING_SECONDS`, 30) or no phone is registered.
The target is another number or `:agent`:

```lisp
("101" :password "hunter2" :name "Bob" :busy "100" :no-answer :agent)
```

The phone that ends up ringing is told where the call has been
(`History-Info`), and a call forwarded more than five times is taken for a
loop and refused.

Hold and transfer work from the phone's own buttons. A held party's phone
is told it is on hold. A transfer, blind or after consulting the third
party on a second call, is carried out by the server: the parties that
remain are joined and the phone that transferred is released, or, if the
target does not answer, the original call carries on. Calls can be
transferred to the agent like any other number.

The design and what is still to come (dialling out, busy lamps) are in
`docs/pbx-plan.md`.

### Looking things up in the wiki

With a capability token for the LLM-wiki MCP server, the agent can consult
the wiki during a call. Put the token in `.envrc.local`:

```sh
VOICE_WIKI_TOKEN=...                           # must carry the mcp capability
VOICE_WIKI_URL=http://localhost:8788/mcp       # the default
```

`bin/voice-server` then checks that the wiki accepts the token. The server's
tools are offered to the model, apart from `ask_wiki`: it has a local model
write the answer, which takes longer than a caller will wait, so the agent
follows the same routine itself, a turn at a time: whatever the caller says
is searched for (about 0.2 s) and the hits go to the model with their words;
it reads the most promising pages, searches again if they miss, and answers
briefly from them, ready for a follow-up. The wiki's catalog (`index`) rides
along in the system prompt as a map of what exists; it is long, so it is
marked for prompt caching and costs little after a call's first turn. `VOICE_WIKI_EXCLUDE`, a
comma-separated list, changes which tools are withheld. The model says a few words ("Let me check"),
calls a tool, and answers from the result; each call appears in the log as a
`tool:` line. If the wiki is down or refuses the token, the agent carries on
without it and the log says why; a wiki that does not answer is left alone
for a minute, so it costs a caller one five-second wait rather than one per
turn. Lookups last for one reply: what was said
is remembered for the rest of the call, the pages read are not.

Anyone who reaches the agent can ask it about the wiki, so set
`VOICE_ALLOWED_CALLERS` when a trunk is connected.

From Lisp, any MCP server reached over HTTP works, as do plain functions:

```lisp
(cl-sip.voice:start-server
 :llm (cl-sip.voice:make-claude-llm
       :tools (list (cl-sip.voice:make-mcp-server
                     :url "http://localhost:8788/mcp" :token "...")
                    (cl-sip.voice:make-function-tool
                     "current_time" "The current time."
                     (lambda (input) (declare (ignore input)) "half past nine")))))
```

### A cloned voice

`make-chatterbox-tts` speaks with the Chatterbox Turbo model, which can
imitate a voice from a short reference recording (about ten seconds of clean
speech). It runs `tools/chatterbox_tts.py` under `uv` as a worker that keeps
the model loaded; the first start downloads the model (4 GB) unless it is
already in the Hugging Face cache.

```sh
VOICE_TTS=chatterbox VOICE_REFERENCE=me.wav bin/voice-server
```

```lisp
(cl-sip.voice:start-server
 :tts (cl-sip.voice:make-chatterbox-tts :voice "me.wav"))
```

On an Apple Silicon laptop it takes about as long to synthesize a sentence
as to speak it, so replies start two to three seconds later than with `say`,
and loading takes around half a minute.

Other models plug in the same way: `make-worker-tts` runs any long-lived
program that speaks the worker's line protocol (see the script's header),
and `make-command-tts` runs a program once per sentence:

```lisp
(cl-sip.voice:make-command-tts '("my-tts" "--text" :text "--out" :output))
```

## Layout

| Path | Contents |
|---|---|
| `src/uri.lisp`, `headers.lisp`, `message.lisp`, `parser.lisp`, `serializer.lisp` | Message codec |
| `src/auth.lisp` | Digest authentication, client and server side |
| `src/scheduler.lisp` | Event loop with a real or virtual clock |
| `src/transport*.lisp` | Transport layer; in-memory, UDP and TCP transports |
| `src/transaction.lisp` | Client and server transaction state machines |
| `src/dialog.lisp`, `ua.lisp`, `registration.lisp`, `call.lisp` | Dialogs and the UA core |
| `src/sdp/` | Package `cl-sip.sdp`: SDP and offer/answer |
| `src/rtp/` | Package `cl-sip.rtp`: RTP packets, G.711, DTMF, a simple session |
| `src/voice/` | Package `cl-sip.voice`: audio helpers, voice agent, tools (MCP client), phone server |
| `t/` | Tests; protocol tests run on a virtual clock over an in-memory network |

## Testing without a network

The protocol tests never sleep: agents share a virtual scheduler and an
in-memory network, so a test can lose or duplicate chosen messages and step
through 32 seconds of retransmission timers instantly. `t/harness.lisp`
shows how; the same pieces (`make-scheduler :virtual t`,
`make-memory-network`, `make-memory-transport`, `pump`, `advance-time`) are
exported for use at the REPL.
