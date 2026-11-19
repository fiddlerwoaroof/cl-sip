# cl-sip implementation plan

A basic SIP user-agent stack in Common Lisp, built from the RFCs in
`docs/rfcs/`. This document is the plan of record: scope, architecture, the
phases in build order, and how each phase is verified.

## 1. Scope

### In scope

| Area | Source | Notes |
|---|---|---|
| Message syntax: parse and serialize | RFC 3261 §7, §25 | Requests, responses, compact forms, line folding, multi-value headers |
| SIP/SIPS URIs | RFC 3261 §19.1 | Parse, serialize, escaping, comparison (§19.1.4) |
| Transport: UDP and TCP | RFC 3261 §18, RFC 3581 | `received`/`rport`, response routing, stream framing |
| Transactions (all four machines) | RFC 3261 §17, RFC 6026, RFC 4320 | With the Accepted state and timers L/M |
| Dialogs | RFC 3261 §12 | Route sets, loose and strict routing, CSeq tracking |
| UA core: REGISTER, INVITE, ACK, BYE, CANCEL, OPTIONS | RFC 3261 §8–§15, RFC 5407 | UAC and UAS roles, re-INVITE, glare (491) |
| Digest authentication, client and server | RFC 3261 §22, RFC 7616, RFC 8760 | MD5, SHA-256, `-sess` variants, `auth` and `auth-int` |
| SDP | RFC 8866 | Parse, serialize, attribute helpers |
| Offer/answer | RFC 3264, RFC 4317 | Audio streams, direction, hold, rejected streams |
| RTP basics | RFC 3550, RFC 3551, RFC 4733 | Packet codec, G.711 µ-law/A-law, telephone-event, simple UDP session |
| In-memory registrar | RFC 3261 §10.3 | Enough to exercise registration flows and act as a test peer |

### Out of scope (for now)

- Proxy behaviour (RFC 3261 §16): stateful or stateless forwarding, forking. A
  UAC here does cope with *receiving* forked 2xx responses.
- RFC 3263 server location (NAPTR/SRV). Destinations resolve by literal IP or
  an A/AAAA lookup, port 5060 unless stated, transport from the `transport`
  URI parameter (default UDP).
- TLS/SIPS transport, S/MIME, multicast, SCTP.
- Extensions beyond the base spec: PRACK/100rel, UPDATE, SUBSCRIBE/NOTIFY,
  REFER, session timers, outbound, ICE. Unknown methods get 405, unknown
  `Require` tags get 420, per the base spec.
- RTCP, jitter buffering, audio device I/O, codecs other than G.711.
- RFC 2543 backwards compatibility beyond matching transactions whose branch
  lacks the magic cookie.

## 2. Dependencies and tooling

- SBCL (developed on 2.6.8); code stays portable CL plus the libraries below.
- `usocket` (UDP/TCP sockets), `bordeaux-threads`, `ironclad` (digests),
  `babel` (UTF-8), `alexandria`.
- Tests: `fiveam`. Run with `(asdf:test-system "cl-sip")`.

## 3. Architecture

Layers follow RFC 3261 §5. Each layer only calls the one below it and is
called back by it.

```
   application
       │  calls: register / invite / answer / hangup …   callbacks: events
   ┌───▼───────────────────────────────────────────────┐
   │ UA core (ua.lisp, call.lisp, registration.lisp)   │  TU, §8–§15
   ├───────────────────────────────────────────────────┤
   │ dialog (dialog.lisp)                              │  §12
   ├───────────────────────────────────────────────────┤
   │ transaction (transaction.lisp)                    │  §17, RFC 6026
   ├───────────────────────────────────────────────────┤
   │ transport (transport.lisp, udp.lisp, tcp.lisp)    │  §18, RFC 3581
   ├───────────────────────────────────────────────────┤
   │ message / uri / headers (codec, no I/O)           │  §7, §19, §20, §25
   └───────────────────────────────────────────────────┘
   alongside: auth.lisp (§22)   sdp.lisp + offer-answer.lisp   rtp/
```

### 3.1 Concurrency model: one event loop per stack

All protocol state (transactions, dialogs, calls) is owned by a single
**stack** and is only touched from that stack's event loop. Three kinds of
event enter the loop through a thread-safe queue:

1. a message received by a transport,
2. a timer firing,
3. a closure posted by application code (every public API call is one).

Consequences:

- No locks inside the protocol code; state machines are plain sequential code.
- Application callbacks run on the loop thread and may call back into the API
  freely. Public API calls made from *other* threads are posted to the loop
  and wait for the result; `wait-until` polls for a state from such threads.
- **Tests run the loop by hand.** With a virtual clock and an in-memory
  transport, a test steps events and advances time explicitly, so every timer
  in §17 can be exercised deterministically and instantly.

The stack therefore has two drivers:

- `run-stack` / `start-stack`: a real thread, wall-clock timers, sockets.
- `pump` + `advance-time`: manual, used by the test suite.

### 3.2 Packages

| Package | Contents |
|---|---|
| `cl-sip` | URI, message, headers, auth, transport, transaction, dialog, UA |
| `cl-sip.sdp` | SDP codec and offer/answer |
| `cl-sip.rtp` | RTP packet codec, G.711, telephone-event, RTP session |
| `cl-sip.test` | Test suite |

### 3.3 Source layout

```
cl-sip.asd
src/
  package.lisp
  util.lisp            scanner, char classes, tokens, random ids
  uri.lisp             sip-uri, parse/serialize/compare
  headers.lisp         name-addr, via, cseq, params; header name table
  message.lisp         request/response classes, typed accessors, builders
  parser.lisp          octets -> message, stream framing
  serializer.lisp      message -> octets
  auth.lisp            digest challenge/credentials, client + server side
  scheduler.lisp       clock, timers, event queue
  transport.lisp       stack object, transport protocol, resolution, via handling
  transport-memory.lisp  in-process transport for tests
  transport-udp.lisp
  transport-tcp.lisp
  transaction.lisp     four state machines + matching
  dialog.lisp
  ua.lisp              UA core: request dispatch, UAS checks, OPTIONS
  registration.lisp    REGISTER client + in-memory registrar
  call.lisp            INVITE sessions: UAC, UAS, CANCEL, BYE, re-INVITE
  sdp/sdp.lisp
  sdp/offer-answer.lisp
  rtp/packet.lisp
  rtp/g711.lisp
  rtp/dtmf.lisp
  rtp/session.lisp
t/
  package.lisp, harness.lisp (virtual clock, wired stack pairs)
  uri.lisp, message.lisp (incl. torture tests), auth.lisp, transaction.lisp,
  dialog.lisp, ua.lisp, sdp.lisp, rtp.lisp, sockets.lisp (UDP, TCP, RTP)
examples/
  register.lisp, echo-uas.lisp, call.lisp
```

## 4. Core data model

### 4.1 URIs (`uri.lisp`)

```lisp
(defstruct sip-uri scheme user password host port params headers)
;; scheme: :sip | :sips       host: string (IPv6 kept bracketed)
;; params / headers: alists of (name . value-or-nil), names downcased
(defstruct generic-uri scheme rest)   ; tel:, http:, anything else
```

- `parse-uri`, `uri-string`, `uri-param`, `uri-equal` (§19.1.4: user
  case-sensitive, host case-insensitive, `user`/`ttl`/`method`/`maddr`/
  `transport` must agree when present in either, other params only compared
  when present in both, headers must all match).
- Escaping per §19.1.2; `%xx` is decoded in user, password, params and
  headers, and re-encoded on output using the per-component allowed sets.

### 4.2 Messages (`message.lisp`, `headers.lisp`)

```lisp
(defclass message () (headers body))          ; body: octet vector or nil
(defclass request  (message) (method uri))    ; method: string, e.g. "INVITE"
(defclass response (message) (status reason))
```

- `headers` is an ordered list of `(name . value)` conses. `name` is the
  canonical long form (`"Via"`, `"Call-ID"`); compact forms (§7.3.3) are
  expanded when parsing. `value` is the raw string.
- Headers defined as comma-separated lists (Via, Contact, Route,
  Record-Route, Allow, Supported, Require, …) are split into one entry per
  value at parse time, respecting quotes and angle brackets, so "the top Via"
  is simply the first `Via` entry. Authentication headers and Date are never
  split.
- Typed views are parsed on demand and are not cached in the message:
  `(msg-via m)` → list of `via`, `(msg-from m)` / `(msg-to m)` → `name-addr`,
  `(msg-cseq m)` → `cseq`, `(msg-contacts m)`, `(msg-routes m)`,
  `(msg-record-routes m)`, `(msg-call-id m)`, `(msg-expires m)`, …

```lisp
(defstruct name-addr display-name uri params)   ; From/To/Contact/Route…
(defstruct via protocol transport host port params)
(defstruct cseq number method)
```

- Editing: `(header m name)`, `(headers m name)`, `(setf header)`,
  `add-header`, `prepend-header`, `remove-header`.
- `make-response-for request status` implements §8.2.6.2: copy From,
  Call-ID, CSeq, all Via, To (adding a tag when asked), in order.

### 4.3 Parsing (`parser.lisp`)

1. Skip leading CRLFs (§7.5). Find the end of the header block.
2. Decode the head as UTF-8 (falling back to Latin-1 on invalid sequences),
   unfold continuation lines, split start line and headers.
3. Start line: `Method SP Request-URI SP SIP/2.0` or
   `SIP/2.0 SP 3DIGIT SP reason`. Exactly one SP between elements.
4. Body length: `Content-Length` if present, otherwise the rest of the
   datagram (UDP) — on a stream it is required. Extra octets after the body
   in a datagram are dropped; a declared length longer than the data is an
   error.
5. Validation is two-level. `parse-message` raises `sip-parse-error` only
   when the message cannot be framed or the start line is unusable.
   `validate-request` then checks the mandatory headers (To, From, CSeq,
   Call-ID, Via — §8.1.1), that CSeq method equals the request method, and
   numeric ranges, returning the status to answer with (400 etc.) so the UAS
   can still build a response from a damaged request.
6. `read-message-from-stream` frames messages on TCP using Content-Length
   (§18.3) and tolerates CRLF keep-alives.

## 5. Phases

Each phase ends with its tests passing. Later phases depend on earlier ones
in the order given.

### Phase 1 — Skeleton, utilities, URIs
- ASDF system, packages, test harness.
- Scanner utilities: character classes for `token`, `word`, `unreserved`…;
  quoted-string read/write; parameter-list parsing (`;name=value`, value may
  be token, host or quoted-string); quote-aware comma splitting.
- ID generation: branch (`z9hG4bK` + random), tags, Call-IDs, cnonces.
- `sip-uri` parse/serialize/compare.
- **Tests:** the example URIs and the equivalence/non-equivalence tables of
  §19.1.3–§19.1.4; escaping round trips; IPv6 references.

### Phase 2 — Messages
- Message classes, header table (canonical names, compact forms, which are
  list-valued), typed header parsers and printers, parser, serializer
  (Content-Length always computed), response builder.
- **Tests:** round-trip every message of RFC 3665 §2.1 and §3.1; a
  transcribed subset of RFC 4475 — valid: §3.1.1.1 (tortuous INVITE),
  §3.1.1.6, §3.1.1.8 (trailing octets), §3.1.1.12/13 (reason phrases);
  invalid: §3.1.2.2/3 (Content-Length), §3.1.2.7–10 (request line),
  §3.1.2.16–19, §3.3.1 (missing headers).

### Phase 3 — Digest authentication
- Parse challenges and credentials (`WWW-Authenticate`,
  `Proxy-Authenticate`, `Authorization`, `Proxy-Authorization`,
  `Authentication-Info`).
- Response computation for MD5, MD5-sess, SHA-256, SHA-256-sess,
  qop `auth`, `auth-int`, and none
  (RFC 2069 form).
- Client: a credential store keyed by realm; when several
  challenges are offered, use the first one whose algorithm is supported
  (the server lists them in preference order, RFC 8760 §2.4); nonce-count
  tracking per nonce; `stale=true` retries without re-prompting.
- Server: `make-challenge`, `verify-credentials` (used by the registrar and
  available to UAS applications).
- **Tests:** RFC 7616 §3.9.1 vectors (MD5 and SHA-256), the RFC 2617
  "Mufasa" vector for the no-surprises MD5 path, client→server self-check
  for every algorithm × qop.

### Phase 4 — Scheduler and transports
- `scheduler`: pluggable clock, timer set (`schedule`, `cancel-timer`),
  event queue, `pump`, `advance-time`.
- Transport protocol (generic functions): `transport-send`,
  `transport-protocol` (`"UDP"`/`"TCP"`), `transport-reliable-p`,
  `transport-local-address`, `transport-start/stop`.
- Sending requests (§18.1.1): resolve the destination from the first Route
  (if loose) or the Request-URI; honour `maddr`, `transport`, port. The UA
  fills the Via sent-by from the chosen transport and always adds `rport`
  (RFC 3581 §3).
- Receiving requests (§18.2.1): add `received` when the sent-by host is not
  the source address; fill `rport` when present (RFC 3581 §4).
- Sending responses (§18.2.2): same connection for TCP; else `maddr`; else
  `received` + `rport`; else sent-by.
- Receiving responses (§18.1.2): drop when the top Via is not ours.
- `memory-transport`: pairs of stacks wired through a shared "network"
  object with hooks to drop, duplicate, or capture messages.
- `udp-transport`: one socket, one reader thread posting to the queue.
- `tcp-transport`: listener thread, connection table keyed by remote
  address (§18), reader thread per connection, connection reuse for
  responses.
- **Tests:** Via `received`/`rport` handling, response destination choice,
  UDP loopback smoke test, TCP framing with several messages in one segment
  and one message split across segments.

### Phase 5 — Transactions
Four state machines, each a class with `state`, the original request, the
last response/request sent, and its timers. T1 = 500 ms, T2 = 4 s, T4 = 5 s,
all configurable on the stack.

**INVITE client** (§17.1.1 + RFC 6026 §7.2)

| State | Event | Action | Next |
|---|---|---|---|
| Calling | enter | send; A = T1 (unreliable); B = 64·T1 | |
| Calling | timer A | resend; A doubles | Calling |
| Calling | timer B | TU ← timeout | Terminated |
| Calling/Proceeding | 1xx | TU ← response | Proceeding |
| Calling/Proceeding | 2xx | TU ← response; M = 64·T1 | Accepted |
| Calling/Proceeding | 300–699 | TU ← response; send ACK; D = 32 s (0 if reliable) | Completed |
| Accepted | 2xx | TU ← response | Accepted |
| Accepted | timer M | | Terminated |
| Completed | 300–699 | resend ACK | Completed |
| Completed | timer D | | Terminated |
| any | transport error | TU ← error | Terminated |

**Non-INVITE client** (§17.1.2): Trying → Proceeding → Completed →
Terminated with timers E (T1 doubling to T2; T2 in Proceeding), F (64·T1),
K (T4, 0 if reliable).

**INVITE server** (§17.2.1 + RFC 6026 §7.1)

| State | Event | Action | Next |
|---|---|---|---|
| Proceeding | enter | TU ← request; send 100 if the TU has not responded within 200 ms | |
| Proceeding | INVITE retransmit | resend last provisional | Proceeding |
| Proceeding | TU 1xx | send | Proceeding |
| Proceeding | TU 2xx | send; L = 64·T1 | Accepted |
| Proceeding | TU 300–699 | send; G = T1 (unreliable); H = 64·T1 | Completed |
| Accepted | INVITE retransmit | absorb | Accepted |
| Accepted | TU 2xx | send | Accepted |
| Accepted | ACK | TU ← ACK | Accepted |
| Accepted | timer L | | Terminated |
| Completed | INVITE retransmit | resend response | Completed |
| Completed | timer G | resend; G = min(2G, T2) | Completed |
| Completed | ACK | I = T4 (0 if reliable) | Confirmed |
| Completed | timer H | TU ← failure | Terminated |
| Confirmed | timer I | | Terminated |

**Non-INVITE server** (§17.2.2): Trying → Proceeding → Completed →
Terminated, timer J (64·T1, 0 if reliable). Per RFC 4320 §4.1 the machine
never emits a 100 on its own.

Matching:
- Client (§17.1.3): key = (top Via branch, CSeq method).
- Server (§17.2.3): key = (branch, sent-by, method) with ACK mapped to
  INVITE. A branch without the `z9hG4bK` cookie falls back to a key built
  from Request-URI, From tag, Call-ID, CSeq number, and top Via.
- ACK for a non-2xx is built by the transaction (§17.1.1.3): same
  Request-URI, Call-ID, From, top Via, Route set and CSeq number as the
  INVITE; To from the response.
- Unmatched responses are dropped unless they are 2xx to INVITE handed to
  the UA core for forked-dialog handling (RFC 6026 §6 forbids forwarding
  strays; a UA still has to ACK them).

TU interface: a transaction holds a `tu` function called with
`(:response r)`, `(:timeout)`, `(:transport-error c)`, `(:request r)`,
`(:ack r)`, `(:terminated)`.

- **Tests** (virtual clock + memory transport): retransmission schedules
  for A, E and G against the tables above; B/F/H timeouts; ACK generation
  and re-ACK on retransmitted final response; absorption of request
  retransmits; reliable transport sets zero-length timers and no
  retransmits; Accepted-state behaviour for both INVITE machines.

### Phase 6 — Dialogs
- State (§12): Call-ID, local tag, remote tag, local/remote CSeq, local
  URI, remote URI, remote target, route set, secure flag, state
  (`:early`, `:confirmed`, `:terminated`).
- Creation as UAC from a 1xx/2xx with To tag (§12.1.2: route set =
  Record-Route reversed) and as UAS when sending one (§12.1.1: route set =
  Record-Route in order; Record-Route copied into the response).
- In-dialog request construction (§12.2.1.1): loose routing (Request-URI =
  remote target, Route = route set) versus strict routing (Request-URI =
  first route, remote target appended to Route).
- Target refresh from Contact on re-INVITE requests and their 2xx
  (§12.2.1.2, §12.2.2). Remote CSeq check: lower → 500 (§12.2.2).
- Dialog table on the stack keyed by (Call-ID, local tag, remote tag).
- **Tests:** RFC 3261 §16.12.1 route examples (loose and strict), dialog
  IDs from the RFC 3665 §3.2 flow, CSeq rules.

### Phase 7 — UA core: general behaviour, REGISTER, OPTIONS
- `user-agent`: address-of-record, contact, display name, credentials,
  stack, callbacks, Allow/Supported/Accept lists.
- Out-of-dialog request construction (§8.1.1): Request-URI, To, From with
  tag, Call-ID, CSeq, Max-Forwards 70, Via with fresh branch, Contact,
  User-Agent; preloaded Route when an outbound proxy is configured.
- Response processing (§8.1.3): more than one Via → discard; unknown final
  status treated as x00; 401/407 → retry once per challenge with
  credentials, new branch, CSeq + 1, same Call-ID and From tag (§22.2);
  3xx → reported to the application, which may call the new target;
  408/timeout/503 mapping (§8.1.3.1, RFC 4320 §4.2).
- UAS request processing (§8.2) in order: method (405 + Allow) → Require
  (420 + Unsupported) → merged request (482) → dialog lookup (481 for
  unknown in-dialog requests) → method handler. Stray CANCEL → 481.
- REGISTER client (§10.2): To = From = AOR, Request-URI = registrar domain
  without user, Contact with `expires`; granted lifetime read from the
  returned binding; 423 → retry with Min-Expires; automatic refresh at
  half the lifetime; unregister (`expires=0`, or `Contact: *`); fetch
  bindings.
- Registrar (§10.3): in-memory bindings table, optional digest
  authentication, Call-ID/CSeq ordering rule, `Contact: *` rules, minimum
  expiry (423), returns all current bindings.
- OPTIONS (§11): client call; server answers 200 with Allow, Accept,
  Supported, and (optionally) an SDP capability body.
- **Tests:** RFC 3665 §2.1 (register with 401 challenge), §2.2 (update),
  §2.3 (fetch), §2.4 (cancel), §2.5 (wrong password → second 401 reported
  as failure); 423 retry; OPTIONS round trip; 405/420/481 paths.

### Phase 8 — UA core: sessions
`call` object: direction, state (`:calling`, `:ringing`/`:early`,
`:incoming`, `:established`, `:terminating`, `:terminated`), dialog, local
and remote SDP, termination cause.

- UAC (§13.2): `invite` sends an offer; 1xx with a tag creates the early
  dialog; 2xx confirms it, and the UA core sends the ACK itself
  (§13.2.2.4) — CSeq number of the INVITE, new branch, credentials copied,
  retransmitted for every retransmitted 2xx. A 2xx from a second fork gets
  ACK followed by BYE. 3xx–6xx → terminated with the status as cause.
- UAS (§13.3): `:incoming` event; `ring` (180), `answer` (200 with SDP
  answer, Contact, Allow, Supported), `reject` (default 486). The UA core
  retransmits the 2xx at T1 doubling to T2 until the ACK arrives and gives
  up after 64·T1 by sending BYE (§13.3.1.4).
- CANCEL (§9): UAC sends CANCEL only once a provisional response has
  arrived (queued until then), same Request-URI/Call-ID/To/From/CSeq
  number/top Via as the INVITE; if a 2xx arrives anyway the call is
  established and then released with BYE (RFC 5407 §3.1.2). UAS answers
  the CANCEL 200 and the INVITE 487 unless it has already sent a final
  response.
- BYE (§15): `hangup` picks the right mechanism from the state — CANCEL,
  reject, or BYE. An incoming BYE is answered 200 and any pending INVITE
  server transaction in the dialog gets 487.
- re-INVITE (§14): `reinvite` with a new offer (hold/resume helpers);
  incoming re-INVITE is passed to the application or auto-answered with
  the current session; 491 on glare with the randomised retry of §14.1;
  a failed re-INVITE leaves the session as it was; 481/408 on a re-INVITE
  ends the dialog.
- **Tests** (two UAs on the memory network): RFC 3665 §3.1 exact message
  sequence (INVITE/180/200/ACK/BYE/200); §3.8 (no answer → CANCEL/487/ACK);
  §3.9 (486 + ACK); INVITE with 407 then success (§3.3 shape); lost 200 →
  retransmission → single ACK per 2xx; lost ACK → UAS gives up with BYE;
  CANCEL/200 race; re-INVITE hold and glare; then the §3.1 flow again over
  real UDP on localhost.

### Phase 9 — SDP and offer/answer
- `session` and `media` structs covering every line type in RFC 8866 §5
  (`v o s i u e p c b t r z k a m`), tolerant of bare-LF line endings.
- Attribute helpers: `rtpmap`, `fmtp`, `ptime`, direction
  (`sendrecv`/`sendonly`/`recvonly`/`inactive`), lookup with session-level
  fallback.
- Offer/answer (RFC 3264): `make-offer` from a local capability list;
  `make-answer` (§6): one answer `m=` per offered `m=` in order, unsupported
  or unmatched streams rejected with port 0, codec intersection in the
  offerer's order, dynamic payload types matched by encoding name/clock
  rate/channels and answered with the offerer's numbers, direction reversal
  table, `telephone-event` carried over when offered; `o=` version bump on
  each new offer (§8); hold (`sendonly`/`inactive`).
- Integration: `call` uses these by default when the application supplies
  capabilities instead of raw SDP bodies.
- **Tests:** RFC 4317 §2.1 (audio+video, one codec accepted), §2.2 (video
  rejected), §2.4 (telephone-event), §3.1 (hold and resume); parse and
  re-serialize the SDP bodies of RFC 3665.

### Phase 10 — RTP
- Packet codec (RFC 3550 §5.1): version, padding, extension, CSRC list,
  marker, payload type, sequence, timestamp, SSRC.
- Static payload table (RFC 3551 §6) for the audio types; G.711 µ-law and
  A-law encode/decode of 16-bit linear samples.
- telephone-event payload (RFC 4733 §2.3): encode/decode, and generation of
  the packet train for one digit (marker on first, three end packets,
  constant timestamp, growing duration).
- `rtp-session`: UDP socket, send with sequence/timestamp bookkeeping,
  receive thread with callback, SSRC and sequence tracking (§A.1 simplified).
- **Tests:** header vectors built by hand, G.711 known sample pairs and
  round-trip error bounds, DTMF packet train shape, localhost send/receive.

### Phase 11 — Examples and README
- `examples/register.lisp`, `examples/echo-uas.lisp` (answers and echoes
  RTP), `examples/call.lisp`; README with a quick start and the conformance
  notes of §8 below.

## 6. Public API sketch

```lisp
(defvar *ua*
  (cl-sip:make-user-agent :aor "sip:alice@example.com"
                          :transports '((:udp :host "0.0.0.0" :port 5060))
                          :credentials '(("example.com" "alice" "secret"))))
(cl-sip:start *ua*)

(cl-sip:register *ua* :expires 3600
                 :on-event (lambda (reg event &key) ...))  ; :registered :unregistered :bindings :failed

(setf (cl-sip:ua-on-incoming-call *ua*)
      (lambda (call) (cl-sip:ring call) (cl-sip:answer call :sdp answer)))

(let ((call (cl-sip:invite *ua* "sip:bob@example.com" :sdp offer
                           :on-event (lambda (call event &key) ...))))
  ;; events: :progress :ringing :established :reinvite :terminated
  (cl-sip:hangup call))

(cl-sip:options *ua* "sip:bob@example.com" :on-response #'print)
(cl-sip:stop *ua*)
```

## 7. Testing strategy

- Unit tests per file, named after the RFC section they check.
- Protocol tests run on the manual driver: virtual clock, memory network,
  message capture — so a test can assert both the *sequence* of messages on
  the wire and the *times* of retransmissions, and can drop or duplicate
  specific messages.
- A small number of real-socket tests (UDP and TCP on 127.0.0.1) cover the
  threaded driver.
- RFC 3665 flows are the acceptance tests for the UA; RFC 4475 for the
  parser; RFC 7616 for digest; RFC 4317 for offer/answer.

## 8. Decisions worth knowing about

1. **Raw header strings with on-demand typed views**, rather than fully
   typed headers. Unknown and malformed headers survive a round trip
   untouched, and a bad optional header cannot make a whole message
   unparseable.
2. **Single-threaded protocol core.** Simplicity and deterministic tests
   outweigh parallelism; a SIP UA is not CPU-bound.
3. **RFC 6026 is built in from the start** rather than layered on: the
   INVITE machines have the Accepted state, and 2xx retransmission/ACK
   remain UA-core duties.
4. **Methods are strings**, compared case-sensitively as the grammar
   requires; header and parameter names are case-insensitive.
5. **No proxy.** The transaction and transport layers are written without
   UA assumptions so a proxy core could be added on top later.

## 9. Status

All eleven phases are implemented and covered by the test suite
(`(asdf:test-system "cl-sip")`, 662 checks). Where the result differs from
the plan above:

- **Digest algorithms:** MD5 and SHA-256 (plain and `-sess`). SHA-512-256
  from RFC 7616 is not offered because the installed ironclad has no
  SHA-512/256 digest; a challenge using it is skipped in favour of the next
  one the server lists.
- **Message size:** the rule of RFC 3261 §18.1.1 that a request within 200
  bytes of the path MTU must switch to a congestion-controlled transport is
  not implemented. A request goes out on the transport its URI selects.
- **Redirects:** a 3xx ends the call with cause `:rejected` and the response
  attached; following the Contact is left to the application.
- **Offer/answer:** an answer accepts the first usable audio stream and
  rejects every other stream, including further audio streams.
- **Early media and forking:** an early dialog is tracked per call, and a
  2xx from a second fork is acknowledged and released with BYE, but several
  simultaneous early dialogs are not kept side by side.
- **RFC 4475:** the parser tests transcribe a subset of the torture messages
  (listed in `t/message.lisp`); the full set is not included.
- **RTP:** `rtp-send-dtmf` sends the packets of a digit as one burst rather
  than pacing them 20 ms apart.

## 10. Voice agent (added after the plan)

`cl-sip/voice` puts a language model on the end of a call. It was not part
of the original plan; the design in brief:

```
softphone ──SIP──▶ phone server (registrar + dialplan) ──▶ voice agent
   ▲                                                          │
   └────────────── RTP (G.711) ◀── paced audio out ◀── TTS ◀──┤
                         │                                    │
                         └─▶ decode ─▶ VAD ─▶ STT ─▶ Claude ──┘
```

- **Server** (`server.lisp`): a UA with a registrar; each INVITE is routed
  by the user part of its Request-URI through a dialplan to the agent, an
  echo, or an announcement. RTP is symmetric (it sends to wherever the
  caller's packets come from).
- **Turn taking** (`vad.lisp`, `agent.lisp`): an energy gate cuts the
  caller's audio into utterances (start after 60 ms of speech, end after
  700 ms of quiet). A worker thread handles one turn at a time. Speech that
  starts while the agent is talking bumps a generation counter, which drops
  queued audio and abandons the reply in progress.
- **Backends** (`backends.lisp`): three generic functions, `transcribe`,
  `respond`, `synthesize`. The defaults are whisper.cpp (`whisper-cli`),
  Claude Haiku 5.5 over the Messages API, and macOS `say`.
- **Claude** is called through `curl` with `stream: true`; text deltas are
  cut into sentences and each is synthesized and queued as soon as it is
  complete, so speech starts with the first sentence. Thinking is off and
  effort is `low` to keep the first word quick. The API key is passed to
  curl on standard input, not on the command line. There is no realtime
  speech API for Claude, hence the pipeline.
- **Not done yet:** calls between two registered phones (needs a
  back-to-back UA with an RTP relay), DTMF input, a voice-cloning TTS
  backend (the `command-tts` hook is there for one), and conversation
  length limits.
