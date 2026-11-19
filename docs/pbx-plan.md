# PBX plan: extensions, bridged calls and call features

The phone server (`cl-sip/voice`, see section 10 of
`implementation-plan.md`) answers every call itself. This plan turns it into
a small PBX: a list of extensions that register, call each other, and use the
usual features, with the voice agent as one more destination.

The RFCs are in `docs/rfcs/`. Each phase names the ones it works from.

## 1. Shape

The server is a **back-to-back user agent that relays media** (RFC 7092: a
"media relay" B2BUA in the signalling/media plane). For a call from A to B
it is the UAS of A's call and the UAC of a separate call to B. Each leg has
its own dialog, its own offer/answer exchange with the server, and its own
RTP session on the server; packets are forwarded between the two sessions.

Why not a proxy: the server already terminates media for the agent, trunk
calls need NAT handling the phones cannot do, and every feature below
(forwarding to the agent, transfer, ringback, codec differences) is simpler
when the server owns both legs. The cost is that media always flows through
the server, which is fine at this scale.

```
 phone A ──INVITE──▶ ┌──────────── server ────────────┐ ──INVITE──▶ phone B
         ◀──180/200─ │ leg A (UAS)  bridge  leg B (UAC)│ ◀─180/200──
         ══ RTP ════▶│ session A ══ forward ══ session B│◀════ RTP ══
                     └─────────────────────────────────┘
```

## 2. The extension list

No RFC defines this; it is local policy. One file, read as data (never
evaluated), one form per extension:

```lisp
("100" :password "s3cret" :name "Alice")
("101" :password "hunter2" :name "Bob" :no-answer "100" :ring-seconds 20)
("102" :password "pw" :name "Carol" :busy :agent :forward nil)
("200" :ring ("100" "101") :name "Front desk")      ; a ring group
```

- A phone registers as its extension number with its password (digest,
  RFC 7616). With a list in place, only listed extensions may register, and
  only as themselves.
- `:forward` (always), `:busy` and `:no-answer` name where a call goes
  instead: another extension, `:agent`, or nothing.
- `:ring` makes a group: the listed extensions ring together and the first
  to answer takes the call.
- `:outbound t` lets the extension dial out through the trunk (phase 6).

`bin/voice-server` reads the file named by `VOICE_EXTENSIONS` (default
`extensions.lisp` if present; git-ignored, since it holds passwords). From
Lisp the same list is the `:extensions` argument of `start-server`. Without
a list the server behaves as before: open registration, every call answered
by the dialplan.

Routing order for a dialled number: the dialplan's explicit entries (600,
700, ...), then extensions and groups, then the outbound pattern if allowed,
then the dialplan default (the agent).

## 3. Phases

Each phase ends with tests passing and a commit. Everything is verified by
simulation: signalling in the virtual-time world of `t/harness.lisp`, audio
over real sockets on loopback with stand-in phones and a stand-in provider,
as the voice tests do. No phase needs a real softphone or the real trunk.

### Phase 1 — Directory and authentication (RFC 3261 §10, §22; RFC 3966)

- `extensions` table on the server; registrar restricted to listed users and
  to "To user = authenticated user" (a phone cannot register someone else's
  number).
- Digest challenge for INVITE from a phone when extensions have passwords
  (401, same realm as the registrar), so calls are tied to an extension.
  Loopback stays trusted when no list is configured.
- Dialled-number normalisation: `tel:` URIs and `user=phone` SIP URIs, visual
  separators and a leading `+` removed before matching (RFC 3966 §5.1).
- Caller identity on bridged legs: From display name and user are the
  calling extension's, never what the phone claims.

### Phase 2 — Bridged calls (RFC 7092, 6337, 3960, 3326)

- `rtp-forward`: resend a received packet from another session, keeping
  marker and relative sequence/timestamp, rewriting SSRC and payload type.
- Bridge: place the B leg with the server's own offer (G.711 and telephone
  events); answer A with the server's answer once B answers. PCMU/PCMA
  mismatch between legs is transcoded; telephone-event payload types are
  mapped.
- Forking: every registered contact of B (and every member of a group) is
  called at once. First 2xx wins; the others are cancelled with
  `Reason: SIP;cause=200;text="Call completed elsewhere"` (RFC 3326).
- Ringback (RFC 3960): A gets `180 Ringing` when any B rings and makes its
  own ringback. If a B leg sends early media (183 with SDP, as a trunk
  does), A gets 183 with the server's SDP and the media is relayed.
- Outcomes: not registered → 480; busy → 486; declined → 603; no answer
  within the ring time → 480 (or the `:no-answer` target); A hangs up →
  CANCEL/BYE to B and vice versa.
- Limits: an extension already in a bridged call is busy unless the phone
  itself says otherwise (the phone decides; the server just forwards the
  INVITE and relays 486).

### Phase 3 — Changes during a call (RFC 6337, 3264 §8, 5359 §2.1, 3311)

- A re-INVITE on either leg is answered by the server from the new offer
  (direction, address, codecs), not by repeating the old description.
- Hold: when a phone offers `sendonly`/`inactive`, the server stops relaying
  audio in both directions and re-INVITEs the other leg with `sendonly`, so
  its phone shows the call as held. Resume reverses it.
- UPDATE (RFC 3311): accepted inside a dialog; without SDP it is a refresh,
  with SDP it is an offer answered in the 200. Listed in `Allow`.

### Phase 4 — Forwarding (RFC 5359 §2.7–2.9, RFC 7044, RFC 3326)

- `:forward`, `:busy`, `:no-answer` per extension, including to `:agent`
  (the server answers A itself and starts the voice agent).
- Each retarget adds `History-Info` entries (RFC 7044) on the new leg, with
  the reason (`SIP;cause=302`, `486`, `408`) on the entry retargeted from;
  loops are cut off after five hops.

### Phase 5 — Transfer (RFC 3515, 3891, 5589; RFC 5359 §2.4, 2.5)

- REFER inside a bridged call. Unattended: 202, then the server calls the
  Refer-To target for the *other* party, reporting progress to the
  transferor with `NOTIFY` (`message/sipfrag`), and joins them on answer;
  the transferor's leg is released. On failure the original call resumes.
- Attended: Refer-To carries `Replaces` naming the transferor's other call
  on this server; the server joins the two far parties and releases both of
  the transferor's legs.
- A transfer target that is not an extension (the agent, an announcement)
  is handled by the server taking the remaining party's call itself.
- Deferred: INVITE with a `Replaces` header (RFC 3891 as a UAS), which call
  pickup needs. Phones transferring through this server use REFER, which the
  server acts on itself, so nothing here depends on it; a `Require:
  replaces` is refused with 420 like any other extension.

### Phase 6 — Dialling out through the trunk

- Dialplan action `:trunk`: a bridge whose B leg is
  `sip:<number>@<registrar>` with the trunk's credentials and the public
  media address.
- Off unless `VOICE_OUTBOUND_CALLER_ID` is set; only extensions with
  `:outbound t` may use it; early media from the carrier is relayed (phase 2).
- A call that came in from the trunk is never sent back out on the caller's
  say-so. Forwarding or transferring to an outside number uses the right of
  the extension that forwards or transfers.
- To check against the real provider: the number format (eleven digits is
  assumed), that credentials rather than the source address authorise
  outbound calls, and what caller ID Flowroute accepts in From.

### Phase 7 — Events (RFC 6665, 4235)

- SUBSCRIBE/NOTIFY core with expiry and refresh.
- `dialog` event package for an extension, so a softphone's busy lamp
  follows it: a full `dialog-info+xml` document on every change, with
  `trying` (dialling), `early` (ringing) and `confirmed`; an idle extension
  has no dialogs. Only bridged calls are shown: a call to the agent or an
  announcement is not, yet.
- Subscribers authenticate as an extension; any extension may watch any
  other.

### Not planned yet

- Message waiting (RFC 3842): needs voicemail, which does not exist. The
  agent taking a message would be the natural source.
- Reliable provisional responses (RFC 3262): neither Flowroute nor common
  softphones require them; a `Require: 100rel` is still refused with 420.
- Music on hold, conferences, call park (RFC 5359 §2.3, 2.10, 2.15).
- Session timers (RFC 4028). UPDATE is implemented so they can follow.

## 4. Status

Updated as phases land.

| Phase | State |
|---|---|
| 1 Directory and authentication | done |
| 2 Bridged calls | done |
| 3 Changes during a call | done |
| 4 Forwarding | done |
| 5 Transfer | done, except INVITE with Replaces |
| 6 Dialling out | done in simulation; not yet tried against Flowroute |
| 7 Events | done |
