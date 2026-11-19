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
`usocket` (and `fiveam` for the tests), all in Quicklisp. Developed on SBCL.

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
| `t/` | Tests; protocol tests run on a virtual clock over an in-memory network |

## Testing without a network

The protocol tests never sleep: agents share a virtual scheduler and an
in-memory network, so a test can lose or duplicate chosen messages and step
through 32 seconds of retransmission timers instantly. `t/harness.lisp`
shows how; the same pieces (`make-scheduler :virtual t`,
`make-memory-network`, `make-memory-transport`, `pump`, `advance-time`) are
exported for use at the REPL.
