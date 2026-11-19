(defpackage :cl-sip.voice
  (:use :cl)
  (:local-nicknames (:sip :cl-sip) (:sdp :cl-sip.sdp) (:rtp :cl-sip.rtp)
                    (:jzon :com.inuoe.jzon))
  (:export
   ;; audio
   #:read-wav #:write-wav #:resample #:rms
   #:audio-out #:make-audio-out #:audio-out-enqueue #:audio-out-flush
   #:audio-out-idle-p #:audio-out-stop
   ;; voice activity detection
   #:vad #:make-vad #:vad-push
   ;; backends
   #:transcribe #:synthesize #:respond #:abort-response
   #:whisper-stt #:make-whisper-stt
   #:say-tts #:make-say-tts #:command-tts #:make-command-tts
   #:claude-llm #:make-claude-llm #:function-llm #:make-function-llm
   #:function-stt #:make-function-stt #:function-tts #:make-function-tts
   #:llm-error #:*phone-system-prompt*
   #:split-sentences
   ;; agent
   #:voice-agent #:start-agent #:stop-agent #:agent-messages
   ;; server
   #:phone-server #:start-server #:stop-server #:server-ua #:server-port
   #:*default-dialplan*))
