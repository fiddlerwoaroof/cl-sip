{
  description = "cl-sip: a SIP stack and voice agent in Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    # The development shell, on every platform.
    dev = flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      # Speech-to-text model for whisper.cpp, used by the voice agent.
      whisperModel = pkgs.fetchurl {
        url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin";
        hash = "sha256-oDd5yG3zMjB19eeWyyzlAp8A7Ihp7uP9+4l6/jbG0AI=";
      };
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.curl # Claude API requests from the voice agent
          pkgs.whisper-cpp # whisper-cli / whisper-server: local speech-to-text
          pkgs.pjsip # pjsua: a command-line softphone for interop tests
        ];
        WHISPER_MODEL = "${whisperModel}";
      };
    });
  in
    nixpkgs.lib.recursiveUpdate dev {};
}
