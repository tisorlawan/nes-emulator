{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    xorg.libX11
    xorg.libXext
    pkg-config
    
    pulseaudio
    libpulseaudio
    alsaLib

    mesa
    mesa.dev
    mesa_glu
    libGL
    libGLU
  ];

  nativeBuildInputs = with pkgs; [ pkg-config ];
}
