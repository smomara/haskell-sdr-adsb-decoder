{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    gcc
    gnumake
    pkg-config
    rtl-sdr
    ghc
    cabal-install
    libusb1
  ];

  shellHook = ''
    echo "RTL-SDR and Haskell development environment"
    echo "Available tools: gcc, make, pkg-config, rtl-sdr, ghc, cabal"
    echo "Use 'cabal build' to build the Haskell project"
    echo "Use 'cabal run' to run the Haskell project"

    export RTL_SDR_INCLUDE="${pkgs.rtl-sdr}/include"
    export RTL_SDR_LIB="${pkgs.rtl-sdr}/lib"
    export LD_LIBRARY_PATH="${pkgs.rtl-sdr}/lib:$LD_LIBRARY_PATH"
    export PKG_CONFIG_PATH="${pkgs.rtl-sdr}/lib/pkgconfig:${pkgs.fftw}/lib/pkgconfig:$PKG_CONFIG_PATH"
  '';

  NIX_CFLAGS_COMPILE = "-I${pkgs.rtl-sdr}/include -I${pkgs.fftw}/include";
  NIX_LDFLAGS = "-L${pkgs.rtl-sdr}/lib -L${pkgs.fftw}/lib";
}
