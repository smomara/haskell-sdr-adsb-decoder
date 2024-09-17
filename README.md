# Haskell ADS-B Decoder with RTL-SDR

This project is a Haskell-based ADS-B decoder, inspired by dump1090, designed to track aircraft in real-time using an RTL-SDR. Currently, it's in early development, focusing on building the foundational libraries for SDR access and ADS-B message decoding.

## Features (In Progress)

* RTLSDR module: Opens the device, reads samples, tunes frequency/gain (via FFI with C code using the `rtlsdr` library)

* ADS-B module: Decodes messages and tracks aircraft information

* Planned: A terminal-bassed user interface for real-time plane tracking

## Getting Started

### Prerequisites

* RTL-SDR USB device

* Nix (for environment setup)

### Setup

1. Clone the repository:

```shell
git clone https://github.com/smomara/haskell-sdr-adsb-decoder.git
cd haskell-sdr-adsb-decoder
```

2. Use `nix-shell` to set up the environment:

```shell
nix-shell
```

3. Build and run tests:

```shell
cabal test
```

## Development Roadmap

* Enhance ADS-B module with error checking and CRC validation.

* Integrate all modules and build a TUI for real-time plane tracking.

* Implement a complete main program.

## License

Do whatever u want :)
