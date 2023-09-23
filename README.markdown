# Hoppie Standalone MCDU

A standalone MCDU for the Hoppie ACARS system.

## Introduction

This is a standalone application for interacting with the [Hoppie ACARS
system](https://www.hoppie.nl/acars/) as a sim pilot. It does not interface
with any flightsim (or at least, not yet); the intended use is with aircraft
models that do not offer Hoppie integration out of the box.

There are two frontends: a (rather spartan) terminal UI, and a web interface.

Since the MCDU runs completely independently from the flightsim, it can of
course run on a separate computer; the web interface is also accessible from
any web browser that can connect to the main MCDU process.

## Installation

### From Binaries

- Grab the latest binaries from github (TODO: package release with binaries)
- Unzip/untar to somewhere on your `$PATH`

### From Source

- Install prerequisites:
    - GHC 9.4 and Cabal 3.0 or later (easiest way is to use `ghcup`)
- Check out the repository:
    ```sh
    git clone https://github.com/tdammers/hoppie-standalone ./hoppie-standalone
    ```
- Build with cabal:
    ```sh
    cd hoppie-standalone
    cabal build
    cabal install
    ```

## Configuration

The main (or, currently, the only) Configuration file is
`~/.config/hoppie-mcdu/config.yaml`. An absolute minimum configuration file is:
```yaml
logon: password123
```
(Where "password123" is your Hoppie logon code).
Additional keys mirror the command-line options, and do the same thing.

Full list of options:

| command line | env var | config file | in-app | description |
|--------------|---------|-------------|--------|-------------|
| `-lLOGON`, `--logon LOGON` | `HOPPIE_LOGON` | `logon` | read-only | Hoppie logon code |
| `-cCALLSIGN`, `--callsign CALLSIGN` | `HOPPIE_CALLSIGN` | `callsign` | yes | Your callsign |
| `-tICAO`, `--aircraft-type ICAO` | n/a | `aircraft-type` | yes | ICAO aircraft type code for your aircraft (A32N, B738, ...)
| `-PINTERVAL`, `--fast-polling INTERVAL` | `HOPPIE_FAST_POLLING_INTERVAL` | fast-polling-interval | no | Fast polling interval, in seconds |
| `-pINTERVAL`, `--slow-polling INTERVAL` | `HOPPIE_SLOW_POLLING_INTERVAL` | slow-polling-interval | no | Slow polling interval, in seconds |
| `-uURL`, `--hoppie-url URL`, `--url URL` | `HOPPIE_URL` | `hoppie-url` | no | Hoppie ACARS connect URL |
| `--show-debug-log`, `--show-log` | n/a | `show-log` | yes | Show debug log on screen |
| `--http-server-hostname HOSTNAME` | `HOPPIE_HTTP_HOST` | `http-server-hostname` | yes | Expose MCDU as an HTTP app on this hostname.  Does not change the server itself, but causes a different URL to be shown in the log, and used for QR codes.
| `--http-server-port PORT` | `HOPPIE_HTTP_PORT` | `http-server-port` | yes | Expose MCDU as an HTTP app on this port
| `-H`, `--headless` | n/a | `headless` | yes | Headless mode (no terminal UI, only HTTP) |

## Usage

The `hoppie-mcdu` command, run in a terminal, should show something like this:

![](docs/startup-screen.png)

This is the terminal UI; you can operate it by using the letters and digits on
your keyboard for entering characters, while the function keys (F1 through
F12), PgUp/PgDn, and Esc, operate the line-select keys (LSK's) and MCDU
function keys, as labelled on screen. The Delete key is mapped to the MCDU's
DEL button, and the Backspace key to the MCDU's CLR button (both not shown in
the terminal UI).

If a web server is configured, you can navigate your browser to the configured
URL, and it should look like this:

![](docs/web-mcdu.png)

Both interfaces control the same simulated MCDU though, so you can use
whichever you prefer.

## MCDU Module

### DLK (Data LinK)

This module, accessible via the `DLK` option in the main menu, or by pushing
the `DLK` mode key, controls various ACARS functions:

- `RECVD MSGS` shows the uplink history (messages you have received)
- `SENT MSGS` shows the downlink history (messages you have sent)
- `TELEX` opens a TELEX (free text message) editor
- `ATIS`, `METAR` and `TAF` open editors for requesting the respective
  information from the ACARS system (`ATIS` translates to `VATATIS` on the
  Hoppie network; no other ATIS sources are currently supported)
- `DCL` opens a DCL request editor (European style pre-departure clearance; the
  request is sent as an ACARS TELEX message, but the response will be issued
  via CPDLC)

### ATC (CPDLC, Controller-Pilot-DataLink Communications)

This module, accessible via the `ATC` option in the main menu, or by pushing
the `ATC` mode key, controls CPDLC functionality:

- `MSG LOG` shows the history of all CPDLC uplinks and downlinks
- `LOGON` opens the CPDLC logon/status page, from where a CPDLC logon can be
  initiated.
- Once comms are established, the `ATC` will additionally show a range of
  options for making all sorts of requests.

### Configuration module

This module, on page 2 of the main menu under `CONFIG`, allows you to change
some options for the current session; however, these options will not be
stored. If you want persistent changes, edit your configuration file.
On page 2, you can configure and enable/disable the built-in HTTP web server,
and output a QR code in the terminal (useful for accessing the web MCDU from a
smartphone or tablet).

### Status module

This module, on page 2 of the main menu under `STATUS`, shows the current
configuration and status of the MCDU system.
