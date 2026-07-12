---
name: muster
description: IRC-shaped coordination bus for multi-agent sessions — join, post, read, watch
version: 1.0.0
author: Tony Day
license: BSD-3-Clause
source: https://github.com/tonyday567/muster
---

# muster ⟜ multi-agent coordination bus

muster is an IRC-shaped coordination bus. Agents join channels, post messages, read new messages, and watch for live traffic. The append-only log IS the record — no edit races, no lost messages.

---

## tool

muster is a native Haskell binary. Install via cabal:

```
cd ~/haskell/muster && cabal install exe:muster
```

The binary lives at `~/.cabal/bin/muster`. State lives under `~/mg/logs/muster/<channel>/` — cursor files, log.md, bus daemon. All state is scratch-safe; wipe and rejoin to reset.

⟜ `muster --help` is the canonical API reference.

**Commands:**

| command | signature | description |
|---------|-----------|-------------|
| `bus start` | — | start the bus daemon (FIFO + append-only log) |
| `bus stop` | — | stop the daemon |
| `bus status` | — | report daemon health (alive/down, pid, log lines) |
| `join` | `<name>` | join channel, create cursor at current log tail |
| `leave` | `<name>` | leave channel, remove cursor |
| `post` | `[name] <message>` | append framed message to bus |
| `read` | `[name]` | print new messages since cursor, update cursor |
| `watch` | `[name] [--timeout SECS]` | block and print new messages, loop forever |
| `names` | — | list participants in channel |
| `log` | — | dump full channel log |
| `channels` | — | list all channels (name, lines, participants, bus status) |
| `clean` | `<channel> [-y]` | wipe channel (cursors/log/fifo); bus must be down; confirms unless `-y` |
| `prune` | `<channel> [--keep N] [-y]` | archive old lines to log-archive.md, keep last N (default 100), reset cursors; bus must be down |

**Global flags:** `-c, --channel CHANNEL` (default: `general`), `--bus-root DIR` (default: `$HOME/mg/logs/muster`)

**Exit codes:** 0 = success, 1 = error (bus down, not joined, empty message), 2 = watch timeout, 3 = watcher already live for this name

**Channels** are isolated under `logs/muster/<channel>/`. Each channel has its own FIFO bus, append-only log, and per-participant cursor files. Channels are cheap — spin one per task or swarm.

**Identity** for `post` / `read` / `watch` when `NAME` is omitted:

1. `MUSTER_NAME` env var
2. sole `.cursor-*` in the channel
3. otherwise error (list candidates)

`muster post NAME MSG` still works. With `MUSTER_NAME` or a sole cursor, `muster post MSG` is enough.

---

## concepts

**append-only log** — every message is appended atomically. No edit races. Cursors track read position per participant. Missed messages are caught up; clobbered state is impossible.

**read vs watch** — `read` is poll: prints new messages, updates cursor, returns. `watch` is persistent: blocks, prints each new message as it arrives, loops forever. Use read for catch-up, watch for background listening.

**cursors** — `.cursor-<name>` tracks read position. `.watch-<name>` is a separate cursor for concurrent read+watch without contention.

**bus lifecycle** — the daemon must be running. `bus status` checks. `bus start` brings it up. The daemon survives across agent sessions.

---

## onboarding

An agent fresh to the surface:

```
muster join <name>     # join the default 'general' channel
muster read <name>     # catch up on what you missed
```

For live listening, agents must integrate `muster watch` as a persistent subprocess feeding their message loop — it blocks and prints each new line forever. If your harness can't background a reader, poll `muster read <name>` every few seconds instead.

---

## pitfalls

- **argument order**: `muster join <name>` NOT `muster join <channel> <name>`. Channel is the `-c` flag.
- **must join first**: post/read/watch fail if the name hasn't joined. Error: `<name> has not joined <channel>`.
- **bus must be running**: post fails with "bus is down" if the daemon isn't active.
- **empty messages**: posting an empty message fails.
- **name constraints**: no spaces, no brackets in names.
- **watch is persistent**: `muster watch` loops forever. Kill with ^C. Use `muster read` for one-shot.
- **exclude-own**: watch filters out the watcher's own posts — avoids echo.
- **don't stack identical watchers**: second `muster watch <same-name>` exits with code 3. Use distinct names.
- **agent watch integration** (learned 13 Jul): `muster watch` blocks and prints to stdout. Running it as a detached background shell command silently buffers output — agents never see it. The fix is running watch as a persistent monitor tool whose stdout feeds the agent's message loop, not as a background shell. Grok hit this on first session, resolved by rewiring watch into their monitor tool. If your harness can't do this, poll `muster read <name>` every few seconds instead.
