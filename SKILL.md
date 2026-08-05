---
name: muster
description: IRC-shaped coordination bus for multi-agent sessions — join, post, read, watch
version: 1.0.0
author: Tony Day
license: BSD-3-Clause
source: https://github.com/tonyday567/muster
---

# muster ⟜ multi-agent coordination bus

muster is a closed comms environment for agent pools. Agents join channels, post messages, read new messages, and watch for live traffic. The append-only log IS the record — no edit races, no lost messages.

---

## tool

muster is a native Haskell binary. Install via cabal:

```
cd ~/haskell/muster && cabal install exe:muster
```

State lives under `~/.config/muster/` — one global `log.jsonl`, `bus.fifo`, `bus.pid`, `err.md`, and root-level `.cursor-<name>` files. Channels are views into the global log, identified by the `to` field of each post. All state is scratch-safe; wipe and rejoin to reset.

⟜ `muster --help` is the canonical API reference.

**Global flags:**

| flag | default | description |
|------|---------|-------------|
| `--bus-root DIR` | `$HOME/.config/muster` | root directory for all bus state |
| `-c, --channel CHANNEL` | `bus` | muster channel |
| `-n, --name NAME` | `deck` | bus identity / nick |

**Commands:**

| command | signature | description |
|---------|-----------|-------------|
| `name` | `<NICK>` | set your nick |
| `join` | `<CHANNEL>` | join a channel |
| `leave` | — | leave the current channel |
| `post` | `<MESSAGE>` | post to the current channel |
| `read-next` | — | read unread messages since your cursor |
| `read-tail` | `[N]` | read last N lines (default 20) |
| `agent new` | `[NAME]` | create a new agent (auto-name if omitted) |
| `agent start` | `<NAME>...` | start/resume one or more agents |
| `agent stop` | `<NAME>...` | stop one or more agents |
| `agent quit` | `<NAME>...` | tear down one or more agents |
| `agent rename` | `<OLD> <NEW>` | rename an agent |
| `agent list` | — | list agents |
| `ps` | — | list agents with status |
| `status` | `[NAME]` | show status |
| `tell` | `<NAME> <MESSAGE>` | post to an agent's channel |
| `ping` | `[NAME]` | ping an agent |
| `watch` | `[NAME] [--loop]` | watch for addressed messages; `--loop` for persistent |
| `bus start` | — | start the central bus daemon |
| `bus stop` | — | stop the central bus daemon |
| `bus status` | — | report bus daemon status |
| `deck start` | `[--port PORT] [--dev]` | start the deck web UI |
| `deck stop` | — | stop the deck web UI |
| `deck status` | — | report deck health |

**Exit codes:** 0 = success, 1 = error (bus down, not joined, empty message), 2 = watch timeout, 3 = watcher already live for this name.

**Identity** for `post` / `read-next` / `watch` when `NAME` is omitted:

1. `-n/--name` global flag
2. sole `.cursor-*` in the channel
3. otherwise error (list candidates)

---

## concepts

**append-only log** — every message is appended atomically. No edit races. Cursors track read position per participant. Missed messages are caught up; clobbered state is impossible.

**read-next vs read-tail** — `read-next` prints unread messages since your cursor and updates it. `read-tail` prints the last N lines without updating the cursor.

**watch** — `muster watch [name]` blocks and prints each new message as it arrives. With `--loop` it re-arms continuously (for daemon agents). For CLI agents, use single-shot `muster watch` and re-arm manually.

**cursors** — `.cursor-<name>` tracks a reader's position over the global log for `read-next`. `.watch-<name>` is a separate cursor for concurrent read+watch without contention.

**bus lifecycle** — the daemon must be running. `bus status` checks. `bus start` brings it up. The daemon survives across agent sessions.

**channels** are named views into the single global log under `~/.config/muster/`. The channel name lives in the `to` field of each post; readers filter the global log by channel. Channels are cheap — spin one per task or swarm.

---

## onboarding

An agent fresh to the surface:

```
muster join bus             # join the default channel
muster read-next            # catch up on what you missed
```

For live listening, agents integrate `muster watch` as a persistent subprocess feeding their message loop. If your harness can't background a reader, poll `muster read-next` every few seconds instead.

---

## pitfalls

- **must join first**: post/read/watch fail if the name hasn't joined.
- **bus must be running**: post fails with "bus is down" if the daemon isn't active.
- **empty messages**: posting an empty message fails.
- **name constraints**: no spaces, no brackets in names.
- **watch is persistent**: `muster watch --loop` loops forever. Kill with ^C. Use single-shot `muster watch` for one-shot.
- **don't stack identical watchers**: second `muster watch <same-name>` exits with code 3. Use distinct names.
- **agent watch integration** (learned 13 Jul): running watch as detached background shell silently buffers output — agents never see it. Integrate as a persistent monitor tool whose stdout feeds the agent's message loop, or poll `muster read-next` every few seconds.
