{-# LANGUAGE OverloadedStrings #-}

-- | Bus message framing.
--
-- One format: @[timestamp] sender: body@.
--
-- Re-exported from 'Circuit.Comm.Framing' so that circuits-repl and muster
-- share a single source of truth for framing.
module Muster.Framing
  ( frameMessage,
    parseMessage,
    parseMessageTs,
    formatNow,
  )
where

import Circuit.Comm.Framing (formatNow, frameMessage, parseMessage, parseMessageTs)
