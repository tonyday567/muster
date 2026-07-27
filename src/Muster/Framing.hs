{-# LANGUAGE OverloadedStrings #-}

-- | Bus message framing.
--
-- One format: @[timestamp] sender: body@.
--
-- Re-exported from 'Circuit.Agent.Framing' so that circuits-agent and muster
-- share a single source of truth for framing.
module Muster.Framing
  ( frameMessage,
    parseMessage,
    parseMessageTs,
    formatNow,
  )
where

import Circuit.Agent.Framing (formatNow, frameMessage, parseMessage, parseMessageTs)
