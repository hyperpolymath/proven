-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- NTP Clock Filter Algorithm (RFC 5905 Section 10)
--
-- The clock filter maintains a window of the 8 most recent samples
-- from each peer. For each sample, we have the measured offset, delay,
-- and dispersion. The algorithm selects the sample with the minimum
-- delay (as it is most likely to be accurate) and uses its offset.
-- Dispersion and jitter are computed from the filtered samples.

module NTP.Filter

import NTP.Timestamp

%default total

-- ============================================================================
-- Clock filter sample
-- ============================================================================

||| A single clock filter sample from one NTP exchange.
||| Each sample captures the measured offset, round-trip delay,
||| and estimated dispersion from a single request/response pair.
public export
record ClockSample where
  constructor MkClockSample
  ||| Measured clock offset (server time - client time).
  offset     : NTPTimestamp
  ||| Round-trip delay for this sample.
  delay      : NTPTimestamp
  ||| Estimated dispersion (error bound) for this sample.
  dispersion : NTPTimestamp
  ||| Sequence number for ordering (monotonically increasing).
  epoch      : Nat

public export
Show ClockSample where
  show s = "Sample{offset=" ++ show s.offset
           ++ ", delay=" ++ show s.delay
           ++ ", disp=" ++ show s.dispersion
           ++ ", epoch=" ++ show s.epoch ++ "}"

-- ============================================================================
-- Clock filter register (8 samples)
-- ============================================================================

||| The clock filter register holds up to 8 samples.
||| The fixed size of 8 is specified in RFC 5905 Section 10.
||| We use a List with a maximum length invariant enforced at insertion.
public export
record ClockFilter where
  constructor MkClockFilter
  ||| The sample register, newest first, at most 8 entries.
  samples    : List ClockSample
  ||| The current epoch counter (incremented with each new sample).
  nextEpoch  : Nat

||| Maximum number of samples in the clock filter register.
public export
filterSize : Nat
filterSize = 8

||| Create a new empty clock filter.
public export
newFilter : ClockFilter
newFilter = MkClockFilter [] 0

||| Add a new sample to the clock filter register.
||| If the register already contains 8 samples, the oldest is discarded.
public export
addSample : ClockSample -> ClockFilter -> ClockFilter
addSample sample filt =
  let tagged = { epoch := filt.nextEpoch } sample
      newSamples = take filterSize (tagged :: filt.samples)
  in MkClockFilter newSamples (filt.nextEpoch + 1)

||| Create a sample from NTP exchange timestamps.
||| Computes offset and delay from the four timestamps (t1, t2, t3, t4).
public export
mkSampleFromExchange : (t1, t2, t3, t4 : NTPTimestamp) -> ClockSample
mkSampleFromExchange t1 t2 t3 t4 = MkClockSample
  { offset     = clockOffset t1 t2 t3 t4
  , delay      = roundTripDelay t1 t2 t3 t4
  , dispersion = MkNTPTimestamp 0 0  -- Initial dispersion, grows with time
  , epoch      = 0  -- Will be set by addSample
  }

-- ============================================================================
-- Sample selection (minimum delay)
-- ============================================================================

||| Compare two timestamps, returning True if a < b.
||| Used for selecting the sample with minimum delay.
timestampLessThan : NTPTimestamp -> NTPTimestamp -> Bool
timestampLessThan a b =
  case compare a.seconds b.seconds of
    LT => True
    GT => False
    EQ => a.fraction < b.fraction

||| Select the sample with the minimum delay from a list of samples.
||| The sample with the smallest round-trip delay is most likely to have
||| the most accurate offset measurement (RFC 5905 Section 10).
public export
selectBestSample : List ClockSample -> Maybe ClockSample
selectBestSample []        = Nothing
selectBestSample (s :: ss) = Just (foldl pickMin s ss)
  where
    pickMin : ClockSample -> ClockSample -> ClockSample
    pickMin best candidate =
      if timestampLessThan candidate.delay best.delay
        then candidate
        else best

-- ============================================================================
-- Jitter calculation
-- ============================================================================

||| Absolute difference between two timestamps.
absDiff : NTPTimestamp -> NTPTimestamp -> NTPTimestamp
absDiff a b =
  if a >= b then subTimestamp a b
  else subTimestamp b a

||| Calculate peer jitter from the clock filter samples.
||| Jitter is the root-mean-square (RMS) of the offset differences
||| between successive samples. Here we approximate using the mean
||| absolute difference for simplicity (avoiding floating point).
|||
||| Returns the mean absolute offset difference as a timestamp.
public export
calculateJitter : List ClockSample -> NTPTimestamp
calculateJitter []  = nullTimestamp
calculateJitter [_] = nullTimestamp
calculateJitter samples =
  let offsets = map (.offset) samples
      diffs = pairwiseDiffs offsets
      count = length diffs
  in if count == 0
       then nullTimestamp
       else divByCount (sumTimestamps diffs) count
  where
    pairwiseDiffs : List NTPTimestamp -> List NTPTimestamp
    pairwiseDiffs [] = []
    pairwiseDiffs [_] = []
    pairwiseDiffs (a :: b :: rest) = absDiff a b :: pairwiseDiffs (b :: rest)

    sumTimestamps : List NTPTimestamp -> NTPTimestamp
    sumTimestamps [] = nullTimestamp
    sumTimestamps (t :: ts) = addTimestamp t (sumTimestamps ts)

    -- Approximate division by count using repeated halving.
    -- This is a rough approximation but avoids floating point.
    divByCount : NTPTimestamp -> Nat -> NTPTimestamp
    divByCount ts 0 = ts
    divByCount ts 1 = ts
    divByCount ts n =
      if n >= 8 then halfTimestamp (halfTimestamp (halfTimestamp ts))
      else if n >= 4 then halfTimestamp (halfTimestamp ts)
      else if n >= 2 then halfTimestamp ts
      else ts

-- ============================================================================
-- Filter output
-- ============================================================================

||| The output of the clock filter algorithm for one peer.
public export
record FilterResult where
  constructor MkFilterResult
  ||| The selected clock offset (from the best sample).
  offset  : NTPTimestamp
  ||| The minimum delay across all samples.
  delay   : NTPTimestamp
  ||| Estimated jitter from sample-to-sample offset variation.
  jitter  : NTPTimestamp
  ||| Number of valid samples in the register.
  count   : Nat

public export
Show FilterResult where
  show r = "FilterResult{offset=" ++ show r.offset
           ++ ", delay=" ++ show r.delay
           ++ ", jitter=" ++ show r.jitter
           ++ ", n=" ++ show r.count ++ "}"

||| Run the clock filter algorithm on the current register.
||| Returns Nothing if the register is empty.
public export
runFilter : ClockFilter -> Maybe FilterResult
runFilter filt =
  case selectBestSample filt.samples of
    Nothing   => Nothing
    Just best => Just (MkFilterResult
      { offset = best.offset
      , delay  = best.delay
      , jitter = calculateJitter filt.samples
      , count  = length filt.samples
      })
