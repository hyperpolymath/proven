-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeDSP - Digital signal processing safety
|||
||| This module provides type-safe primitives for digital signal processing
||| configuration: window function selection, filter specification, sample
||| rate validation, and Nyquist frequency enforcement. All parameters are
||| validated to prevent launching DSP pipelines with physically impossible
||| configurations (e.g. cutoff frequency above the Nyquist limit).
module Proven.SafeDSP

%default total

--------------------------------------------------------------------------------
-- Window Functions
--------------------------------------------------------------------------------

||| DSP windowing functions for spectral analysis
public export
data WindowFunction = Hamming | Hanning | Blackman | Kaiser | Rectangular

public export
Eq WindowFunction where
  Hamming     == Hamming     = True
  Hanning     == Hanning     = True
  Blackman    == Blackman    = True
  Kaiser      == Kaiser      = True
  Rectangular == Rectangular = True
  _           == _           = False

public export
Show WindowFunction where
  show Hamming     = "Hamming"
  show Hanning     = "Hanning"
  show Blackman    = "Blackman"
  show Kaiser      = "Kaiser"
  show Rectangular = "Rectangular"

--------------------------------------------------------------------------------
-- Filter Types
--------------------------------------------------------------------------------

||| Standard digital filter topologies
public export
data FilterType = LowPass | HighPass | BandPass | BandStop | AllPass

public export
Eq FilterType where
  LowPass  == LowPass  = True
  HighPass == HighPass = True
  BandPass == BandPass = True
  BandStop == BandStop = True
  AllPass  == AllPass  = True
  _        == _        = False

public export
Show FilterType where
  show LowPass  = "LowPass"
  show HighPass = "HighPass"
  show BandPass = "BandPass"
  show BandStop = "BandStop"
  show AllPass  = "AllPass"

--------------------------------------------------------------------------------
-- Sample Configuration
--------------------------------------------------------------------------------

||| Audio / signal sample configuration
||| @ sampleRate  Sample rate in Hz (must be positive)
||| @ bitDepth    Bits per sample (8, 16, 24, or 32)
||| @ channels    Number of audio channels (1-8)
public export
record SampleConfig where
  constructor MkSampleConfig
  sampleRate : Nat
  bitDepth   : Nat
  channels   : Nat

public export
Eq SampleConfig where
  a == b = a.sampleRate == b.sampleRate
        && a.bitDepth   == b.bitDepth
        && a.channels   == b.channels

public export
Show SampleConfig where
  show cfg = "Sample(" ++ show cfg.sampleRate ++ " Hz, "
          ++ show cfg.bitDepth ++ "bit, "
          ++ show cfg.channels ++ "ch)"

||| Allowed audio bit depths
isValidBitDepth : Nat -> Bool
isValidBitDepth 8  = True
isValidBitDepth 16 = True
isValidBitDepth 24 = True
isValidBitDepth 32 = True
isValidBitDepth _  = False

||| Validate a sample configuration
||| Checks sample rate is positive, bit depth is standard, channels in 1-8
public export
validateSampleConfig : SampleConfig -> Maybe SampleConfig
validateSampleConfig cfg =
  if cfg.sampleRate >= 1
     && isValidBitDepth cfg.bitDepth
     && cfg.channels >= 1 && cfg.channels <= 8
    then Just cfg
    else Nothing

||| Nyquist frequency: half the sample rate
public export
nyquistFrequency : SampleConfig -> Nat
nyquistFrequency cfg = cfg.sampleRate `div` 2

||| Check whether the Nyquist frequency is above a given target frequency
public export
isNyquistValid : SampleConfig -> (targetFreq : Nat) -> Bool
isNyquistValid cfg target = nyquistFrequency cfg > target

--------------------------------------------------------------------------------
-- Filter Specification
--------------------------------------------------------------------------------

||| Digital filter specification
||| @ filterType      Filter topology
||| @ cutoffFreq      Cutoff frequency in Hz
||| @ order           Filter order (must be positive)
||| @ window          Window function for FIR design
public export
record FilterSpec where
  constructor MkFilterSpec
  filterType : FilterType
  cutoffFreq : Nat
  order      : Nat
  window     : WindowFunction

public export
Eq FilterSpec where
  a == b = a.filterType == b.filterType
        && a.cutoffFreq == b.cutoffFreq
        && a.order      == b.order
        && a.window     == b.window

public export
Show FilterSpec where
  show fs = "Filter(" ++ show fs.filterType
         ++ ", fc=" ++ show fs.cutoffFreq ++ " Hz"
         ++ ", order=" ++ show fs.order
         ++ ", " ++ show fs.window ++ ")"

||| Validate a filter specification against a sample configuration
||| The cutoff frequency must be strictly below the Nyquist frequency,
||| and the filter order must be at least 1
public export
validateFilterSpec : SampleConfig -> FilterSpec -> Maybe FilterSpec
validateFilterSpec cfg fs =
  if fs.order >= 1 && fs.cutoffFreq < nyquistFrequency cfg
    then Just fs
    else Nothing

||| Estimate the number of multiply-accumulate (MAC) operations per sample
||| for a given filter order. FIR filters require (order + 1) MACs per sample.
public export
estimateMACs : FilterSpec -> Nat
estimateMACs fs = fs.order + 1

||| Calculate the byte rate (bytes per second) for a sample configuration
public export
byteRate : SampleConfig -> Nat
byteRate cfg = cfg.sampleRate * (cfg.bitDepth `div` 8) * cfg.channels
