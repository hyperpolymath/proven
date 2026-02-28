-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeVPU - Video / vision processing unit safety
|||
||| This module provides type-safe primitives for video and vision processing
||| pipelines: codec selection, frame configuration validation, and resolution
||| bounds checking. All configurations are validated to prevent launching
||| pipelines with invalid parameters that would cause hardware faults.
module Proven.SafeVPU

%default total

--------------------------------------------------------------------------------
-- VPU Pipeline Stages
--------------------------------------------------------------------------------

||| Hardware-accelerated video pipeline operations
public export
data VPUPipeline = Decode | Encode | Transcode | Inference

public export
Eq VPUPipeline where
  Decode    == Decode    = True
  Encode    == Encode    = True
  Transcode == Transcode = True
  Inference == Inference = True
  _         == _         = False

public export
Show VPUPipeline where
  show Decode    = "Decode"
  show Encode    = "Encode"
  show Transcode = "Transcode"
  show Inference = "Inference"

--------------------------------------------------------------------------------
-- Video Codecs
--------------------------------------------------------------------------------

||| Supported video and image codecs
public export
data VideoCodec = H264 | H265 | AV1 | VP9 | JPEG

public export
Eq VideoCodec where
  H264 == H264 = True
  H265 == H265 = True
  AV1  == AV1  = True
  VP9  == VP9  = True
  JPEG == JPEG = True
  _    == _    = False

public export
Show VideoCodec where
  show H264 = "H.264"
  show H265 = "H.265"
  show AV1  = "AV1"
  show VP9  = "VP9"
  show JPEG = "JPEG"

--------------------------------------------------------------------------------
-- Pixel Formats
--------------------------------------------------------------------------------

||| Common pixel / colour space formats
public export
data PixelFormat = NV12 | YUV420 | RGB24 | RGBA32 | BGR24

public export
Eq PixelFormat where
  NV12   == NV12   = True
  YUV420 == YUV420 = True
  RGB24  == RGB24  = True
  RGBA32 == RGBA32 = True
  BGR24  == BGR24  = True
  _      == _      = False

public export
Show PixelFormat where
  show NV12   = "NV12"
  show YUV420 = "YUV420"
  show RGB24  = "RGB24"
  show RGBA32 = "RGBA32"
  show BGR24  = "BGR24"

||| Bytes per pixel for a given format
||| NV12 and YUV420 use 1.5 bytes/pixel on average; we return the
||| per-plane numerator and denominator to stay in Nat arithmetic.
||| For simplicity, this returns the luma-plane bytes-per-pixel (1 for
||| planar YUV, 3 for packed RGB, 4 for RGBA).
public export
bytesPerPixel : PixelFormat -> Nat
bytesPerPixel NV12   = 1
bytesPerPixel YUV420 = 1
bytesPerPixel RGB24  = 3
bytesPerPixel RGBA32 = 4
bytesPerPixel BGR24  = 3

--------------------------------------------------------------------------------
-- Frame Configuration
--------------------------------------------------------------------------------

||| Video frame configuration
||| @ width    Frame width in pixels (must be positive)
||| @ height   Frame height in pixels (must be positive)
||| @ format   Pixel / colour space format
||| @ bitDepth Bits per colour channel (typically 8, 10, or 12)
public export
record FrameConfig where
  constructor MkFrameConfig
  width    : Nat
  height   : Nat
  format   : PixelFormat
  bitDepth : Nat

public export
Eq FrameConfig where
  a == b = a.width    == b.width
        && a.height   == b.height
        && a.format   == b.format
        && a.bitDepth == b.bitDepth

public export
Show FrameConfig where
  show cfg = "Frame(" ++ show cfg.width ++ "x" ++ show cfg.height
          ++ ", " ++ show cfg.format
          ++ ", " ++ show cfg.bitDepth ++ "bit)"

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Maximum supported resolution per dimension (16384 = 16K)
maxResolution : Nat
maxResolution = 16384

||| Allowed bit depths for video frames
isValidBitDepth : Nat -> Bool
isValidBitDepth 8  = True
isValidBitDepth 10 = True
isValidBitDepth 12 = True
isValidBitDepth _  = False

||| Check if width and height are within hardware limits
public export
isResolutionValid : (width : Nat) -> (height : Nat) -> Bool
isResolutionValid w h =
  w >= 1 && w <= maxResolution && h >= 1 && h <= maxResolution

||| Validate a complete frame configuration
||| Checks resolution bounds and bit depth
public export
validateFrameConfig : FrameConfig -> Maybe FrameConfig
validateFrameConfig cfg =
  if isResolutionValid cfg.width cfg.height && isValidBitDepth cfg.bitDepth
    then Just cfg
    else Nothing

||| Estimate raw frame size in bytes (width * height * bytesPerPixel)
public export
estimateFrameSize : FrameConfig -> Nat
estimateFrameSize cfg = cfg.width * cfg.height * bytesPerPixel cfg.format
