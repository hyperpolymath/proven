-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-media streaming server.
||| Defines closed sum types for media types, codecs, streaming protocols,
||| transcoding profiles, and player events.
module Media.Types

%default total

---------------------------------------------------------------------------
-- Media type: the kind of media resource
---------------------------------------------------------------------------

||| Classification of a media resource.
public export
data MediaType : Type where
  Audio      : MediaType
  Video      : MediaType
  LiveStream : MediaType
  Playlist   : MediaType
  Subtitle   : MediaType

export
Show MediaType where
  show Audio      = "Audio"
  show Video      = "Video"
  show LiveStream = "LiveStream"
  show Playlist   = "Playlist"
  show Subtitle   = "Subtitle"

---------------------------------------------------------------------------
-- Codec: audio and video codecs
---------------------------------------------------------------------------

||| Audio and video codec identifiers.
public export
data Codec : Type where
  H264 : Codec
  H265 : Codec
  AV1  : Codec
  VP9  : Codec
  AAC  : Codec
  Opus : Codec
  FLAC : Codec
  MP3  : Codec

export
Show Codec where
  show H264 = "H.264"
  show H265 = "H.265"
  show AV1  = "AV1"
  show VP9  = "VP9"
  show AAC  = "AAC"
  show Opus = "Opus"
  show FLAC = "FLAC"
  show MP3  = "MP3"

---------------------------------------------------------------------------
-- Stream protocol: transport protocols for media delivery
---------------------------------------------------------------------------

||| Transport protocol used to deliver media streams.
public export
data StreamProtocol : Type where
  ||| HTTP Live Streaming (Apple).
  HLS   : StreamProtocol
  ||| Dynamic Adaptive Streaming over HTTP (MPEG).
  DASH  : StreamProtocol
  ||| Real-Time Messaging Protocol.
  RTMP  : StreamProtocol
  ||| Real-Time Streaming Protocol.
  RTSP  : StreamProtocol
  ||| Web Real-Time Communication.
  WebRTC : StreamProtocol
  ||| Secure Reliable Transport.
  SRT   : StreamProtocol

export
Show StreamProtocol where
  show HLS    = "HLS"
  show DASH   = "DASH"
  show RTMP   = "RTMP"
  show RTSP   = "RTSP"
  show WebRTC = "WebRTC"
  show SRT    = "SRT"

---------------------------------------------------------------------------
-- Transcode profile: quality presets for transcoding
---------------------------------------------------------------------------

||| Transcoding quality preset.
public export
data TranscodeProfile : Type where
  Passthrough : TranscodeProfile
  Low         : TranscodeProfile
  Medium      : TranscodeProfile
  High        : TranscodeProfile
  Ultra       : TranscodeProfile

export
Show TranscodeProfile where
  show Passthrough = "Passthrough"
  show Low         = "Low"
  show Medium      = "Medium"
  show High        = "High"
  show Ultra       = "Ultra"

---------------------------------------------------------------------------
-- Player event: events emitted by the media player
---------------------------------------------------------------------------

||| Events emitted by a media player during playback.
public export
data PlayerEvent : Type where
  Play          : PlayerEvent
  Pause         : PlayerEvent
  Seek          : PlayerEvent
  Stop          : PlayerEvent
  BufferStart   : PlayerEvent
  BufferEnd     : PlayerEvent
  Error         : PlayerEvent
  QualityChange : PlayerEvent

export
Show PlayerEvent where
  show Play          = "Play"
  show Pause         = "Pause"
  show Seek          = "Seek"
  show Stop          = "Stop"
  show BufferStart   = "BufferStart"
  show BufferEnd     = "BufferEnd"
  show Error         = "Error"
  show QualityChange = "QualityChange"
