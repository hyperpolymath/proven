-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-media streaming server.
||| Prints server identification and enumerates core type constructors.
module Main

import Media

%default total

||| Print server name, ports, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (RTMP port " ++ show rtmpPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "HLS segment duration: " ++ show hlsSegmentDuration ++ "s"
  putStrLn $ "DASH segment duration: " ++ show dashSegmentDuration ++ "s"
  putStrLn $ "Max bitrate: " ++ show maxBitrate ++ " bps"
  putStrLn ""
  putStrLn "--- MediaType ---"
  printLn Audio
  printLn Video
  printLn LiveStream
  printLn Playlist
  printLn Subtitle
  putStrLn ""
  putStrLn "--- Codec ---"
  printLn H264
  printLn H265
  printLn AV1
  printLn VP9
  printLn AAC
  printLn Opus
  printLn FLAC
  printLn MP3
  putStrLn ""
  putStrLn "--- StreamProtocol ---"
  printLn HLS
  printLn DASH
  printLn RTMP
  printLn RTSP
  printLn WebRTC
  printLn SRT
  putStrLn ""
  putStrLn "--- TranscodeProfile ---"
  printLn Passthrough
  printLn Low
  printLn Medium
  printLn High
  printLn Ultra
  putStrLn ""
  putStrLn "--- PlayerEvent ---"
  printLn Play
  printLn Pause
  printLn Seek
  printLn Stop
  printLn BufferStart
  printLn BufferEnd
  printLn Error
  printLn QualityChange
