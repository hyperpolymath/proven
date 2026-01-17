// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeArchive - Archive format detection and validation that cannot crash.
 *
 * Provides safe functions to detect and validate common archive formats
 * (zip, tar, gzip, bzip2, xz, 7z, rar) based on magic bytes.
 * All operations are bounds-checked and cannot crash.
 */

/** Error types for archive operations */
type archiveError =
  | UnknownFormat
  | BufferTooSmall
  | InvalidHeader
  | CorruptedArchive

/** Supported archive formats */
type archiveFormat =
  | Zip
  | Tar
  | Gzip
  | Bzip2
  | Xz
  | SevenZip
  | Rar
  | Zstd

/** Confidence level for format detection */
type confidence =
  | High // Magic bytes match perfectly
  | Medium // Some indicators present
  | Low // Might be this format
  | None // Not detected

/** TAR block size (512 bytes) */
let tarBlockSize = 512

/** Minimum bytes needed for reliable format detection */
let minDetectionBytes = 512

/** Get the file extension for a format */
let formatExtension = (format: archiveFormat): string => {
  switch format {
  | Zip => ".zip"
  | Tar => ".tar"
  | Gzip => ".gz"
  | Bzip2 => ".bz2"
  | Xz => ".xz"
  | SevenZip => ".7z"
  | Rar => ".rar"
  | Zstd => ".zst"
  }
}

/** Get the MIME type for a format */
let formatMimeType = (format: archiveFormat): string => {
  switch format {
  | Zip => "application/zip"
  | Tar => "application/x-tar"
  | Gzip => "application/gzip"
  | Bzip2 => "application/x-bzip2"
  | Xz => "application/x-xz"
  | SevenZip => "application/x-7z-compressed"
  | Rar => "application/vnd.rar"
  | Zstd => "application/zstd"
  }
}

/** Get the human-readable name for a format */
let formatDisplayName = (format: archiveFormat): string => {
  switch format {
  | Zip => "ZIP"
  | Tar => "TAR"
  | Gzip => "GZIP"
  | Bzip2 => "BZIP2"
  | Xz => "XZ"
  | SevenZip => "7-Zip"
  | Rar => "RAR"
  | Zstd => "Zstandard"
  }
}

/** Get minimum bytes needed to detect a specific format */
let minBytesForFormat = (format: archiveFormat): int => {
  switch format {
  | Zip => 4
  | Tar => 262 // Need to reach "ustar" magic
  | Gzip => 2
  | Bzip2 => 3
  | Xz => 6
  | SevenZip => 6
  | Rar => 8
  | Zstd => 4
  }
}

/** Archive format to string representation */
let formatToString = (format: archiveFormat): string => {
  switch format {
  | Zip => "zip"
  | Tar => "tar"
  | Gzip => "gzip"
  | Bzip2 => "bzip2"
  | Xz => "xz"
  | SevenZip => "7z"
  | Rar => "rar"
  | Zstd => "zstd"
  }
}

/** Check if data matches magic bytes at given offset */
let matchesMagic = (data: array<int>, magic: array<int>, offset: int): bool => {
  let dataLen = Belt.Array.length(data)
  let magicLen = Belt.Array.length(magic)

  if dataLen < offset + magicLen {
    false
  } else {
    let matches = ref(true)
    for i in 0 to magicLen - 1 {
      if matches.contents {
        let dataByte = Belt.Array.getUnsafe(data, offset + i)
        let magicByte = Belt.Array.getUnsafe(magic, i)
        if dataByte != magicByte {
          matches := false
        }
      }
    }
    matches.contents
  }
}

/** ZIP magic bytes: PK\x03\x04 */
let zipMagic = [0x50, 0x4B, 0x03, 0x04]

/** ZIP empty archive magic: PK\x05\x06 */
let zipEmptyMagic = [0x50, 0x4B, 0x05, 0x06]

/** GZIP magic bytes: \x1f\x8b */
let gzipMagic = [0x1F, 0x8B]

/** BZIP2 magic bytes: BZh */
let bzip2Magic = [0x42, 0x5A, 0x68]

/** XZ magic bytes: \xFD7zXZ\x00 */
let xzMagic = [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

/** 7-Zip magic bytes: 7z\xBC\xAF\x27\x1C */
let sevenZipMagic = [0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C]

/** RAR 5.0 magic bytes: Rar!\x1A\x07\x01\x00 */
let rar5Magic = [0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x01, 0x00]

/** RAR 4.x magic bytes: Rar!\x1A\x07\x00 */
let rar4Magic = [0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x00]

/** Zstandard magic bytes: \x28\xB5\x2F\xFD */
let zstdMagic = [0x28, 0xB5, 0x2F, 0xFD]

/** USTAR magic bytes for TAR detection */
let ustarMagic = [0x75, 0x73, 0x74, 0x61, 0x72] // "ustar"

/** Check if data appears to be a TAR archive */
let isTarArchive = (data: array<int>): bool => {
  // TAR files have "ustar" at offset 257
  let ustarOffset = 257
  matchesMagic(data, ustarMagic, ustarOffset)
}

/** Detect archive format from magic bytes */
let detectFormat = (data: array<int>): option<archiveFormat> => {
  // Check ZIP magic
  if matchesMagic(data, zipMagic, 0) || matchesMagic(data, zipEmptyMagic, 0) {
    Some(Zip)
  } else if matchesMagic(data, gzipMagic, 0) {
    Some(Gzip)
  } else if matchesMagic(data, bzip2Magic, 0) {
    Some(Bzip2)
  } else if matchesMagic(data, xzMagic, 0) {
    Some(Xz)
  } else if matchesMagic(data, sevenZipMagic, 0) {
    Some(SevenZip)
  } else if matchesMagic(data, rar5Magic, 0) || matchesMagic(data, rar4Magic, 0) {
    Some(Rar)
  } else if matchesMagic(data, zstdMagic, 0) {
    Some(Zstd)
  } else if isTarArchive(data) {
    Some(Tar)
  } else {
    None
  }
}

/** Check if data is a valid archive of any supported format */
let isValidArchive = (data: array<int>): bool => {
  Option.isSome(detectFormat(data))
}

/** Archive detection result with confidence level */
type detectionResult = {
  format: option<archiveFormat>,
  confidence: confidence,
  reason: string,
}

/** Detect archive format with confidence level */
let detectFormatWithConfidence = (data: array<int>): detectionResult => {
  switch detectFormat(data) {
  | Some(format) => {
      format: Some(format),
      confidence: High,
      reason: "Magic bytes match",
    }
  | None =>
    if Belt.Array.length(data) < minDetectionBytes {
      {
        format: None,
        confidence: Low,
        reason: "Insufficient data for reliable detection",
      }
    } else {
      {
        format: None,
        confidence: None,
        reason: "No known archive format detected",
      }
    }
  }
}

/** ZIP local file header information */
type zipHeaderInfo = {
  versionNeeded: int,
  flags: int,
  compressionMethod: int,
  filenameLen: int,
  extraLen: int,
  headerSize: int,
}

/** Read a 16-bit little-endian value from byte array */
let readUint16Le = (data: array<int>, offset: int): option<int> => {
  if Belt.Array.length(data) < offset + 2 {
    None
  } else {
    let low = Belt.Array.getUnsafe(data, offset)
    let high = Belt.Array.getUnsafe(data, offset + 1)
    Some(low + lsl(high, 8))
  }
}

/** Read a 32-bit little-endian value from byte array */
let readUint32Le = (data: array<int>, offset: int): option<int> => {
  if Belt.Array.length(data) < offset + 4 {
    None
  } else {
    let b0 = Belt.Array.getUnsafe(data, offset)
    let b1 = Belt.Array.getUnsafe(data, offset + 1)
    let b2 = Belt.Array.getUnsafe(data, offset + 2)
    let b3 = Belt.Array.getUnsafe(data, offset + 3)
    Some(b0 + lsl(b1, 8) + lsl(b2, 16) + lsl(b3, 24))
  }
}

/** Validate ZIP archive header structure */
let validateZipHeader = (data: array<int>): result<zipHeaderInfo, archiveError> => {
  if Belt.Array.length(data) < 30 {
    Error(BufferTooSmall)
  } else if !matchesMagic(data, zipMagic, 0) {
    Error(InvalidHeader)
  } else {
    switch (
      readUint16Le(data, 4),
      readUint16Le(data, 6),
      readUint16Le(data, 8),
      readUint16Le(data, 26),
      readUint16Le(data, 28),
    ) {
    | (Some(versionNeeded), Some(flags), Some(compressionMethod), Some(filenameLen), Some(extraLen)) =>
      let headerSize = 30 + filenameLen + extraLen
      if Belt.Array.length(data) < headerSize {
        Error(BufferTooSmall)
      } else {
        Ok({
          versionNeeded,
          flags,
          compressionMethod,
          filenameLen,
          extraLen,
          headerSize,
        })
      }
    | _ => Error(InvalidHeader)
    }
  }
}

/** Check if a ZIP file is encrypted */
let isZipEncrypted = (info: zipHeaderInfo): bool => {
  land(info.flags, 0x0001) != 0
}

/** Get the compression method name for a ZIP file */
let zipCompressionName = (method: int): string => {
  switch method {
  | 0 => "stored"
  | 1 => "shrunk"
  | 6 => "imploded"
  | 8 => "deflated"
  | 9 => "deflate64"
  | 12 => "bzip2"
  | 14 => "lzma"
  | 93 => "zstd"
  | 95 => "xz"
  | _ => "unknown"
  }
}

/** GZIP header information */
type gzipHeaderInfo = {
  compressionMethod: int,
  flags: int,
  mtime: int,
  extraFlags: int,
  os: int,
  headerSize: int,
}

/** Validate GZIP header structure */
let validateGzipHeader = (data: array<int>): result<gzipHeaderInfo, archiveError> => {
  if Belt.Array.length(data) < 10 {
    Error(BufferTooSmall)
  } else {
    let b0 = Belt.Array.getUnsafe(data, 0)
    let b1 = Belt.Array.getUnsafe(data, 1)
    if b0 != 0x1F || b1 != 0x8B {
      Error(InvalidHeader)
    } else {
      let compressionMethod = Belt.Array.getUnsafe(data, 2)
      let flags = Belt.Array.getUnsafe(data, 3)
      let extraFlags = Belt.Array.getUnsafe(data, 8)
      let os = Belt.Array.getUnsafe(data, 9)

      // Only deflate compression is supported
      if compressionMethod != 8 {
        Error(InvalidHeader)
      } else {
        switch readUint32Le(data, 4) {
        | None => Error(InvalidHeader)
        | Some(mtime) =>
          // Calculate header size based on flags
          let headerSize = ref(10)

          // FEXTRA flag
          if land(flags, 0x04) != 0 {
            switch readUint16Le(data, headerSize.contents) {
            | None => ()
            | Some(xlen) => headerSize := headerSize.contents + 2 + xlen
            }
          }

          // FNAME flag - skip null-terminated filename
          if land(flags, 0x08) != 0 {
            let i = ref(headerSize.contents)
            while i.contents < Belt.Array.length(data) && Belt.Array.getUnsafe(data, i.contents) != 0 {
              i := i.contents + 1
            }
            if i.contents < Belt.Array.length(data) {
              headerSize := i.contents + 1
            }
          }

          // FCOMMENT flag - skip null-terminated comment
          if land(flags, 0x10) != 0 {
            let i = ref(headerSize.contents)
            while i.contents < Belt.Array.length(data) && Belt.Array.getUnsafe(data, i.contents) != 0 {
              i := i.contents + 1
            }
            if i.contents < Belt.Array.length(data) {
              headerSize := i.contents + 1
            }
          }

          // FHCRC flag
          if land(flags, 0x02) != 0 {
            headerSize := headerSize.contents + 2
          }

          if Belt.Array.length(data) < headerSize.contents {
            Error(BufferTooSmall)
          } else {
            Ok({
              compressionMethod,
              flags,
              mtime,
              extraFlags,
              os,
              headerSize: headerSize.contents,
            })
          }
        }
      }
    }
  }
}

/** Check if a GZIP file has an original filename */
let gzipHasFilename = (info: gzipHeaderInfo): bool => {
  land(info.flags, 0x08) != 0
}

/** Check if a GZIP file has a comment */
let gzipHasComment = (info: gzipHeaderInfo): bool => {
  land(info.flags, 0x10) != 0
}

/** Get the OS name from a GZIP header */
let gzipOsName = (os: int): string => {
  switch os {
  | 0 => "FAT filesystem"
  | 1 => "Amiga"
  | 2 => "VMS"
  | 3 => "Unix"
  | 4 => "VM/CMS"
  | 5 => "Atari TOS"
  | 6 => "HPFS filesystem"
  | 7 => "Macintosh"
  | 8 => "Z-System"
  | 9 => "CP/M"
  | 10 => "TOPS-20"
  | 11 => "NTFS filesystem"
  | 12 => "QDOS"
  | 13 => "Acorn RISCOS"
  | _ => "unknown"
  }
}

/** TAR header information */
type tarHeaderInfo = {
  nameLen: int,
  size: int,
  typeflag: int,
  isEnd: bool,
  isUstar: bool,
}

/** Get the entry type name for a TAR file */
let tarTypeName = (typeflag: int): string => {
  switch typeflag {
  | 0 | 48 => "regular file" // 0 or '0'
  | 49 => "hard link" // '1'
  | 50 => "symbolic link" // '2'
  | 51 => "character device" // '3'
  | 52 => "block device" // '4'
  | 53 => "directory" // '5'
  | 54 => "FIFO" // '6'
  | 55 => "contiguous file" // '7'
  | 120 => "extended header" // 'x'
  | 103 => "global extended header" // 'g'
  | _ => "unknown"
  }
}

/** Check if a TAR entry is a regular file */
let tarIsRegularFile = (info: tarHeaderInfo): bool => {
  info.typeflag == 0 || info.typeflag == 48 // 0 or '0'
}

/** Check if a TAR entry is a directory */
let tarIsDirectory = (info: tarHeaderInfo): bool => {
  info.typeflag == 53 // '5'
}

/** Archive error to string representation */
let archiveErrorToString = (error: archiveError): string => {
  switch error {
  | UnknownFormat => "Unknown archive format"
  | BufferTooSmall => "Buffer is too small"
  | InvalidHeader => "Invalid archive header"
  | CorruptedArchive => "Archive is corrupted"
  }
}

/** Confidence to string representation */
let confidenceToString = (conf: confidence): string => {
  switch conf {
  | High => "high"
  | Medium => "medium"
  | Low => "low"
  | None => "none"
  }
}
