// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeDocker - Docker image and container name validation that cannot crash.
 *
 * Provides validation for Docker image references, container names,
 * registry URLs, and related identifiers per OCI distribution spec.
 */

/** Maximum lengths per Docker/OCI specifications */
let maxImageNameLength = 128
let maxTagLength = 128
let maxContainerNameLength = 63
let maxRepositoryLength = 256

/** Error types for Docker operations */
type dockerError =
  | InvalidImageName
  | InvalidTag
  | InvalidDigest
  | InvalidRegistry
  | InvalidContainerName
  | InvalidRepository
  | InvalidNamespace
  | NameTooLong

/** Parsed Docker image reference */
type imageReference = {
  registry: option<string>,
  namespace: option<string>,
  repository: string,
  tag: option<string>,
  digest: option<string>,
}

/** Common Docker registries */
module Registry = {
  let dockerHub = "docker.io"
  let gcr = "gcr.io"
  let ghcr = "ghcr.io"
  let ecrPublic = "public.ecr.aws"
  let quay = "quay.io"
  let acr = "azurecr.io"
}

/** Check if a character is lowercase alphanumeric */
let isLowerAlnum = (char: string): bool => {
  let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
  (code >= 48 && code <= 57) || (code >= 97 && code <= 122)
}

/** Check if a character is alphanumeric */
let isAlnum = (char: string): bool => {
  let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
  (code >= 48 && code <= 57) || (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
}

/** Validate a Docker image name component (repository name) */
let isValidRepositoryName = (name: string): bool => {
  let length = Js.String2.length(name)
  if length == 0 || length > maxRepositoryLength {
    false
  } else {
    let firstChar = Js.String2.charAt(name, 0)
    // Must start with lowercase alphanumeric
    if !isLowerAlnum(firstChar) {
      false
    } else {
      let valid = ref(true)
      let prevSeparator = ref(false)

      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(name, i)
          let isSeparator = char == "." || char == "_" || char == "-"
          let isAlnumLower = isLowerAlnum(char)

          if !isAlnumLower && !isSeparator {
            valid := false
          } else if isSeparator && prevSeparator.contents {
            // No consecutive separators
            valid := false
          }

          prevSeparator := isSeparator
        }
      }

      // Must not end with separator
      if valid.contents {
        let lastChar = Js.String2.charAt(name, length - 1)
        if lastChar == "." || lastChar == "_" || lastChar == "-" {
          valid := false
        }
      }

      valid.contents
    }
  }
}

/** Validate a Docker image tag */
let isValidTag = (tag: string): bool => {
  let length = Js.String2.length(tag)
  if length == 0 || length > maxTagLength {
    false
  } else {
    let firstChar = Js.String2.charAt(tag, 0)
    // Must start with alphanumeric or underscore
    if !isAlnum(firstChar) && firstChar != "_" {
      false
    } else {
      let valid = ref(true)
      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(tag, i)
          if !isAlnum(char) && char != "_" && char != "-" && char != "." {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Validate a Docker image digest */
let isValidDigest = (digest: string): bool => {
  // Format: algorithm:hex
  switch Js.String2.indexOf(digest, ":") {
  | -1 => false
  | colonPos =>
    let algorithm = Js.String2.slice(digest, ~from=0, ~to_=colonPos)
    let hash = Js.String2.sliceToEnd(digest, ~from=colonPos + 1)

    // Validate algorithm (sha256, sha384, sha512)
    let validAlgorithm = algorithm == "sha256" || algorithm == "sha384" || algorithm == "sha512"

    if !validAlgorithm {
      false
    } else {
      // Validate hash length
      let expectedLen = switch algorithm {
      | "sha256" => 64
      | "sha384" => 96
      | _ => 128 // sha512
      }

      let hashLen = Js.String2.length(hash)
      if hashLen != expectedLen {
        false
      } else {
        // Validate hex characters
        let valid = ref(true)
        for i in 0 to hashLen - 1 {
          if valid.contents {
            let char = Js.String2.charAt(hash, i)
            let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
            let isHex =
              (code >= 48 && code <= 57) ||
              (code >= 65 && code <= 70) ||
              (code >= 97 && code <= 102)
            if !isHex {
              valid := false
            }
          }
        }
        valid.contents
      }
    }
  }
}

/** Validate a Docker container name */
let isValidContainerName = (name: string): bool => {
  let length = Js.String2.length(name)
  if length == 0 || length > maxContainerNameLength {
    false
  } else {
    let firstChar = Js.String2.charAt(name, 0)
    // Must start with alphanumeric
    if !isAlnum(firstChar) {
      false
    } else {
      let valid = ref(true)
      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(name, i)
          if !isAlnum(char) && char != "_" && char != "." && char != "-" {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Validate a registry hostname */
let isValidRegistry = (registry: string): bool => {
  let length = Js.String2.length(registry)
  if length == 0 {
    false
  } else {
    // Check for port number
    let hostname = switch Js.String2.lastIndexOf(registry, ":") {
    | -1 => registry
    | colonPos =>
      let portStr = Js.String2.sliceToEnd(registry, ~from=colonPos + 1)
      // Validate port number
      switch Belt.Int.fromString(portStr) {
      | None => ""
      | Some(port) =>
        if port <= 0 || port > 65535 {
          ""
        } else {
          Js.String2.slice(registry, ~from=0, ~to_=colonPos)
        }
      }
    }

    if Js.String2.length(hostname) == 0 {
      false
    } else {
      // Must contain a dot or be localhost
      if hostname != "localhost" && !Js.String2.includes(hostname, ".") {
        false
      } else {
        // Validate hostname characters
        let valid = ref(true)
        let prevDot = ref(false)
        let hostnameLen = Js.String2.length(hostname)

        for i in 0 to hostnameLen - 1 {
          if valid.contents {
            let char = Js.String2.charAt(hostname, i)
            let isDot = char == "."
            let validChar = isAlnum(char) || char == "-" || char == "."

            if !validChar {
              valid := false
            } else if isDot && prevDot.contents {
              valid := false
            }

            prevDot := isDot
          }
        }

        // Must not start or end with dot or hyphen
        if valid.contents {
          let firstChar = Js.String2.charAt(hostname, 0)
          let lastChar = Js.String2.charAt(hostname, hostnameLen - 1)
          if firstChar == "." || firstChar == "-" || lastChar == "." || lastChar == "-" {
            valid := false
          }
        }

        valid.contents
      }
    }
  }
}

/** Validate a namespace (organization) name */
let isValidNamespace = (namespace: string): bool => {
  let length = Js.String2.length(namespace)
  if length == 0 || length > 255 {
    false
  } else {
    let firstChar = Js.String2.charAt(namespace, 0)
    // Must start with alphanumeric
    if !isAlnum(firstChar) {
      false
    } else {
      let valid = ref(true)
      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(namespace, i)
          if !isAlnum(char) && char != "_" && char != "-" {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Parse a Docker image reference string */
let parseImageReference = (input: string): result<imageReference, dockerError> => {
  let length = Js.String2.length(input)
  if length == 0 {
    Error(InvalidImageName)
  } else {
    let remaining = ref(input)
    let digest = ref(None)
    let tag = ref(None)

    // Check for digest (@sha256:...)
    switch Js.String2.indexOf(remaining.contents, "@") {
    | -1 => ()
    | atPos =>
      let digestStr = Js.String2.sliceToEnd(remaining.contents, ~from=atPos + 1)
      if !isValidDigest(digestStr) {
        ()
      } else {
        digest := Some(digestStr)
        remaining := Js.String2.slice(remaining.contents, ~from=0, ~to_=atPos)
      }
    }

    // Check for tag (:tag) - only if no digest found
    if Belt.Option.isNone(digest.contents) {
      switch Js.String2.lastIndexOf(remaining.contents, ":") {
      | -1 => ()
      | colonPos =>
        let afterColon = Js.String2.sliceToEnd(remaining.contents, ~from=colonPos + 1)
        // Make sure it's not a port number (would be followed by /)
        if !Js.String2.includes(afterColon, "/") {
          if Js.String2.length(afterColon) > 0 && isValidTag(afterColon) {
            tag := Some(afterColon)
            remaining := Js.String2.slice(remaining.contents, ~from=0, ~to_=colonPos)
          }
        }
      }
    }

    // Split by /
    let parts = Js.String2.split(remaining.contents, "/")
    let partCount = Belt.Array.length(parts)

    if partCount == 0 {
      Error(InvalidImageName)
    } else if partCount == 1 {
      // Just repository (e.g., "nginx")
      let repository = Belt.Array.getUnsafe(parts, 0)
      if !isValidRepositoryName(repository) {
        Error(InvalidRepository)
      } else {
        Ok({
          registry: None,
          namespace: None,
          repository,
          tag: tag.contents,
          digest: digest.contents,
        })
      }
    } else if partCount == 2 {
      let first = Belt.Array.getUnsafe(parts, 0)
      let second = Belt.Array.getUnsafe(parts, 1)

      // Check if first looks like a registry (has dot or port)
      if (
        Js.String2.includes(first, ".") || Js.String2.includes(first, ":") || first == "localhost"
      ) {
        if !isValidRegistry(first) {
          Error(InvalidRegistry)
        } else if !isValidRepositoryName(second) {
          Error(InvalidRepository)
        } else {
          Ok({
            registry: Some(first),
            namespace: None,
            repository: second,
            tag: tag.contents,
            digest: digest.contents,
          })
        }
      } else {
        // namespace/repo
        if !isValidNamespace(first) {
          Error(InvalidNamespace)
        } else if !isValidRepositoryName(second) {
          Error(InvalidRepository)
        } else {
          Ok({
            registry: None,
            namespace: Some(first),
            repository: second,
            tag: tag.contents,
            digest: digest.contents,
          })
        }
      }
    } else {
      // registry/namespace/repo or more
      let first = Belt.Array.getUnsafe(parts, 0)
      let registry = ref(None)
      let namespace = ref(None)

      // First part should be registry
      if isValidRegistry(first) || first == "docker.io" {
        registry := Some(first)
        if partCount >= 3 {
          let ns = Belt.Array.getUnsafe(parts, 1)
          if !isValidNamespace(ns) {
            ()
          } else {
            namespace := Some(ns)
          }
        }
      } else {
        // Could be namespace/...
        if isValidNamespace(first) {
          namespace := Some(first)
        }
      }

      // Last part is repository
      let repository = Belt.Array.getUnsafe(parts, partCount - 1)
      if !isValidRepositoryName(repository) {
        Error(InvalidRepository)
      } else {
        Ok({
          registry: registry.contents,
          namespace: namespace.contents,
          repository,
          tag: tag.contents,
          digest: digest.contents,
        })
      }
    }
  }
}

/** Check if a string is a valid Docker image reference */
let isValidImageReference = (input: string): bool => {
  switch parseImageReference(input) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Get the full image name without tag or digest */
let getFullName = (imgRef: imageReference): string => {
  let parts = ref([])

  switch imgRef.registry {
  | Some(reg) => parts := Belt.Array.concat(parts.contents, [reg])
  | None => ()
  }

  switch imgRef.namespace {
  | Some(ns) => parts := Belt.Array.concat(parts.contents, [ns])
  | None => ()
  }

  parts := Belt.Array.concat(parts.contents, [imgRef.repository])

  Js.Array2.joinWith(parts.contents, "/")
}

/** Get the full reference including tag or digest */
let getFullReference = (imgRef: imageReference): string => {
  let name = getFullName(imgRef)

  switch imgRef.digest {
  | Some(dig) => name ++ "@" ++ dig
  | None =>
    switch imgRef.tag {
    | Some(t) => name ++ ":" ++ t
    | None => name
    }
  }
}

/** Check if this is an official Docker Hub image (library/) */
let isOfficial = (imgRef: imageReference): bool => {
  switch imgRef.registry {
  | Some(reg) if reg != "docker.io" && reg != "index.docker.io" => false
  | _ =>
    switch imgRef.namespace {
    | None => true
    | Some(ns) => ns == "library"
    }
  }
}

/** Check if the image has a specific tag */
let hasTag = (imgRef: imageReference): bool => {
  Belt.Option.isSome(imgRef.tag)
}

/** Check if the image is pinned by digest */
let isPinned = (imgRef: imageReference): bool => {
  Belt.Option.isSome(imgRef.digest)
}

/** Check if using latest tag (explicit or implicit) */
let isLatest = (imgRef: imageReference): bool => {
  if Belt.Option.isSome(imgRef.digest) {
    false
  } else {
    switch imgRef.tag {
    | Some(t) => t == "latest"
    | None => true // No tag = implicit latest
    }
  }
}

/** Normalize an image reference (add docker.io and library if needed) */
let normalizeImageReference = (input: string): result<string, dockerError> => {
  switch parseImageReference(input) {
  | Error(e) => Error(e)
  | Ok(ref) =>
    let registry = switch ref.registry {
    | Some(r) => r
    | None => "docker.io"
    }
    let namespace = switch ref.namespace {
    | Some(ns) => ns
    | None => "library"
    }
    let tagOrDigest = switch ref.digest {
    | Some(dig) => "@" ++ dig
    | None =>
      switch ref.tag {
      | Some(t) => ":" ++ t
      | None => ":latest"
      }
    }

    Ok(`${registry}/${namespace}/${ref.repository}${tagOrDigest}`)
  }
}

/** Docker error to string representation */
let dockerErrorToString = (error: dockerError): string => {
  switch error {
  | InvalidImageName => "Invalid image name"
  | InvalidTag => "Invalid tag"
  | InvalidDigest => "Invalid digest"
  | InvalidRegistry => "Invalid registry"
  | InvalidContainerName => "Invalid container name"
  | InvalidRepository => "Invalid repository name"
  | InvalidNamespace => "Invalid namespace"
  | NameTooLong => "Name is too long"
  }
}
