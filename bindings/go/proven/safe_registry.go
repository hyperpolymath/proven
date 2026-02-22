// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeRegistry provides OCI image reference parsing via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ImageRef represents a parsed OCI image reference.
type ImageRef struct {
	Registry   string
	Repository string
	Tag        string
	Digest     string
}

// RegistryParse parses an OCI image reference string
// (e.g., "ghcr.io/user/repo:v1.0").
func RegistryParse(input string) (*ImageRef, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_registry_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	ref := &ImageRef{}
	if result.reference.registry != nil {
		ref.Registry = C.GoStringN(result.reference.registry, C.int(result.reference.registry_len))
	}
	if result.reference.repository != nil {
		ref.Repository = C.GoStringN(result.reference.repository, C.int(result.reference.repository_len))
	}
	if result.reference.tag != nil {
		ref.Tag = C.GoStringN(result.reference.tag, C.int(result.reference.tag_len))
	}
	if result.reference.digest != nil {
		ref.Digest = C.GoStringN(result.reference.digest, C.int(result.reference.digest_len))
	}

	return ref, nil
}

// RegistryToString converts an image reference back to a string.
func RegistryToString(ref *ImageRef) (string, error) {
	cRef := C.ImageReference{}

	if ref.Registry != "" {
		regCS := C.CString(ref.Registry)
		defer unsafeFree(regCS)
		cRef.registry = regCS
		cRef.registry_len = C.size_t(len(ref.Registry))
	}
	if ref.Repository != "" {
		repoCS := C.CString(ref.Repository)
		defer unsafeFree(repoCS)
		cRef.repository = repoCS
		cRef.repository_len = C.size_t(len(ref.Repository))
	}
	if ref.Tag != "" {
		tagCS := C.CString(ref.Tag)
		defer unsafeFree(tagCS)
		cRef.tag = tagCS
		cRef.tag_len = C.size_t(len(ref.Tag))
	}
	if ref.Digest != "" {
		digestCS := C.CString(ref.Digest)
		defer unsafeFree(digestCS)
		cRef.digest = digestCS
		cRef.digest_len = C.size_t(len(ref.Digest))
	}

	return goStringResult(C.proven_registry_to_string(&cRef))
}

// RegistryHasRegistry checks whether an image reference has an explicit
// registry hostname.
func RegistryHasRegistry(ref *ImageRef) (bool, error) {
	cRef := C.ImageReference{}

	if ref.Registry != "" {
		regCS := C.CString(ref.Registry)
		defer unsafeFree(regCS)
		cRef.registry = regCS
		cRef.registry_len = C.size_t(len(ref.Registry))
	}
	if ref.Repository != "" {
		repoCS := C.CString(ref.Repository)
		defer unsafeFree(repoCS)
		cRef.repository = repoCS
		cRef.repository_len = C.size_t(len(ref.Repository))
	}

	result := C.proven_registry_has_registry(&cRef)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}
