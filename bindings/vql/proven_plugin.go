// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// proven_plugin.go - Velociraptor VQL plugin calling libproven via CGo
//
// This Go plugin exposes formally verified functions from libproven as
// VQL (Velociraptor Query Language) functions. ALL computation is performed
// in Idris 2 via the Zig FFI bridge. This file contains ONLY CGo marshaling
// to the libproven C ABI. No algorithms are reimplemented here.
//
// Build:
//   CGO_LDFLAGS="-L/path/to/libproven -lproven" go build -buildmode=plugin proven_plugin.go
//
// Deploy:
//   Copy the .so plugin to Velociraptor's plugin directory.

package main

/*
#cgo LDFLAGS: -lproven
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Result structs matching the libproven C ABI.

typedef struct {
    int32_t status;
    int64_t value;
} IntResult;

typedef struct {
    int32_t status;
    int32_t value;
} BoolResult;

typedef struct {
    int32_t status;
    char*   ptr;
    size_t  len;
} StringResult;

typedef struct {
    int32_t status;
    double  value;
} FloatResult;

// Lifecycle
extern int32_t proven_init(void);
extern void    proven_deinit(void);
extern bool    proven_is_initialized(void);
extern void    proven_free_string(char* ptr);

// SafeMath
extern IntResult proven_math_add_checked(int64_t a, int64_t b);
extern IntResult proven_math_sub_checked(int64_t a, int64_t b);
extern IntResult proven_math_mul_checked(int64_t a, int64_t b);
extern IntResult proven_math_div(int64_t a, int64_t b);
extern IntResult proven_math_mod(int64_t a, int64_t b);
extern IntResult proven_math_abs_safe(int64_t n);
extern int64_t   proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
extern IntResult proven_math_pow_checked(int64_t base, uint32_t exp);

// SafeString
extern BoolResult   proven_string_is_valid_utf8(const char* ptr, size_t len);
extern StringResult proven_string_escape_sql(const char* ptr, size_t len);
extern StringResult proven_string_escape_html(const char* ptr, size_t len);
extern StringResult proven_string_escape_js(const char* ptr, size_t len);

// SafeEmail
extern BoolResult proven_email_is_valid(const char* ptr, size_t len);

// SafeUrl
extern void* proven_url_parse(const char* ptr, size_t len);
extern void  proven_url_free(void* components);

// SafeCrypto
extern BoolResult proven_crypto_constant_time_eq(
    const char* a, size_t a_len, const char* b, size_t b_len);
extern int32_t proven_crypto_random_bytes(char* buf, size_t len);
extern StringResult proven_hex_encode(const char* ptr, size_t len, bool uppercase);

// SafeJson
extern BoolResult proven_json_is_valid(const char* ptr, size_t len);
extern int32_t    proven_json_get_type(const char* ptr, size_t len);

// SafeChecksum
extern IntResult  proven_checksum_crc32(const char* ptr, size_t len);
extern BoolResult proven_checksum_verify_crc32(const char* ptr, size_t len, uint32_t expected);
*/
import "C"

import (
	"context"
	"sync"
	"unsafe"

	"www.velocidex.com/golang/vfilter"
	vql_subsystem "www.velocidex.com/golang/velociraptor/vql"
	"www.velocidex.com/golang/vfilter/arg_parser"
)

// initOnce ensures proven_init() is called exactly once.
var initOnce sync.Once

// ensureInit initializes the proven runtime exactly once.
func ensureInit() {
	initOnce.Do(func() {
		C.proven_init()
	})
}

// ============================================================================
// SafeMath VQL Functions
// ============================================================================

// ProvenMathAddArgs holds arguments for proven_math_add.
type ProvenMathAddArgs struct {
	A int64 `vfilter:"required,field=a,doc=First operand"`
	B int64 `vfilter:"required,field=b,doc=Second operand"`
}

// ProvenMathAdd implements checked addition via libproven.
type ProvenMathAdd struct{}

func (self ProvenMathAdd) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathAddArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_add: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_add_checked(C.int64_t(arg.A), C.int64_t(arg.B))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathAdd) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_add",
		Doc:     "Checked addition via libproven. Returns null on overflow.",
		ArgType: type_map.AddType(scope, &ProvenMathAddArgs{}),
	}
}

// ProvenMathSubArgs holds arguments for proven_math_sub.
type ProvenMathSubArgs struct {
	A int64 `vfilter:"required,field=a,doc=First operand"`
	B int64 `vfilter:"required,field=b,doc=Second operand"`
}

// ProvenMathSub implements checked subtraction via libproven.
type ProvenMathSub struct{}

func (self ProvenMathSub) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathSubArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_sub: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_sub_checked(C.int64_t(arg.A), C.int64_t(arg.B))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathSub) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_sub",
		Doc:     "Checked subtraction via libproven. Returns null on underflow.",
		ArgType: type_map.AddType(scope, &ProvenMathSubArgs{}),
	}
}

// ProvenMathMulArgs holds arguments for proven_math_mul.
type ProvenMathMulArgs struct {
	A int64 `vfilter:"required,field=a,doc=First operand"`
	B int64 `vfilter:"required,field=b,doc=Second operand"`
}

// ProvenMathMul implements checked multiplication via libproven.
type ProvenMathMul struct{}

func (self ProvenMathMul) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathMulArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_mul: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_mul_checked(C.int64_t(arg.A), C.int64_t(arg.B))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathMul) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_mul",
		Doc:     "Checked multiplication via libproven. Returns null on overflow.",
		ArgType: type_map.AddType(scope, &ProvenMathMulArgs{}),
	}
}

// ProvenMathDivArgs holds arguments for proven_math_div.
type ProvenMathDivArgs struct {
	A int64 `vfilter:"required,field=a,doc=Numerator"`
	B int64 `vfilter:"required,field=b,doc=Denominator"`
}

// ProvenMathDiv implements safe division via libproven.
type ProvenMathDiv struct{}

func (self ProvenMathDiv) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathDivArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_div: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_div(C.int64_t(arg.A), C.int64_t(arg.B))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathDiv) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_div",
		Doc:     "Safe division via libproven. Returns null on division by zero.",
		ArgType: type_map.AddType(scope, &ProvenMathDivArgs{}),
	}
}

// ProvenMathModArgs holds arguments for proven_math_mod.
type ProvenMathModArgs struct {
	A int64 `vfilter:"required,field=a,doc=Numerator"`
	B int64 `vfilter:"required,field=b,doc=Denominator"`
}

// ProvenMathMod implements safe modulo via libproven.
type ProvenMathMod struct{}

func (self ProvenMathMod) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathModArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_mod: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_mod(C.int64_t(arg.A), C.int64_t(arg.B))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathMod) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_mod",
		Doc:     "Safe modulo via libproven. Returns null on zero divisor.",
		ArgType: type_map.AddType(scope, &ProvenMathModArgs{}),
	}
}

// ProvenMathAbsArgs holds arguments for proven_math_abs.
type ProvenMathAbsArgs struct {
	N int64 `vfilter:"required,field=n,doc=Input value"`
}

// ProvenMathAbs implements safe absolute value via libproven.
type ProvenMathAbs struct{}

func (self ProvenMathAbs) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathAbsArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_abs: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_abs_safe(C.int64_t(arg.N))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathAbs) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_abs",
		Doc:     "Safe absolute value via libproven. Returns null for INT64_MIN.",
		ArgType: type_map.AddType(scope, &ProvenMathAbsArgs{}),
	}
}

// ProvenMathClampArgs holds arguments for proven_math_clamp.
type ProvenMathClampArgs struct {
	Value int64 `vfilter:"required,field=value,doc=Value to clamp"`
	Lo    int64 `vfilter:"required,field=lo,doc=Lower bound"`
	Hi    int64 `vfilter:"required,field=hi,doc=Upper bound"`
}

// ProvenMathClamp implements value clamping via libproven.
type ProvenMathClamp struct{}

func (self ProvenMathClamp) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathClampArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_clamp: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_clamp(C.int64_t(arg.Lo), C.int64_t(arg.Hi), C.int64_t(arg.Value))
	return int64(result)
}

func (self ProvenMathClamp) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_clamp",
		Doc:     "Clamp value to [lo, hi] via libproven. Always succeeds.",
		ArgType: type_map.AddType(scope, &ProvenMathClampArgs{}),
	}
}

// ProvenMathPowArgs holds arguments for proven_math_pow.
type ProvenMathPowArgs struct {
	Base int64  `vfilter:"required,field=base,doc=Base value"`
	Exp  uint32 `vfilter:"required,field=exp,doc=Exponent"`
}

// ProvenMathPow implements checked exponentiation via libproven.
type ProvenMathPow struct{}

func (self ProvenMathPow) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenMathPowArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_math_pow: %v", err)
		return vfilter.Null{}
	}
	result := C.proven_math_pow_checked(C.int64_t(arg.Base), C.uint32_t(arg.Exp))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenMathPow) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_math_pow",
		Doc:     "Checked exponentiation via libproven. Returns null on overflow.",
		ArgType: type_map.AddType(scope, &ProvenMathPowArgs{}),
	}
}

// ============================================================================
// Validation VQL Functions
// ============================================================================

// ProvenValidateStringArgs holds arguments for string-input validation functions.
type ProvenValidateStringArgs struct {
	Input string `vfilter:"required,field=input,doc=Input string to validate"`
}

// ProvenValidateEmail validates email addresses via libproven.
type ProvenValidateEmail struct{}

func (self ProvenValidateEmail) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_validate_email: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_email_is_valid(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return result.value != 0
}

func (self ProvenValidateEmail) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_validate_email",
		Doc:     "Validate email address (RFC 5321) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenValidateUrl validates URLs via libproven.
type ProvenValidateUrl struct{}

func (self ProvenValidateUrl) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_validate_url: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_url_parse(cStr, C.size_t(len(arg.Input)))
	if result == nil {
		return false
	}
	C.proven_url_free(result)
	return true
}

func (self ProvenValidateUrl) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_validate_url",
		Doc:     "Validate URL (RFC 3986) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenValidateJson validates JSON documents via libproven.
type ProvenValidateJson struct{}

func (self ProvenValidateJson) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_validate_json: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_json_is_valid(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return result.value != 0
}

func (self ProvenValidateJson) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_validate_json",
		Doc:     "Validate JSON document via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenValidateUtf8 validates UTF-8 encoding via libproven.
type ProvenValidateUtf8 struct{}

func (self ProvenValidateUtf8) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_validate_utf8: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_string_is_valid_utf8(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return result.value != 0
}

func (self ProvenValidateUtf8) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_validate_utf8",
		Doc:     "Validate UTF-8 encoding via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ============================================================================
// String Operation VQL Functions
// ============================================================================

// ProvenStringEscapeHtml escapes strings for HTML via libproven.
type ProvenStringEscapeHtml struct{}

func (self ProvenStringEscapeHtml) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_string_escape_html: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_string_escape_html(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 || result.ptr == nil {
		return vfilter.Null{}
	}
	goStr := C.GoStringN(result.ptr, C.int(result.len))
	C.proven_free_string(result.ptr)
	return goStr
}

func (self ProvenStringEscapeHtml) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_string_escape_html",
		Doc:     "Escape string for HTML (XSS prevention) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenStringEscapeSql escapes strings for SQL via libproven.
type ProvenStringEscapeSql struct{}

func (self ProvenStringEscapeSql) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_string_escape_sql: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_string_escape_sql(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 || result.ptr == nil {
		return vfilter.Null{}
	}
	goStr := C.GoStringN(result.ptr, C.int(result.len))
	C.proven_free_string(result.ptr)
	return goStr
}

func (self ProvenStringEscapeSql) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_string_escape_sql",
		Doc:     "Escape string for SQL (injection prevention) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenStringEscapeJs escapes strings for JavaScript via libproven.
type ProvenStringEscapeJs struct{}

func (self ProvenStringEscapeJs) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_string_escape_js: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_string_escape_js(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 || result.ptr == nil {
		return vfilter.Null{}
	}
	goStr := C.GoStringN(result.ptr, C.int(result.len))
	C.proven_free_string(result.ptr)
	return goStr
}

func (self ProvenStringEscapeJs) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_string_escape_js",
		Doc:     "Escape string for JavaScript (literal safety) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ============================================================================
// Crypto VQL Functions
// ============================================================================

// ProvenCryptoHexEncode hex-encodes input via libproven.
type ProvenCryptoHexEncode struct{}

func (self ProvenCryptoHexEncode) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_crypto_hex_encode: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_hex_encode(cStr, C.size_t(len(arg.Input)), C.bool(false))
	if result.status != 0 || result.ptr == nil {
		return vfilter.Null{}
	}
	goStr := C.GoStringN(result.ptr, C.int(result.len))
	C.proven_free_string(result.ptr)
	return goStr
}

func (self ProvenCryptoHexEncode) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_crypto_hex_encode",
		Doc:     "Hex-encode input bytes via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenCryptoCrc32 computes CRC32 checksum via libproven.
type ProvenCryptoCrc32 struct{}

func (self ProvenCryptoCrc32) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenValidateStringArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_crypto_crc32: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_checksum_crc32(cStr, C.size_t(len(arg.Input)))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return int64(result.value)
}

func (self ProvenCryptoCrc32) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_crypto_crc32",
		Doc:     "Compute CRC32 checksum via libproven.",
		ArgType: type_map.AddType(scope, &ProvenValidateStringArgs{}),
	}
}

// ProvenCryptoVerifyCrc32Args holds arguments for CRC32 verification.
type ProvenCryptoVerifyCrc32Args struct {
	Input    string `vfilter:"required,field=input,doc=Input string"`
	Expected uint32 `vfilter:"required,field=expected,doc=Expected CRC32 value"`
}

// ProvenCryptoVerifyCrc32 verifies CRC32 checksum via libproven.
type ProvenCryptoVerifyCrc32 struct{}

func (self ProvenCryptoVerifyCrc32) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenCryptoVerifyCrc32Args{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_crypto_verify_crc32: %v", err)
		return vfilter.Null{}
	}
	cStr := C.CString(arg.Input)
	defer C.free(unsafe.Pointer(cStr))
	result := C.proven_checksum_verify_crc32(cStr, C.size_t(len(arg.Input)), C.uint32_t(arg.Expected))
	if result.status != 0 {
		return vfilter.Null{}
	}
	return result.value != 0
}

func (self ProvenCryptoVerifyCrc32) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_crypto_verify_crc32",
		Doc:     "Verify CRC32 checksum via libproven.",
		ArgType: type_map.AddType(scope, &ProvenCryptoVerifyCrc32Args{}),
	}
}

// ProvenCryptoCtEqArgs holds arguments for constant-time comparison.
type ProvenCryptoCtEqArgs struct {
	A string `vfilter:"required,field=a,doc=First byte sequence"`
	B string `vfilter:"required,field=b,doc=Second byte sequence"`
}

// ProvenCryptoCtEq performs constant-time comparison via libproven.
type ProvenCryptoCtEq struct{}

func (self ProvenCryptoCtEq) Call(ctx context.Context, scope vfilter.Scope, args *vfilter.Dict) vfilter.Any {
	ensureInit()
	arg := &ProvenCryptoCtEqArgs{}
	err := arg_parser.ExtractArgs(scope, args, arg)
	if err != nil {
		scope.Log("proven_crypto_ct_eq: %v", err)
		return vfilter.Null{}
	}
	cStrA := C.CString(arg.A)
	defer C.free(unsafe.Pointer(cStrA))
	cStrB := C.CString(arg.B)
	defer C.free(unsafe.Pointer(cStrB))
	result := C.proven_crypto_constant_time_eq(
		cStrA, C.size_t(len(arg.A)),
		cStrB, C.size_t(len(arg.B)),
	)
	if result.status != 0 {
		return vfilter.Null{}
	}
	return result.value != 0
}

func (self ProvenCryptoCtEq) Info(scope vfilter.Scope, type_map *vfilter.TypeMap) *vfilter.FunctionInfo {
	return &vfilter.FunctionInfo{
		Name:    "proven_crypto_ct_eq",
		Doc:     "Constant-time byte comparison (timing-attack resistant) via libproven.",
		ArgType: type_map.AddType(scope, &ProvenCryptoCtEqArgs{}),
	}
}

// ============================================================================
// Plugin Registration
// ============================================================================

func init() {
	vql_subsystem.RegisterFunction(&ProvenMathAdd{})
	vql_subsystem.RegisterFunction(&ProvenMathSub{})
	vql_subsystem.RegisterFunction(&ProvenMathMul{})
	vql_subsystem.RegisterFunction(&ProvenMathDiv{})
	vql_subsystem.RegisterFunction(&ProvenMathMod{})
	vql_subsystem.RegisterFunction(&ProvenMathAbs{})
	vql_subsystem.RegisterFunction(&ProvenMathClamp{})
	vql_subsystem.RegisterFunction(&ProvenMathPow{})
	vql_subsystem.RegisterFunction(&ProvenValidateEmail{})
	vql_subsystem.RegisterFunction(&ProvenValidateUrl{})
	vql_subsystem.RegisterFunction(&ProvenValidateJson{})
	vql_subsystem.RegisterFunction(&ProvenValidateUtf8{})
	vql_subsystem.RegisterFunction(&ProvenStringEscapeHtml{})
	vql_subsystem.RegisterFunction(&ProvenStringEscapeSql{})
	vql_subsystem.RegisterFunction(&ProvenStringEscapeJs{})
	vql_subsystem.RegisterFunction(&ProvenCryptoHexEncode{})
	vql_subsystem.RegisterFunction(&ProvenCryptoCrc32{})
	vql_subsystem.RegisterFunction(&ProvenCryptoVerifyCrc32{})
	vql_subsystem.RegisterFunction(&ProvenCryptoCtEq{})
}
