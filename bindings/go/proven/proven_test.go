// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// Tests for the Proven Go FFI bindings.
// These tests call the Idris 2 formally-verified library via CGO.
// They require libproven to be compiled and available in the library path.

package proven

import (
	"math"
	"os"
	"testing"
)

// TestMain initializes and deinitializes the Proven runtime for all tests.
func TestMain(m *testing.M) {
	if err := Init(); err != nil {
		// If the library is not available, skip all tests gracefully.
		os.Exit(0)
	}
	code := m.Run()
	Deinit()
	os.Exit(code)
}

// ---------------------------------------------------------------------------
// SafeMath Tests
// ---------------------------------------------------------------------------

func TestSafeDiv(t *testing.T) {
	result, err := SafeDiv(10, 2)
	if err != nil || result != 5 {
		t.Errorf("SafeDiv(10, 2) = %d, %v; want 5, nil", result, err)
	}
	_, err = SafeDiv(10, 0)
	if err == nil {
		t.Error("SafeDiv(10, 0) should return an error")
	}
}

func TestSafeMod(t *testing.T) {
	result, err := SafeMod(10, 3)
	if err != nil || result != 1 {
		t.Errorf("SafeMod(10, 3) = %d, %v; want 1, nil", result, err)
	}
	_, err = SafeMod(10, 0)
	if err == nil {
		t.Error("SafeMod(10, 0) should return an error")
	}
}

func TestSafeAdd(t *testing.T) {
	result, err := SafeAdd(1, 2)
	if err != nil || result != 3 {
		t.Errorf("SafeAdd(1, 2) = %d, %v; want 3, nil", result, err)
	}
	_, err = SafeAdd(math.MaxInt64, 1)
	if err == nil {
		t.Error("SafeAdd(MaxInt64, 1) should return an error (overflow)")
	}
}

func TestSafeSub(t *testing.T) {
	result, err := SafeSub(5, 3)
	if err != nil || result != 2 {
		t.Errorf("SafeSub(5, 3) = %d, %v; want 2, nil", result, err)
	}
	_, err = SafeSub(math.MinInt64, 1)
	if err == nil {
		t.Error("SafeSub(MinInt64, 1) should return an error (overflow)")
	}
}

func TestSafeMul(t *testing.T) {
	result, err := SafeMul(3, 4)
	if err != nil || result != 12 {
		t.Errorf("SafeMul(3, 4) = %d, %v; want 12, nil", result, err)
	}
	_, err = SafeMul(math.MaxInt64, 2)
	if err == nil {
		t.Error("SafeMul(MaxInt64, 2) should return an error (overflow)")
	}
}

func TestClamp(t *testing.T) {
	if result := Clamp(0, 10, 5); result != 5 {
		t.Errorf("Clamp(0, 10, 5) = %d; want 5", result)
	}
	if result := Clamp(0, 10, -5); result != 0 {
		t.Errorf("Clamp(0, 10, -5) = %d; want 0", result)
	}
	if result := Clamp(0, 10, 15); result != 10 {
		t.Errorf("Clamp(0, 10, 15) = %d; want 10", result)
	}
}

// ---------------------------------------------------------------------------
// SafeString Tests
// ---------------------------------------------------------------------------

func TestEscapeHTML(t *testing.T) {
	result, err := EscapeHTML("<script>")
	if err != nil {
		t.Fatalf("EscapeHTML(<script>) error: %v", err)
	}
	if result != "&lt;script&gt;" {
		t.Errorf("EscapeHTML(<script>) = %s; want &lt;script&gt;", result)
	}
}

func TestEscapeSQL(t *testing.T) {
	result, err := EscapeSQL("it's")
	if err != nil {
		t.Fatalf("EscapeSQL(it's) error: %v", err)
	}
	if result != "it''s" {
		t.Errorf("EscapeSQL(it's) = %s; want it''s", result)
	}
}

func TestEscapeJS(t *testing.T) {
	result, err := EscapeJS("line\nbreak")
	if err != nil {
		t.Fatalf("EscapeJS error: %v", err)
	}
	if result != "line\\nbreak" {
		t.Errorf("EscapeJS(line\\nbreak) = %s; want line\\nbreak", result)
	}
}

// ---------------------------------------------------------------------------
// SafePath Tests
// ---------------------------------------------------------------------------

func TestHasTraversal(t *testing.T) {
	result, err := HasTraversal("../etc/passwd")
	if err != nil {
		t.Fatalf("HasTraversal(../etc/passwd) error: %v", err)
	}
	if !result {
		t.Error("HasTraversal(../etc/passwd) should be true")
	}
	result, err = HasTraversal("normal/path")
	if err != nil {
		t.Fatalf("HasTraversal(normal/path) error: %v", err)
	}
	if result {
		t.Error("HasTraversal(normal/path) should be false")
	}
}

// ---------------------------------------------------------------------------
// SafeEmail Tests
// ---------------------------------------------------------------------------

func TestIsValidEmail(t *testing.T) {
	result, err := IsValidEmail("user@example.com")
	if err != nil {
		t.Fatalf("IsValidEmail(user@example.com) error: %v", err)
	}
	if !result {
		t.Error("IsValidEmail(user@example.com) should be true")
	}
	result, err = IsValidEmail("not-an-email")
	if err != nil {
		t.Fatalf("IsValidEmail(not-an-email) error: %v", err)
	}
	if result {
		t.Error("IsValidEmail(not-an-email) should be false")
	}
}

// ---------------------------------------------------------------------------
// SafeFloat Tests
// ---------------------------------------------------------------------------

func TestFloatDiv(t *testing.T) {
	result, err := FloatDiv(10.0, 2.0)
	if err != nil || result != 5.0 {
		t.Errorf("FloatDiv(10, 2) = %f, %v; want 5.0, nil", result, err)
	}
	_, err = FloatDiv(10.0, 0.0)
	if err == nil {
		t.Error("FloatDiv(10, 0) should return an error")
	}
}

func TestFloatIsFinite(t *testing.T) {
	if !FloatIsFinite(1.0) {
		t.Error("FloatIsFinite(1.0) should be true")
	}
	if FloatIsFinite(math.Inf(1)) {
		t.Error("FloatIsFinite(+Inf) should be false")
	}
	if FloatIsFinite(math.NaN()) {
		t.Error("FloatIsFinite(NaN) should be false")
	}
}

// ---------------------------------------------------------------------------
// SafeJson Tests
// ---------------------------------------------------------------------------

func TestJSONIsValid(t *testing.T) {
	result, err := JSONIsValid(`{"key": "value"}`)
	if err != nil {
		t.Fatalf("JSONIsValid error: %v", err)
	}
	if !result {
		t.Error("JSONIsValid({\"key\": \"value\"}) should be true")
	}
	result, err = JSONIsValid("not json")
	if err != nil {
		t.Fatalf("JSONIsValid error: %v", err)
	}
	if result {
		t.Error("JSONIsValid(not json) should be false")
	}
}

// ---------------------------------------------------------------------------
// SafeProbability Tests
// ---------------------------------------------------------------------------

func TestProbabilityCreate(t *testing.T) {
	p := ProbabilityCreate(0.5)
	if p != 0.5 {
		t.Errorf("ProbabilityCreate(0.5) = %f; want 0.5", p)
	}
	// Values outside [0,1] should be clamped.
	p = ProbabilityCreate(1.5)
	if p != 1.0 {
		t.Errorf("ProbabilityCreate(1.5) = %f; want 1.0", p)
	}
	p = ProbabilityCreate(-0.5)
	if p != 0.0 {
		t.Errorf("ProbabilityCreate(-0.5) = %f; want 0.0", p)
	}
}

// ---------------------------------------------------------------------------
// SafeAngle Tests
// ---------------------------------------------------------------------------

func TestAngleDegToRad(t *testing.T) {
	result := AngleDegToRad(180.0)
	if math.Abs(result-math.Pi) > 1e-10 {
		t.Errorf("AngleDegToRad(180) = %f; want %f", result, math.Pi)
	}
}

func TestAngleNormalizeDegrees(t *testing.T) {
	result := AngleNormalizeDegrees(450.0)
	if math.Abs(result-90.0) > 1e-10 {
		t.Errorf("AngleNormalizeDegrees(450) = %f; want 90.0", result)
	}
}

// ---------------------------------------------------------------------------
// Version Tests
// ---------------------------------------------------------------------------

func TestVersionInfo(t *testing.T) {
	if !IsInitialized() {
		t.Error("IsInitialized() should be true after Init()")
	}
	if ModuleCount() == 0 {
		t.Error("ModuleCount() should be > 0")
	}
}
