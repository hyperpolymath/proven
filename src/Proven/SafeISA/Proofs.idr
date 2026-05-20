-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeISA target-platform surface.
module Proven.SafeISA.Proofs

import Proven.SafeISA

%default total

-- ISA self-equality (7 architectures)
public export
x86_64SelfEq : X86_64 == X86_64 = True
x86_64SelfEq = Refl

public export
aarch64SelfEq : AArch64 == AArch64 = True
aarch64SelfEq = Refl

public export
riscv64SelfEq : RISCV64 == RISCV64 = True
riscv64SelfEq = Refl

public export
mips64SelfEq : MIPS64 == MIPS64 = True
mips64SelfEq = Refl

public export
powerpcSelfEq : PowerPC == PowerPC = True
powerpcSelfEq = Refl

public export
s390xSelfEq : S390x == S390x = True
s390xSelfEq = Refl

public export
wasm32SelfEq : WASM32 == WASM32 = True
wasm32SelfEq = Refl

public export
x86NotAArch64 : X86_64 == AArch64 = False
x86NotAArch64 = Refl

-- Extension self-equality
public export
sseSelfEq : SSE == SSE = True
sseSelfEq = Refl

public export
avx2SelfEq : AVX2 == AVX2 = True
avx2SelfEq = Refl

public export
avx512SelfEq : AVX512 == AVX512 = True
avx512SelfEq = Refl

public export
neonSelfEq : NEON == NEON = True
neonSelfEq = Refl

public export
sveSelfEq : SVE == SVE = True
sveSelfEq = Refl

public export
rvvSelfEq : RVV == RVV = True
rvvSelfEq = Refl

-- Endianness
public export
littleSelfEq : Little == Little = True
littleSelfEq = Refl

public export
bigSelfEq : Big == Big = True
bigSelfEq = Refl

public export
littleNotBig : Little == Big = False
littleNotBig = Refl
