-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
Proven Safety Primitives for Dhall

Dhall is already a dependently-typed configuration language,
making it a natural fit for Proven's safety guarantees.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan
let Optional/default = Prelude.Optional.default
let Text/concatSep = Prelude.Text.concatSep

-- Result type for safe operations
let Result = λ(T : Type) → λ(E : Type) → < Ok : T | Err : E >

let ok = λ(T : Type) → λ(E : Type) → λ(value : T) → (Result T E).Ok value
let err = λ(T : Type) → λ(E : Type) → λ(error : E) → (Result T E).Err error

let isOk = λ(T : Type) → λ(E : Type) → λ(r : Result T E) →
  merge { Ok = λ(_ : T) → True, Err = λ(_ : E) → False } r

let unwrapOr = λ(T : Type) → λ(E : Type) → λ(default : T) → λ(r : Result T E) →
  merge { Ok = λ(v : T) → v, Err = λ(_ : E) → default } r

-- Safe bounded natural number
let BoundedNat = { value : Natural, min : Natural, max : Natural }

let mkBoundedNat
    : Natural → Natural → Natural → Optional BoundedNat
    = λ(value : Natural) → λ(min : Natural) → λ(max : Natural) →
        if Natural/lessThan value min
        then None BoundedNat
        else if Natural/greaterThan value max
        then None BoundedNat
        else Some { value, min, max }

-- Safe port number (1-65535)
let Port = { port : Natural }

let mkPort
    : Natural → Optional Port
    = λ(n : Natural) →
        if Natural/lessThan n 1
        then None Port
        else if Natural/greaterThan n 65535
        then None Port
        else Some { port = n }

let commonPorts = {
    http = { port = 80 },
    https = { port = 443 },
    ssh = { port = 22 },
    dns = { port = 53 }
}

-- Memory size
let MemoryUnit = < B | KB | MB | GB | TB >

let Memory = { value : Natural, unit : MemoryUnit }

let toBytes
    : Memory → Natural
    = λ(m : Memory) →
        let multiplier = merge {
            B = 1,
            KB = 1024,
            MB = 1048576,
            GB = 1073741824,
            TB = 1099511627776
        } m.unit
        in m.value * multiplier

-- CPU resources
let CPUUnit = < Millicores | Cores >

let CPU = { value : Natural, unit : CPUUnit }

let toMillicores
    : CPU → Natural
    = λ(c : CPU) →
        merge {
            Millicores = c.value,
            Cores = c.value * 1000
        } c.unit

-- Duration
let DurationUnit = < Milliseconds | Seconds | Minutes | Hours | Days >

let Duration = { value : Natural, unit : DurationUnit }

let toMilliseconds
    : Duration → Natural
    = λ(d : Duration) →
        let multiplier = merge {
            Milliseconds = 1,
            Seconds = 1000,
            Minutes = 60000,
            Hours = 3600000,
            Days = 86400000
        } d.unit
        in d.value * multiplier

-- Replica count with bounds
let Replicas = { count : Natural, min : Natural, max : Natural }

let mkReplicas
    : Natural → Natural → Natural → Optional Replicas
    = λ(count : Natural) → λ(min : Natural) → λ(max : Natural) →
        if Natural/lessThan count min
        then None Replicas
        else if Natural/greaterThan count max
        then None Replicas
        else Some { count, min, max }

-- Resource constraints
let ResourceConstraints = {
    requestsMemory : Optional Memory,
    requestsCPU : Optional CPU,
    limitsMemory : Optional Memory,
    limitsCPU : Optional CPU
}

let defaultResources : ResourceConstraints = {
    requestsMemory = None Memory,
    requestsCPU = None CPU,
    limitsMemory = None Memory,
    limitsCPU = None CPU
}

-- Health probe
let ProbeType = < HTTP : { path : Text, port : Port } | TCP : Port | Exec : List Text >

let Probe = {
    type : ProbeType,
    initialDelay : Optional Duration,
    interval : Optional Duration,
    timeout : Optional Duration,
    retries : Optional Natural
}

-- Service definition
let Protocol = < TCP | UDP >

let Service = {
    name : Text,
    port : Port,
    protocol : Protocol,
    targetPort : Optional Port
}

-- Environment variable
let EnvVar = { name : Text, value : Text }

-- Secret reference
let SecretRef = { name : Text, key : Text }

-- Export all definitions
in {
    -- Types
    Result,
    BoundedNat,
    Port,
    MemoryUnit,
    Memory,
    CPUUnit,
    CPU,
    DurationUnit,
    Duration,
    Replicas,
    ResourceConstraints,
    ProbeType,
    Probe,
    Protocol,
    Service,
    EnvVar,
    SecretRef,

    -- Constructors
    ok,
    err,
    mkBoundedNat,
    mkPort,
    mkReplicas,

    -- Utilities
    isOk,
    unwrapOr,
    toBytes,
    toMillicores,
    toMilliseconds,

    -- Constants
    commonPorts,
    defaultResources
}
