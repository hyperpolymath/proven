-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-loadbalancer load balancer.
||| Defines closed sum types for load balancing algorithms, health check
||| types, backend states, session persistence modes, and protocols.
module Loadbalancer.Types

%default total

---------------------------------------------------------------------------
-- Algorithm: The strategy used to distribute traffic across backends.
---------------------------------------------------------------------------

||| Enumerates the load balancing algorithms available for distributing
||| incoming requests across the pool of healthy backend servers.
public export
data Algorithm
  = RoundRobin         -- ^ Cycle through backends in sequential order
  | LeastConnections   -- ^ Route to the backend with fewest active connections
  | IPHash             -- ^ Hash client IP to select a consistent backend
  | Random             -- ^ Select a backend uniformly at random
  | WeightedRoundRobin -- ^ Round-robin weighted by configured backend weights
  | LeastResponseTime  -- ^ Route to the backend with lowest recent latency

||| Display a human-readable label for each algorithm.
public export
Show Algorithm where
  show RoundRobin         = "RoundRobin"
  show LeastConnections   = "LeastConnections"
  show IPHash             = "IPHash"
  show Random             = "Random"
  show WeightedRoundRobin = "WeightedRoundRobin"
  show LeastResponseTime  = "LeastResponseTime"

---------------------------------------------------------------------------
-- HealthCheckType: The probe mechanism for backend health verification.
---------------------------------------------------------------------------

||| Specifies how the load balancer probes a backend to determine whether
||| it is healthy enough to receive traffic.
public export
data HealthCheckType
  = HcHTTP   -- ^ Send an HTTP request and validate the response status code
  | HcTCP    -- ^ Attempt a TCP connection to the backend port
  | HcGRPC   -- ^ Use the gRPC health checking protocol (grpc.health.v1)
  | HcScript -- ^ Execute an external script; exit code 0 means healthy

||| Display a human-readable label for each health check type.
public export
Show HealthCheckType where
  show HcHTTP   = "HTTP"
  show HcTCP    = "TCP"
  show HcGRPC   = "gRPC"
  show HcScript = "Script"

---------------------------------------------------------------------------
-- BackendState: The operational state of a backend server.
---------------------------------------------------------------------------

||| Represents the current operational state of a backend in the pool,
||| as determined by health checks and administrative actions.
public export
data BackendState
  = Healthy   -- ^ Backend is passing health checks and receiving traffic
  | Unhealthy -- ^ Backend is failing health checks and removed from rotation
  | Draining  -- ^ Backend is completing in-flight requests but accepting no new ones
  | BDisabled -- ^ Backend is administratively disabled

||| Display a human-readable label for each backend state.
public export
Show BackendState where
  show Healthy   = "Healthy"
  show Unhealthy = "Unhealthy"
  show Draining  = "Draining"
  show BDisabled = "Disabled"

---------------------------------------------------------------------------
-- SessionPersistence: How client affinity to a backend is maintained.
---------------------------------------------------------------------------

||| Controls whether and how the load balancer maintains session affinity
||| so that a client's requests consistently reach the same backend.
public export
data SessionPersistence
  = None     -- ^ No session affinity; each request independently balanced
  | Cookie   -- ^ Inject or read a cookie to pin the client to a backend
  | SourceIP -- ^ Use the client's source IP address for consistent hashing
  | Header   -- ^ Use a specified HTTP header value for consistent hashing

||| Display a human-readable label for each session persistence mode.
public export
Show SessionPersistence where
  show None     = "None"
  show Cookie   = "Cookie"
  show SourceIP = "SourceIP"
  show Header   = "Header"

---------------------------------------------------------------------------
-- Protocol: The network protocol handled by the load balancer listener.
---------------------------------------------------------------------------

||| Specifies the protocol of a listener/frontend on the load balancer.
public export
data Protocol
  = HTTP  -- ^ Plaintext HTTP (Layer 7)
  | HTTPS -- ^ TLS-terminated HTTP (Layer 7)
  | TCP   -- ^ Raw TCP passthrough (Layer 4)
  | UDP   -- ^ UDP passthrough (Layer 4)
  | GRPC  -- ^ gRPC over HTTP/2 (Layer 7)

||| Display a human-readable label for each protocol.
public export
Show Protocol where
  show HTTP  = "HTTP"
  show HTTPS = "HTTPS"
  show TCP   = "TCP"
  show UDP   = "UDP"
  show GRPC  = "gRPC"
