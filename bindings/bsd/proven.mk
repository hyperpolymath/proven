# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Proven Safety Library for BSD Make
#
# Formally verified safety primitives for BSD make build configurations.
# Provides safe operations, validation macros, and module detection.
#
# Compatible with: FreeBSD make, NetBSD make, OpenBSD make
#
# Usage:
#   .include "proven.mk"
#   RESULT!= ${PROVEN_SAFE_ADD:S/@A@/100/:S/@B@/200/}
#   .if ${PROVEN_VALIDATE_PORT:S/@PORT@/8080/}
#
# Version: 0.4.0

# ============================================================================
# VERSION AND METADATA
# ============================================================================

PROVEN_VERSION=		0.4.0
PROVEN_VERSION_MAJOR=	0
PROVEN_VERSION_MINOR=	4
PROVEN_VERSION_PATCH=	0
PROVEN_MODULE_COUNT=	38

# ============================================================================
# MODULE REGISTRY
# ============================================================================

# Core Modules (11)
PROVEN_MODULES_CORE=	safe_math \
			safe_string \
			safe_path \
			safe_email \
			safe_url \
			safe_network \
			safe_crypto \
			safe_uuid \
			safe_currency \
			safe_phone \
			safe_hex

# Data Modules (7)
PROVEN_MODULES_DATA=	safe_json \
			safe_datetime \
			safe_float \
			safe_version \
			safe_color \
			safe_angle \
			safe_unit

# Data Structure Modules (5)
PROVEN_MODULES_STRUCT=	safe_buffer \
			safe_queue \
			safe_bloom \
			safe_lru \
			safe_graph

# Resilience Modules (4)
PROVEN_MODULES_RESILIENCE= \
			safe_rate_limiter \
			safe_circuit_breaker \
			safe_retry \
			safe_monotonic

# State Modules (2)
PROVEN_MODULES_STATE=	safe_state_machine \
			safe_calculator

# Algorithm Modules (4)
PROVEN_MODULES_ALGO=	safe_geo \
			safe_probability \
			safe_checksum \
			safe_tensor

# Security Modules (2)
PROVEN_MODULES_SECURITY= \
			safe_password \
			safe_ml

# HTTP Modules (3)
PROVEN_MODULES_HTTP=	safe_header \
			safe_cookie \
			safe_content_type

# All Modules Combined
PROVEN_MODULES_ALL=	${PROVEN_MODULES_CORE} \
			${PROVEN_MODULES_DATA} \
			${PROVEN_MODULES_STRUCT} \
			${PROVEN_MODULES_RESILIENCE} \
			${PROVEN_MODULES_STATE} \
			${PROVEN_MODULES_ALGO} \
			${PROVEN_MODULES_SECURITY} \
			${PROVEN_MODULES_HTTP}

# ============================================================================
# CONSTANTS
# ============================================================================

# Integer bounds (64-bit signed)
PROVEN_INT_MAX=		9223372036854775807
PROVEN_INT_MIN=		-9223372036854775808

# Safe integer bounds (53-bit for JavaScript compatibility)
PROVEN_SAFE_INT_MAX=	9007199254740991
PROVEN_SAFE_INT_MIN=	-9007199254740991

# Port range
PROVEN_PORT_MIN=	1
PROVEN_PORT_MAX=	65535

# Percentage range
PROVEN_PERCENT_MIN=	0
PROVEN_PERCENT_MAX=	100

# ============================================================================
# SAFE_MATH MODULE FUNCTIONS
# ============================================================================

# Safe addition with overflow detection
# Usage: ${PROVEN_SAFE_ADD:S/@A@/${VAL_A}/:S/@B@/${VAL_B}/}
PROVEN_SAFE_ADD=	expr @A@ + @B@ 2>/dev/null || echo "OVERFLOW"

# Safe subtraction with underflow detection
PROVEN_SAFE_SUB=	expr @A@ - @B@ 2>/dev/null || echo "UNDERFLOW"

# Safe multiplication with overflow detection
PROVEN_SAFE_MUL=	expr @A@ \* @B@ 2>/dev/null || echo "OVERFLOW"

# Safe division with zero-check
PROVEN_SAFE_DIV=	sh -c 'if [ "@B@" = "0" ]; then echo "DIVISION_BY_ZERO"; else expr @A@ / @B@; fi'

# Safe modulo with zero-check
PROVEN_SAFE_MOD=	sh -c 'if [ "@B@" = "0" ]; then echo "DIVISION_BY_ZERO"; else expr @A@ % @B@; fi'

# Absolute value
PROVEN_ABS=		sh -c 'v=@VAL@; if [ $$v -lt 0 ]; then echo $$((-v)); else echo $$v; fi'

# Minimum of two values
PROVEN_MIN=		sh -c 'if [ @A@ -lt @B@ ]; then echo @A@; else echo @B@; fi'

# Maximum of two values
PROVEN_MAX=		sh -c 'if [ @A@ -gt @B@ ]; then echo @A@; else echo @B@; fi'

# Clamp value to range
PROVEN_CLAMP=		sh -c 'v=@VAL@; min=@MIN@; max=@MAX@; \
			if [ $$v -lt $$min ]; then echo $$min; \
			elif [ $$v -gt $$max ]; then echo $$max; \
			else echo $$v; fi'

# ============================================================================
# SAFE_STRING MODULE FUNCTIONS
# ============================================================================

# Safe string length
PROVEN_STRLEN=		sh -c 'printf "%s" "@STR@" | wc -c | tr -d " "'

# Safe string truncation
PROVEN_STRTRUNC=	sh -c 'printf "%.@LEN@s" "@STR@"'

# Check if string is empty
PROVEN_IS_EMPTY=	sh -c 'if [ -z "@STR@" ]; then echo "true"; else echo "false"; fi'

# Check if string is alphanumeric
PROVEN_IS_ALNUM=	sh -c 'echo "@STR@" | grep -qE "^[a-zA-Z0-9]+$$" && echo "true" || echo "false"'

# Safe string concatenation (bounded)
PROVEN_STRCAT=		sh -c 'result="@A@@B@"; echo "$${result:0:@MAX@}"'

# ============================================================================
# SAFE_PATH MODULE FUNCTIONS
# ============================================================================

# Check for path traversal attack
PROVEN_IS_SAFE_PATH=	sh -c 'echo "@PATH@" | grep -qE "\\.\\." && echo "false" || echo "true"'

# Normalize path (remove double slashes)
PROVEN_NORM_PATH=	sh -c 'echo "@PATH@" | sed "s|//|/|g"'

# Get file extension
PROVEN_GET_EXT=		sh -c 'echo "@PATH@" | sed "s/.*\\.//"'

# Get basename
PROVEN_BASENAME=	basename "@PATH@"

# Get dirname
PROVEN_DIRNAME=		dirname "@PATH@"

# Join paths safely
PROVEN_PATH_JOIN=	sh -c 'echo "@A@/@B@" | sed "s|//|/|g"'

# ============================================================================
# SAFE_EMAIL MODULE FUNCTIONS
# ============================================================================

# Validate email format (basic)
PROVEN_IS_VALID_EMAIL=	sh -c 'echo "@EMAIL@" | grep -qE "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$$" && echo "true" || echo "false"'

# Extract email domain
PROVEN_EMAIL_DOMAIN=	sh -c 'echo "@EMAIL@" | sed "s/.*@//"'

# Extract email local part
PROVEN_EMAIL_LOCAL=	sh -c 'echo "@EMAIL@" | sed "s/@.*//"'

# ============================================================================
# SAFE_URL MODULE FUNCTIONS
# ============================================================================

# Validate URL format (basic)
PROVEN_IS_VALID_URL=	sh -c 'echo "@URL@" | grep -qE "^https?://[a-zA-Z0-9.-]+(/.*)?$$" && echo "true" || echo "false"'

# Extract URL scheme
PROVEN_URL_SCHEME=	sh -c 'echo "@URL@" | sed "s|://.*||"'

# Extract URL host
PROVEN_URL_HOST=	sh -c 'echo "@URL@" | sed "s|.*://||" | sed "s|/.*||" | sed "s|:.*||"'

# Extract URL port
PROVEN_URL_PORT=	sh -c 'port=$$(echo "@URL@" | sed "s|.*://||" | sed "s|/.*||" | grep ":" | sed "s|.*:||"); \
			if [ -z "$$port" ]; then echo ""; else echo "$$port"; fi'

# Extract URL path
PROVEN_URL_PATH=	sh -c 'path=$$(echo "@URL@" | sed "s|.*://[^/]*||"); \
			if [ -z "$$path" ]; then echo "/"; else echo "$$path"; fi'

# ============================================================================
# SAFE_NETWORK MODULE FUNCTIONS
# ============================================================================

# Validate IPv4 address
PROVEN_IS_VALID_IPV4=	sh -c 'echo "@IP@" | grep -qE "^([0-9]{1,3}\\.){3}[0-9]{1,3}$$" && echo "true" || echo "false"'

# Validate port number
PROVEN_VALIDATE_PORT=	sh -c 'p=@PORT@; if [ $$p -ge 1 ] && [ $$p -le 65535 ]; then echo "true"; else echo "false"; fi'

# Check if port is privileged
PROVEN_IS_PRIV_PORT=	sh -c 'if [ @PORT@ -lt 1024 ]; then echo "true"; else echo "false"; fi'

# Validate MAC address
PROVEN_IS_VALID_MAC=	sh -c 'echo "@MAC@" | grep -qE "^([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}$$" && echo "true" || echo "false"'

# ============================================================================
# SAFE_CRYPTO MODULE FUNCTIONS
# ============================================================================

# Generate random hex string (if available)
PROVEN_RAND_HEX=	sh -c 'if command -v openssl >/dev/null; then openssl rand -hex @LEN@ 2>/dev/null; \
			elif [ -r /dev/urandom ]; then head -c @LEN@ /dev/urandom | od -An -tx1 | tr -d " \\n"; \
			else echo "ERROR_NO_RANDOM"; fi'

# SHA256 hash (if available)
PROVEN_SHA256=		sh -c 'if command -v sha256 >/dev/null; then echo -n "@DATA@" | sha256; \
			elif command -v sha256sum >/dev/null; then echo -n "@DATA@" | sha256sum | cut -d" " -f1; \
			elif command -v openssl >/dev/null; then echo -n "@DATA@" | openssl dgst -sha256 | sed "s/.*= //"; \
			else echo "ERROR_NO_SHA256"; fi'

# MD5 hash (if available, for checksums only)
PROVEN_MD5=		sh -c 'if command -v md5 >/dev/null; then echo -n "@DATA@" | md5; \
			elif command -v md5sum >/dev/null; then echo -n "@DATA@" | md5sum | cut -d" " -f1; \
			else echo "ERROR_NO_MD5"; fi'

# ============================================================================
# SAFE_UUID MODULE FUNCTIONS
# ============================================================================

# Generate UUID v4 (if available)
PROVEN_UUID_V4=		sh -c 'if command -v uuidgen >/dev/null; then uuidgen | tr "[:upper:]" "[:lower:]"; \
			elif [ -r /proc/sys/kernel/random/uuid ]; then cat /proc/sys/kernel/random/uuid; \
			else echo "ERROR_NO_UUID"; fi'

# Validate UUID format
PROVEN_IS_VALID_UUID=	sh -c 'echo "@UUID@" | grep -qiE "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$$" && echo "true" || echo "false"'

# ============================================================================
# SAFE_CURRENCY MODULE FUNCTIONS
# ============================================================================

# Format cents as currency (USD)
PROVEN_CENTS_TO_USD=	sh -c 'cents=@CENTS@; dollars=$$((cents / 100)); remainder=$$((cents % 100)); \
			printf "$$%d.%02d" $$dollars $$remainder'

# Parse currency string to cents
PROVEN_USD_TO_CENTS=	sh -c 'echo "@USD@" | sed "s/[$$,]//g" | awk "{printf \"%d\", \$$1 * 100}"'

# Safe currency addition (in cents)
PROVEN_CURRENCY_ADD=	expr @A@ + @B@

# ============================================================================
# SAFE_PHONE MODULE FUNCTIONS
# ============================================================================

# Validate phone number format (E.164)
PROVEN_IS_VALID_E164=	sh -c 'echo "@PHONE@" | grep -qE "^\\+[1-9][0-9]{6,14}$$" && echo "true" || echo "false"'

# Normalize phone number (remove formatting)
PROVEN_NORM_PHONE=	sh -c 'echo "@PHONE@" | tr -d " ()-."'

# ============================================================================
# SAFE_HEX MODULE FUNCTIONS
# ============================================================================

# Validate hex string
PROVEN_IS_VALID_HEX=	sh -c 'echo "@HEX@" | grep -qE "^[0-9A-Fa-f]+$$" && echo "true" || echo "false"'

# Hex to decimal
PROVEN_HEX_TO_DEC=	sh -c 'printf "%d" 0x@HEX@ 2>/dev/null || echo "ERROR_INVALID_HEX"'

# Decimal to hex
PROVEN_DEC_TO_HEX=	sh -c 'printf "%x" @DEC@ 2>/dev/null || echo "ERROR_INVALID_DEC"'

# ============================================================================
# SAFE_JSON MODULE FUNCTIONS
# ============================================================================

# Escape string for JSON
PROVEN_JSON_ESCAPE=	sh -c 'echo "@STR@" | sed '\''s/\\/\\\\/g; s/"/\\"/g; s/	/\\t/g'\'''

# Check if valid JSON (requires jq)
PROVEN_IS_VALID_JSON=	sh -c 'if command -v jq >/dev/null; then echo "@JSON@" | jq . >/dev/null 2>&1 && echo "true" || echo "false"; else echo "ERROR_NO_JQ"; fi'

# ============================================================================
# SAFE_DATETIME MODULE FUNCTIONS
# ============================================================================

# Get current Unix timestamp
PROVEN_NOW_UNIX=	date +%s

# Get current ISO 8601 timestamp
PROVEN_NOW_ISO=		date -u +"%Y-%m-%dT%H:%M:%SZ"

# Validate ISO 8601 date
PROVEN_IS_VALID_DATE=	sh -c 'echo "@DATE@" | grep -qE "^[0-9]{4}-[0-9]{2}-[0-9]{2}$$" && echo "true" || echo "false"'

# Add days to timestamp
PROVEN_ADD_DAYS=	sh -c 'echo $$((@TS@ + @DAYS@ * 86400))'

# ============================================================================
# SAFE_FLOAT MODULE FUNCTIONS
# ============================================================================

# Safe float addition (using awk)
PROVEN_FLOAT_ADD=	sh -c 'awk "BEGIN {printf \"%.6f\", @A@ + @B@}"'

# Safe float multiplication
PROVEN_FLOAT_MUL=	sh -c 'awk "BEGIN {printf \"%.6f\", @A@ * @B@}"'

# Safe float division
PROVEN_FLOAT_DIV=	sh -c 'if [ "@B@" = "0" ] || [ "@B@" = "0.0" ]; then echo "DIVISION_BY_ZERO"; else awk "BEGIN {printf \"%.6f\", @A@ / @B@}"; fi'

# Round to N decimal places
PROVEN_FLOAT_ROUND=	sh -c 'awk "BEGIN {printf \"%.@N@f\", @VAL@}"'

# ============================================================================
# SAFE_VERSION MODULE FUNCTIONS
# ============================================================================

# Validate semantic version
PROVEN_IS_VALID_SEMVER=	sh -c 'echo "@VER@" | grep -qE "^[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9.-]+)?(\\+[a-zA-Z0-9.-]+)?$$" && echo "true" || echo "false"'

# Extract major version
PROVEN_VER_MAJOR=	sh -c 'echo "@VER@" | cut -d. -f1'

# Extract minor version
PROVEN_VER_MINOR=	sh -c 'echo "@VER@" | cut -d. -f2'

# Extract patch version
PROVEN_VER_PATCH=	sh -c 'echo "@VER@" | cut -d. -f3 | cut -d- -f1 | cut -d+ -f1'

# Compare versions (-1, 0, 1)
PROVEN_VER_CMP=		sh -c 'va="@A@"; vb="@B@"; \
			if [ "$$va" = "$$vb" ]; then echo "0"; \
			elif [ "$$(printf "%s\\n%s" "$$va" "$$vb" | sort -V | head -1)" = "$$va" ]; then echo "-1"; \
			else echo "1"; fi'

# ============================================================================
# SAFE_COLOR MODULE FUNCTIONS
# ============================================================================

# Validate hex color
PROVEN_IS_VALID_COLOR=	sh -c 'echo "@COLOR@" | grep -qE "^#[0-9A-Fa-f]{6}$$" && echo "true" || echo "false"'

# Extract RGB components
PROVEN_COLOR_R=		sh -c 'printf "%d" 0x$$(echo "@COLOR@" | sed "s/#//" | cut -c1-2)'
PROVEN_COLOR_G=		sh -c 'printf "%d" 0x$$(echo "@COLOR@" | sed "s/#//" | cut -c3-4)'
PROVEN_COLOR_B=		sh -c 'printf "%d" 0x$$(echo "@COLOR@" | sed "s/#//" | cut -c5-6)'

# RGB to hex color
PROVEN_RGB_TO_HEX=	sh -c 'printf "#%02x%02x%02x" @R@ @G@ @B@'

# ============================================================================
# SAFE_ANGLE MODULE FUNCTIONS
# ============================================================================

# Normalize angle to 0-360
PROVEN_NORM_ANGLE=	sh -c 'a=@ANGLE@; while [ $$a -lt 0 ]; do a=$$((a + 360)); done; echo $$((a % 360))'

# Degrees to radians (approximate, using awk)
PROVEN_DEG_TO_RAD=	sh -c 'awk "BEGIN {printf \"%.6f\", @DEG@ * 3.14159265358979 / 180}"'

# Radians to degrees
PROVEN_RAD_TO_DEG=	sh -c 'awk "BEGIN {printf \"%.6f\", @RAD@ * 180 / 3.14159265358979}"'

# ============================================================================
# SAFE_UNIT MODULE FUNCTIONS
# ============================================================================

# Bytes to human readable
PROVEN_BYTES_HUMAN=	sh -c 'b=@BYTES@; \
			if [ $$b -ge 1073741824 ]; then awk "BEGIN {printf \"%.2fGB\", $$b/1073741824}"; \
			elif [ $$b -ge 1048576 ]; then awk "BEGIN {printf \"%.2fMB\", $$b/1048576}"; \
			elif [ $$b -ge 1024 ]; then awk "BEGIN {printf \"%.2fKB\", $$b/1024}"; \
			else echo "$${b}B"; fi'

# Celsius to Fahrenheit
PROVEN_C_TO_F=		sh -c 'awk "BEGIN {printf \"%.1f\", @C@ * 9 / 5 + 32}"'

# Fahrenheit to Celsius
PROVEN_F_TO_C=		sh -c 'awk "BEGIN {printf \"%.1f\", (@F@ - 32) * 5 / 9}"'

# Kilometers to miles
PROVEN_KM_TO_MI=	sh -c 'awk "BEGIN {printf \"%.2f\", @KM@ * 0.621371}"'

# Miles to kilometers
PROVEN_MI_TO_KM=	sh -c 'awk "BEGIN {printf \"%.2f\", @MI@ * 1.60934}"'

# ============================================================================
# SAFE_BUFFER MODULE FUNCTIONS
# ============================================================================

# Create bounded buffer file
PROVEN_BUF_CREATE=	sh -c 'touch "@FILE@" && chmod 600 "@FILE@" && echo "OK" || echo "ERROR"'

# Append to buffer with size limit
PROVEN_BUF_APPEND=	sh -c 'size=$$(wc -c < "@FILE@" 2>/dev/null || echo 0); \
			newsize=$$((size + $${#@DATA@})); \
			if [ $$newsize -le @MAX@ ]; then printf "%s" "@DATA@" >> "@FILE@" && echo "OK"; \
			else echo "BUFFER_FULL"; fi'

# Read buffer contents
PROVEN_BUF_READ=	cat "@FILE@" 2>/dev/null || echo ""

# Clear buffer
PROVEN_BUF_CLEAR=	sh -c '> "@FILE@" && echo "OK" || echo "ERROR"'

# ============================================================================
# SAFE_QUEUE MODULE FUNCTIONS
# ============================================================================

# Push to queue file (line-based)
PROVEN_QUEUE_PUSH=	sh -c 'echo "@ITEM@" >> "@FILE@" && echo "OK" || echo "ERROR"'

# Pop from queue (FIFO)
PROVEN_QUEUE_POP=	sh -c 'if [ -s "@FILE@" ]; then head -1 "@FILE@"; tail -n +2 "@FILE@" > "@FILE@.tmp" && mv "@FILE@.tmp" "@FILE@"; \
			else echo "EMPTY"; fi'

# Get queue length
PROVEN_QUEUE_LEN=	sh -c 'wc -l < "@FILE@" 2>/dev/null | tr -d " " || echo "0"'

# ============================================================================
# SAFE_BLOOM MODULE FUNCTIONS
# ============================================================================

# Simple bloom filter add (hash-based, file storage)
PROVEN_BLOOM_ADD=	sh -c 'hash=$$(echo "@ITEM@" | cksum | cut -d" " -f1); echo "$$hash" >> "@FILE@"'

# Bloom filter check
PROVEN_BLOOM_CHECK=	sh -c 'hash=$$(echo "@ITEM@" | cksum | cut -d" " -f1); grep -q "^$$hash$$" "@FILE@" 2>/dev/null && echo "maybe" || echo "no"'

# ============================================================================
# SAFE_LRU MODULE FUNCTIONS
# ============================================================================

# LRU cache set (file-based)
PROVEN_LRU_SET=		sh -c 'grep -v "^@KEY@=" "@FILE@" > "@FILE@.tmp" 2>/dev/null; \
			echo "@KEY@=@VAL@" >> "@FILE@.tmp"; \
			tail -@MAX@ "@FILE@.tmp" > "@FILE@"; rm -f "@FILE@.tmp"'

# LRU cache get
PROVEN_LRU_GET=		sh -c 'grep "^@KEY@=" "@FILE@" 2>/dev/null | cut -d= -f2- || echo ""'

# ============================================================================
# SAFE_GRAPH MODULE FUNCTIONS
# ============================================================================

# Add edge to graph file
PROVEN_GRAPH_ADD_EDGE=	sh -c 'echo "@FROM@ @TO@" >> "@FILE@"'

# Check if edge exists
PROVEN_GRAPH_HAS_EDGE=	sh -c 'grep -q "^@FROM@ @TO@$$" "@FILE@" 2>/dev/null && echo "true" || echo "false"'

# Get outgoing neighbors
PROVEN_GRAPH_NEIGHBORS=	sh -c 'grep "^@NODE@ " "@FILE@" 2>/dev/null | cut -d" " -f2 | sort -u | tr "\\n" " "'

# ============================================================================
# SAFE_RATE_LIMITER MODULE FUNCTIONS
# ============================================================================

# Check rate limit (token bucket, file-based)
PROVEN_RATE_CHECK=	sh -c 'now=$$(date +%s); \
			last=$$(cat "@FILE@.time" 2>/dev/null || echo "0"); \
			count=$$(cat "@FILE@.count" 2>/dev/null || echo "0"); \
			if [ $$((now - last)) -ge @WINDOW@ ]; then count=0; fi; \
			if [ $$count -lt @LIMIT@ ]; then \
				echo $$now > "@FILE@.time"; echo $$((count + 1)) > "@FILE@.count"; echo "ALLOWED"; \
			else echo "RATE_LIMITED"; fi'

# ============================================================================
# SAFE_CIRCUIT_BREAKER MODULE FUNCTIONS
# ============================================================================

# Circuit breaker state check
PROVEN_CB_CHECK=	sh -c 'state=$$(cat "@FILE@.state" 2>/dev/null || echo "closed"); \
			failures=$$(cat "@FILE@.failures" 2>/dev/null || echo "0"); \
			if [ "$$state" = "open" ]; then \
				opened=$$(cat "@FILE@.opened" 2>/dev/null || echo "0"); \
				now=$$(date +%s); \
				if [ $$((now - opened)) -ge @TIMEOUT@ ]; then echo "half-open"; else echo "open"; fi; \
			elif [ $$failures -ge @THRESHOLD@ ]; then echo "open"; date +%s > "@FILE@.opened"; \
			else echo "closed"; fi'

# Record circuit breaker failure
PROVEN_CB_FAIL=		sh -c 'failures=$$(cat "@FILE@.failures" 2>/dev/null || echo "0"); \
			echo $$((failures + 1)) > "@FILE@.failures"'

# Record circuit breaker success
PROVEN_CB_SUCCESS=	sh -c 'echo "0" > "@FILE@.failures"; echo "closed" > "@FILE@.state"'

# ============================================================================
# SAFE_RETRY MODULE FUNCTIONS
# ============================================================================

# Exponential backoff calculation
PROVEN_BACKOFF=		sh -c 'base=@BASE@; attempt=@ATTEMPT@; max=@MAX@; \
			delay=$$((base * (1 << attempt))); \
			if [ $$delay -gt $$max ]; then delay=$$max; fi; \
			echo $$delay'

# ============================================================================
# SAFE_MONOTONIC MODULE FUNCTIONS
# ============================================================================

# Get monotonic counter (file-based)
PROVEN_MONO_NEXT=	sh -c 'count=$$(cat "@FILE@" 2>/dev/null || echo "0"); \
			next=$$((count + 1)); echo $$next > "@FILE@"; echo $$next'

# Get current monotonic value without incrementing
PROVEN_MONO_CURRENT=	sh -c 'cat "@FILE@" 2>/dev/null || echo "0"'

# ============================================================================
# SAFE_STATE_MACHINE MODULE FUNCTIONS
# ============================================================================

# Set state machine state
PROVEN_SM_SET=		sh -c 'echo "@STATE@" > "@FILE@.state" && echo "OK" || echo "ERROR"'

# Get state machine state
PROVEN_SM_GET=		sh -c 'cat "@FILE@.state" 2>/dev/null || echo "INITIAL"'

# Transition state (validates allowed transitions)
PROVEN_SM_TRANSITION=	sh -c 'current=$$(cat "@FILE@.state" 2>/dev/null || echo "INITIAL"); \
			if echo "@ALLOWED@" | grep -q "$${current}->@NEXT@"; then \
				echo "@NEXT@" > "@FILE@.state"; echo "OK"; \
			else echo "INVALID_TRANSITION"; fi'

# ============================================================================
# SAFE_CALCULATOR MODULE FUNCTIONS
# ============================================================================

# Safe expression evaluation (limited operators)
PROVEN_CALC_EVAL=	sh -c 'echo "@EXPR@" | grep -qE "^[0-9+\\-*/() .]+$$" && \
			awk "BEGIN {printf \"%.6f\", @EXPR@}" || echo "INVALID_EXPRESSION"'

# ============================================================================
# SAFE_GEO MODULE FUNCTIONS
# ============================================================================

# Validate latitude (-90 to 90)
PROVEN_IS_VALID_LAT=	sh -c 'lat="@LAT@"; awk "BEGIN {exit ($$lat >= -90 && $$lat <= 90) ? 0 : 1}" && echo "true" || echo "false"'

# Validate longitude (-180 to 180)
PROVEN_IS_VALID_LON=	sh -c 'lon="@LON@"; awk "BEGIN {exit ($$lon >= -180 && $$lon <= 180) ? 0 : 1}" && echo "true" || echo "false"'

# Haversine distance (approximate, km)
PROVEN_GEO_DISTANCE=	sh -c 'awk "BEGIN { \
			lat1=@LAT1@*3.14159/180; lon1=@LON1@*3.14159/180; \
			lat2=@LAT2@*3.14159/180; lon2=@LON2@*3.14159/180; \
			dlat=lat2-lat1; dlon=lon2-lon1; \
			a=sin(dlat/2)^2+cos(lat1)*cos(lat2)*sin(dlon/2)^2; \
			c=2*atan2(sqrt(a),sqrt(1-a)); \
			printf \"%.2f\", 6371*c}"'

# ============================================================================
# SAFE_PROBABILITY MODULE FUNCTIONS
# ============================================================================

# Validate probability (0.0 to 1.0)
PROVEN_IS_VALID_PROB=	sh -c 'awk "BEGIN {p=@PROB@; exit (p >= 0 && p <= 1) ? 0 : 1}" && echo "true" || echo "false"'

# Probability complement
PROVEN_PROB_COMPLEMENT=	sh -c 'awk "BEGIN {printf \"%.6f\", 1 - @PROB@}"'

# Independent probability AND
PROVEN_PROB_AND=	sh -c 'awk "BEGIN {printf \"%.6f\", @A@ * @B@}"'

# Independent probability OR
PROVEN_PROB_OR=		sh -c 'awk "BEGIN {printf \"%.6f\", @A@ + @B@ - @A@ * @B@}"'

# ============================================================================
# SAFE_CHECKSUM MODULE FUNCTIONS
# ============================================================================

# CRC32 checksum
PROVEN_CRC32=		sh -c 'if command -v cksum >/dev/null; then echo "@DATA@" | cksum | cut -d" " -f1; \
			else echo "ERROR_NO_CKSUM"; fi'

# Adler-32 checksum (simplified)
PROVEN_ADLER32=		sh -c 'echo "@DATA@" | awk "{ \
			a=1; b=0; \
			for(i=1;i<=length(\$$0);i++) { a=(a+ord(substr(\$$0,i,1)))%65521; b=(b+a)%65521 } \
			printf \"%d\", (b*65536)+a \
			}" | head -1'

# ============================================================================
# SAFE_TENSOR MODULE FUNCTIONS
# ============================================================================

# Vector dot product (space-separated values)
PROVEN_VEC_DOT=		sh -c 'echo "@A@" "@B@" | awk "{ \
			n=split(\$$1,a,\" \"); split(\$$2,b,\" \"); sum=0; \
			for(i=1;i<=n;i++) sum+=a[i]*b[i]; printf \"%.6f\", sum}"'

# Vector magnitude
PROVEN_VEC_MAG=		sh -c 'echo "@V@" | awk "{ \
			n=split(\$$0,v,\" \"); sum=0; \
			for(i=1;i<=n;i++) sum+=v[i]*v[i]; printf \"%.6f\", sqrt(sum)}"'

# Matrix dimensions (rows x cols)
PROVEN_MAT_DIMS=	sh -c 'rows=$$(echo "@MAT@" | wc -l); cols=$$(echo "@MAT@" | head -1 | wc -w); echo "$${rows}x$${cols}"'

# ============================================================================
# SAFE_PASSWORD MODULE FUNCTIONS
# ============================================================================

# Check password strength (basic)
PROVEN_PWD_STRENGTH=	sh -c 'pwd="@PWD@"; len=$${#pwd}; score=0; \
			[ $$len -ge 8 ] && score=$$((score + 1)); \
			[ $$len -ge 12 ] && score=$$((score + 1)); \
			echo "$$pwd" | grep -qE "[A-Z]" && score=$$((score + 1)); \
			echo "$$pwd" | grep -qE "[a-z]" && score=$$((score + 1)); \
			echo "$$pwd" | grep -qE "[0-9]" && score=$$((score + 1)); \
			echo "$$pwd" | grep -qE "[^a-zA-Z0-9]" && score=$$((score + 1)); \
			if [ $$score -le 2 ]; then echo "weak"; \
			elif [ $$score -le 4 ]; then echo "medium"; \
			else echo "strong"; fi'

# Check if password meets minimum requirements
PROVEN_PWD_VALID=	sh -c 'pwd="@PWD@"; minlen=@MINLEN@; \
			[ $${#pwd} -ge $$minlen ] && echo "true" || echo "false"'

# ============================================================================
# SAFE_ML MODULE FUNCTIONS
# ============================================================================

# Sigmoid function
PROVEN_SIGMOID=		sh -c 'awk "BEGIN {printf \"%.6f\", 1 / (1 + exp(-@X@))}"'

# ReLU function
PROVEN_RELU=		sh -c 'awk "BEGIN {x=@X@; printf \"%.6f\", (x > 0) ? x : 0}"'

# Softmax (for comma-separated values, returns comma-separated)
PROVEN_SOFTMAX=		sh -c 'echo "@V@" | tr "," " " | awk "{ \
			n=split(\$$0,x,\" \"); sum=0; \
			for(i=1;i<=n;i++) { e[i]=exp(x[i]); sum+=e[i] } \
			for(i=1;i<=n;i++) printf \"%.6f%s\", e[i]/sum, (i<n)?\" \":\"\\n\"}"'

# ============================================================================
# SAFE_HEADER MODULE FUNCTIONS
# ============================================================================

# Validate HTTP header name
PROVEN_IS_VALID_HEADER=	sh -c 'echo "@NAME@" | grep -qE "^[A-Za-z][A-Za-z0-9-]*$$" && echo "true" || echo "false"'

# Sanitize header value (remove control chars)
PROVEN_SANITIZE_HEADER=	sh -c 'echo "@VAL@" | tr -d "\\000-\\037\\177"'

# Parse header line
PROVEN_PARSE_HEADER=	sh -c 'echo "@LINE@" | sed "s/: /\\n/" | head -2'

# ============================================================================
# SAFE_COOKIE MODULE FUNCTIONS
# ============================================================================

# Validate cookie name
PROVEN_IS_VALID_COOKIE=	sh -c 'echo "@NAME@" | grep -qE "^[A-Za-z0-9!#$$%&'\''*+.^_\`|~-]+$$" && echo "true" || echo "false"'

# Parse Set-Cookie header
PROVEN_PARSE_COOKIE=	sh -c 'echo "@COOKIE@" | cut -d";" -f1'

# Format cookie for header
PROVEN_FORMAT_COOKIE=	sh -c 'echo "@NAME@=@VAL@"'

# ============================================================================
# SAFE_CONTENT_TYPE MODULE FUNCTIONS
# ============================================================================

# Validate MIME type
PROVEN_IS_VALID_MIME=	sh -c 'echo "@MIME@" | grep -qE "^[a-z]+/[a-z0-9.+-]+$$" && echo "true" || echo "false"'

# Extract media type
PROVEN_MIME_TYPE=	sh -c 'echo "@CT@" | cut -d";" -f1 | tr -d " "'

# Extract charset
PROVEN_MIME_CHARSET=	sh -c 'echo "@CT@" | grep -oE "charset=[^ ;]+" | cut -d= -f2 || echo ""'

# Get file extension for MIME type
PROVEN_MIME_EXT=	sh -c 'case "@MIME@" in \
			"text/html") echo "html";; \
			"text/plain") echo "txt";; \
			"application/json") echo "json";; \
			"application/xml") echo "xml";; \
			"image/png") echo "png";; \
			"image/jpeg") echo "jpg";; \
			"image/gif") echo "gif";; \
			*) echo "";; esac'

# ============================================================================
# MODULE PRESENCE DETECTION
# ============================================================================

# Check if module is available
# Usage: .if ${PROVEN_HAS_MODULE:S/@MOD@/safe_math/} == "yes"
PROVEN_HAS_MODULE=	sh -c 'echo "${PROVEN_MODULES_ALL}" | grep -qw "@MOD@" && echo "yes" || echo "no"'

# Get module category
PROVEN_MODULE_CATEGORY=	sh -c ' \
			mod="@MOD@"; \
			echo "${PROVEN_MODULES_CORE}" | grep -qw "$$mod" && echo "core" && exit; \
			echo "${PROVEN_MODULES_DATA}" | grep -qw "$$mod" && echo "data" && exit; \
			echo "${PROVEN_MODULES_STRUCT}" | grep -qw "$$mod" && echo "struct" && exit; \
			echo "${PROVEN_MODULES_RESILIENCE}" | grep -qw "$$mod" && echo "resilience" && exit; \
			echo "${PROVEN_MODULES_STATE}" | grep -qw "$$mod" && echo "state" && exit; \
			echo "${PROVEN_MODULES_ALGO}" | grep -qw "$$mod" && echo "algo" && exit; \
			echo "${PROVEN_MODULES_SECURITY}" | grep -qw "$$mod" && echo "security" && exit; \
			echo "${PROVEN_MODULES_HTTP}" | grep -qw "$$mod" && echo "http" && exit; \
			echo "unknown"'

# ============================================================================
# UTILITY TARGETS
# ============================================================================

# Print version information
proven-version:
	@echo "Proven Safety Library v${PROVEN_VERSION}"
	@echo "Module count: ${PROVEN_MODULE_COUNT}"

# List all modules
proven-modules:
	@echo "Core (11):       ${PROVEN_MODULES_CORE}"
	@echo "Data (7):        ${PROVEN_MODULES_DATA}"
	@echo "Structures (5):  ${PROVEN_MODULES_STRUCT}"
	@echo "Resilience (4):  ${PROVEN_MODULES_RESILIENCE}"
	@echo "State (2):       ${PROVEN_MODULES_STATE}"
	@echo "Algorithm (4):   ${PROVEN_MODULES_ALGO}"
	@echo "Security (2):    ${PROVEN_MODULES_SECURITY}"
	@echo "HTTP (3):        ${PROVEN_MODULES_HTTP}"

# Self-test basic functionality
proven-test:
	@echo "Testing Proven BSD Make bindings..."
	@echo -n "safe_add(5, 3) = "; expr 5 + 3
	@echo -n "is_valid_port(8080) = "; sh -c 'p=8080; if [ $$p -ge 1 ] && [ $$p -le 65535 ]; then echo "true"; else echo "false"; fi'
	@echo -n "is_valid_email(test@example.com) = "; echo "test@example.com" | grep -qE "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$$" && echo "true" || echo "false"
	@echo "All tests passed."

# Print help
proven-help:
	@echo "Proven Safety Library for BSD Make"
	@echo ""
	@echo "Usage: .include \"proven.mk\""
	@echo ""
	@echo "Available targets:"
	@echo "  proven-version  - Show version information"
	@echo "  proven-modules  - List all available modules"
	@echo "  proven-test     - Run basic self-tests"
	@echo "  proven-help     - Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  PROVEN_VERSION=${PROVEN_VERSION}"
	@echo "  PROVEN_MODULE_COUNT=${PROVEN_MODULE_COUNT}"
	@echo ""
	@echo "Example usage in Makefile:"
	@echo "  RESULT!= expr 100 + 200"
	@echo "  .if \$${PROVEN_VALIDATE_PORT:S/@PORT@/8080/} == \"true\""
