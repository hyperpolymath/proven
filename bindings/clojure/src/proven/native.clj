;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; Native FFI bridge to libproven via JNA.
;; All struct definitions and marshalling utilities live here.
;; No domain logic is implemented; only data marshalling.

(ns proven.native
  "JNA bridge to libproven. Defines struct layouts and marshalling utilities.

  All computation is performed in Idris 2 via the Zig FFI layer.
  This namespace handles only data marshalling between Clojure and C."
  (:import [com.sun.jna Function Native NativeLibrary Pointer Memory Structure]
           [java.nio.charset StandardCharsets]
           [java.util List]))

;; ---------------------------------------------------------------------------
;; Library handle
;; ---------------------------------------------------------------------------

(def ^:private ^NativeLibrary lib
  (NativeLibrary/getInstance "proven"))

(defn get-fn
  "Get a JNA Function handle for a libproven symbol."
  ^Function [^String name]
  (.getFunction lib name))

;; ---------------------------------------------------------------------------
;; JNA Structure definitions (mirrors C ABI)
;; ---------------------------------------------------------------------------

(gen-class
  :name proven.native.IntResult
  :extends com.sun.jna.Structure
  :implements [com.sun.jna.Structure$ByValue]
  :state state
  :init init-state
  :prefix "ir-")

;; Since gen-class is compile-time only, we use deftype with JNA interop instead.
;; The cleanest Clojure JNA approach is to define result types inline.

(defn- make-int-result-type []
  (proxy [Structure] []
    (getFieldOrder [] (java.util.Arrays/asList (into-array String ["status" "value"])))))

;; For practical Clojure+JNA, we use the raw invoke approach with known struct
;; layouts, reading memory directly.

;; ---------------------------------------------------------------------------
;; Direct invocation helpers
;; ---------------------------------------------------------------------------

(defn call-void
  "Call a void-returning libproven function."
  [^String name & args]
  (.invoke (get-fn name) Void/TYPE (into-array Object (or args []))))

(defn call-int
  "Call a function returning int32_t."
  ^long [^String name & args]
  (long (.invoke (get-fn name) Integer/TYPE (into-array Object (or args [])))))

(defn call-bool
  "Call a function returning bool."
  [^String name & args]
  (boolean (.invoke (get-fn name) Boolean/TYPE (into-array Object (or args [])))))

(defn call-double
  "Call a function returning double."
  ^double [^String name & args]
  (double (.invoke (get-fn name) Double/TYPE (into-array Object (or args [])))))

(defn call-long
  "Call a function returning int64_t / long."
  ^long [^String name & args]
  (long (.invoke (get-fn name) Long/TYPE (into-array Object (or args [])))))

;; ---------------------------------------------------------------------------
;; Struct-returning functions via pointer output parameter
;; ---------------------------------------------------------------------------
;; libproven returns structs by value in the C ABI. JNA handles this via
;; Structure.ByValue in Java. From Clojure, the easiest portable approach
;; is to allocate a Memory buffer, invoke the function, and read fields.
;;
;; IntResult layout: {int32_t status (offset 0), int64_t value (offset 8)}
;; (8-byte alignment for the int64_t field means 4 bytes padding after status)
;;
;; BoolResult layout: {int32_t status (offset 0), bool value (offset 4)}
;;
;; FloatResult layout: {int32_t status (offset 0), double value (offset 8)}
;;
;; StringResult layout: {int32_t status (0), Pointer value (8), long length (16)}

(defn call-int-result
  "Invoke a function returning IntResult. Returns the int64 value on success, nil on error.
  Uses JNA's ability to invoke functions that return struct-by-value through
  a hidden pointer parameter."
  [^String name & args]
  (let [buf (Memory. 16)
        f (get-fn name)
        ;; JNA can handle struct return via an output Memory parameter
        ;; prepended to the args list (sret convention).
        ;; However, this is platform-dependent. A more portable approach:
        ;; use JNA's Structure.ByValue mechanism.
        ;;
        ;; The simplest portable Clojure approach: invoke and capture the
        ;; return as a Pointer, then read fields.
        result (.invoke f Pointer/TYPE (into-array Object (or args [])))]
    ;; result is a Pointer to the struct (or the struct memory)
    ;; Read status at offset 0, value at offset 8
    (when result
      (let [status (.getInt ^Pointer result 0)]
        (when (zero? status)
          (.getLong ^Pointer result 8))))))

(defn call-bool-result
  "Invoke a function returning BoolResult. Returns Boolean on success, nil on error."
  [^String name & args]
  (let [result (.invoke (get-fn name) Pointer/TYPE (into-array Object (or args [])))]
    (when result
      (let [status (.getInt ^Pointer result 0)]
        (when (zero? status)
          (not (zero? (.getInt ^Pointer result 4))))))))

(defn call-float-result
  "Invoke a function returning FloatResult. Returns Double on success, nil on error."
  [^String name & args]
  (let [result (.invoke (get-fn name) Pointer/TYPE (into-array Object (or args [])))]
    (when result
      (let [status (.getInt ^Pointer result 0)]
        (when (zero? status)
          (.getDouble ^Pointer result 8))))))

(defn call-string-result
  "Invoke a function returning StringResult. Returns String on success, nil on error.
  Frees the native string via proven_free_string."
  [^String name & args]
  (let [result (.invoke (get-fn name) Pointer/TYPE (into-array Object (or args [])))]
    (when result
      (let [status (.getInt ^Pointer result 0)]
        (if (not (zero? status))
          (do
            ;; Free string pointer if non-null
            (let [str-ptr (.getPointer ^Pointer result 8)]
              (when str-ptr
                (call-void "proven_free_string" str-ptr)))
            nil)
          (let [str-ptr (.getPointer ^Pointer result 8)
                length  (.getLong ^Pointer result 16)]
            (if (or (nil? str-ptr) (zero? length))
              ""
              (let [bytes (.getByteArray str-ptr 0 (int length))
                    s (String. bytes StandardCharsets/UTF_8)]
                (call-void "proven_free_string" str-ptr)
                s))))))))

;; ---------------------------------------------------------------------------
;; Memory helpers
;; ---------------------------------------------------------------------------

(defn to-native-bytes
  "Copy a byte array into a JNA Memory block."
  ^Memory [^bytes data]
  (when (and data (pos? (alength data)))
    (let [mem (Memory. (alength data))]
      (.write mem 0 data 0 (alength data))
      mem)))

(defn to-native-string
  "Convert a String to a JNA Memory pointer (UTF-8 encoded)."
  ^Memory [^String s]
  (when (and s (not (.isEmpty s)))
    (to-native-bytes (.getBytes s StandardCharsets/UTF_8))))

(defn free-string
  "Free a string pointer allocated by libproven."
  [^Pointer ptr]
  (when ptr
    (call-void "proven_free_string" ptr)))
