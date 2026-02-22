// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeUrl -- Safe URL parsing and validation via libproven JNA FFI.
///
/// Provides URL parsing into components (scheme, host, port, path, query,
/// fragment). All computation is performed by the Idris 2 verified
/// implementation. No URL parsing logic is reimplemented in Flix.
///
mod Proven.SafeUrl {

    use Proven.LibProven.getFunction;

    ///
    /// Parsed URL components.
    ///
    pub enum UrlComponents(
        {scheme = String, host = String, port = Option[Int32],
         path = String, query = String, fragment = String}
    )

    ///
    /// Get the scheme from a UrlComponents.
    ///
    pub def getScheme(uc: UrlComponents): String =
        let UrlComponents(r) = uc;
        r#scheme

    ///
    /// Get the host from a UrlComponents.
    ///
    pub def getHost(uc: UrlComponents): String =
        let UrlComponents(r) = uc;
        r#host

    ///
    /// Get the port from a UrlComponents.
    ///
    pub def getPort(uc: UrlComponents): Option[Int32] =
        let UrlComponents(r) = uc;
        r#port

    ///
    /// Get the path from a UrlComponents.
    ///
    pub def getPath(uc: UrlComponents): String =
        let UrlComponents(r) = uc;
        r#path

    ///
    /// Parse a URL into its components.
    /// Returns `None` if the URL is malformed.
    ///
    /// Delegates to `proven_url_parse` via JNA. The ProvenUrlResult struct
    /// is read field-by-field from the returned pointer, and all C-allocated
    /// strings are freed via `proven_url_free`.
    ///
    pub def parseUrl(url: String): Option[UrlComponents] \ IO =
        region rc {
            let func = getFunction("proven_url_parse");
            let bytes = String.getBytes(url);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj} @ rc;
            import com.sun.jna.Function.invokePointer(Array[##java.lang.Object, rc]): ##com.sun.jna.Pointer \ IO;
            let resultPtr = invokePointer(func, args);
            import com.sun.jna.Pointer.getInt(Int64): Int32 \ IO;
            let status = getInt(resultPtr, 0i64);
            if (status == 0i32) {
                // Read components from the struct via pointer offsets.
                // The UrlComponents struct fields are at known offsets.
                import com.sun.jna.Pointer.getString(Int64): String \ IO;
                import com.sun.jna.Pointer.getPointer(Int64): ##com.sun.jna.Pointer \ IO;
                import com.sun.jna.Pointer.getShort(Int64): Int16 \ IO;

                // Read scheme (ptr at offset 8, len at offset 16)
                let schemePtr = getPointer(resultPtr, 8i64);
                let schemeStr = if (Object.isNull(schemePtr)) "" else getString(schemePtr, 0i64);

                // Read host (ptr at offset 24, len at offset 32)
                let hostPtr = getPointer(resultPtr, 24i64);
                let hostStr = if (Object.isNull(hostPtr)) "" else getString(hostPtr, 0i64);

                // Read port (uint16 at offset 40, has_port bool at offset 42)
                let portVal = getShort(resultPtr, 40i64);
                let hasPort = getInt(resultPtr, 42i64);
                let portOpt = if (hasPort != 0i32) Some(Int16.toInt32(portVal)) else None;

                // Read path (ptr at offset 48, len at offset 56)
                let pathPtr = getPointer(resultPtr, 48i64);
                let pathStr = if (Object.isNull(pathPtr)) "" else getString(pathPtr, 0i64);

                // Read query (ptr at offset 64, len at offset 72)
                let queryPtr = getPointer(resultPtr, 64i64);
                let queryStr = if (Object.isNull(queryPtr)) "" else getString(queryPtr, 0i64);

                // Read fragment (ptr at offset 80, len at offset 88)
                let fragPtr = getPointer(resultPtr, 80i64);
                let fragStr = if (Object.isNull(fragPtr)) "" else getString(fragPtr, 0i64);

                // Free the URL components via proven_url_free
                // (pass pointer to the components portion, offset 8 from result)
                let freeFunc = getFunction("proven_url_free");
                import com.sun.jna.Pointer.share(Int64): ##com.sun.jna.Pointer \ IO;
                let compPtr = share(resultPtr, 8i64);
                let freeArgs: Array[##java.lang.Object, rc] = Array#{checked_cast(compPtr)} @ rc;
                import com.sun.jna.Function.invoke(Array[##java.lang.Object, rc]): ##java.lang.Object \ IO;
                let _ = invoke(freeFunc, freeArgs);

                Some(UrlComponents({
                    scheme = schemeStr,
                    host = hostStr,
                    port = portOpt,
                    path = pathStr,
                    query = queryStr,
                    fragment = fragStr
                }))
            } else {
                None
            }
        }

    ///
    /// Extract just the scheme from a URL (e.g., "https").
    /// Returns `None` if the URL is malformed.
    ///
    pub def scheme(url: String): Option[String] \ IO =
        match parseUrl(url) {
            case Some(uc) =>
                let s = getScheme(uc);
                if (String.isEmpty(s)) None else Some(s)
            case None => None
        }

    ///
    /// Extract just the host from a URL.
    /// Returns `None` if the URL is malformed.
    ///
    pub def host(url: String): Option[String] \ IO =
        match parseUrl(url) {
            case Some(uc) =>
                let h = getHost(uc);
                if (String.isEmpty(h)) None else Some(h)
            case None => None
        }

}
