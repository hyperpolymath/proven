# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Test the exact UTF-8 bytes emitted by Idris, bypassing sqlite3 CLI line editing.
# Uses only Julia's standard library and the installed SQLite shared library.
const SQLITE_LIBRARY = get(ENV, "SQLITE3_LIBRARY", "libsqlite3.so.0")

function execute_checks(path::String)
    sql = read(path, String)
    isempty(sql) && error("Empty SQL fixture")
    occursin('\0', sql) && error("Embedded NUL in SQL fixture")
    database = Ref{Ptr{Cvoid}}(C_NULL)
    open_status = ccall((:sqlite3_open, SQLITE_LIBRARY), Cint,
                        (Cstring, Ref{Ptr{Cvoid}}), ":memory:", database)
    try
        open_status == 0 || error("SQLite open failed: code $open_status")
        message = Ref{Ptr{UInt8}}(C_NULL)
        try
            status = ccall((:sqlite3_exec, SQLITE_LIBRARY), Cint,
                           (Ptr{Cvoid}, Cstring, Ptr{Cvoid}, Ptr{Cvoid}, Ref{Ptr{UInt8}}),
                           database[], sql, C_NULL, C_NULL, message)
            if status != 0
                detail = message[] == C_NULL ? "no diagnostic" : unsafe_string(message[])
                error("SQLite assertions failed (code $status): $detail")
            end
        finally
            if message[] != C_NULL
                ccall((:sqlite3_free, SQLITE_LIBRARY), Cvoid, (Ptr{Cvoid},), message[])
            end
        end
    finally
        if database[] != C_NULL
            close_status = ccall((:sqlite3_close, SQLITE_LIBRARY), Cint, (Ptr{Cvoid},), database[])
            close_status == 0 || error("SQLite close failed: code $close_status")
        end
    end
    println("PASS: SQLite exact-byte literal checks, case count and sentinel preservation")
end

length(ARGS) == 1 || error("Usage: julia sqlite-literals.jl <generated.sql>")
execute_checks(ARGS[1])
