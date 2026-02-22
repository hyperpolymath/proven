// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeUrl - Safe URL parsing and validation wrapper for libproven.
//
// Provides URL component parsing via the formally verified Idris 2 core.
// This unit ONLY wraps FFI calls; it does NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeUrl;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // Parsed URL components in Pascal-native string form.
  TUrlComponents = record
    Scheme: AnsiString;
    Host: AnsiString;
    Port: UInt16;
    HasPort: Boolean;
    Path: AnsiString;
    Query: AnsiString;
    Fragment: AnsiString;
  end;

  // High-level result for URL parsing.
  TSafeUrlResult = record
    Success: Boolean;
    Components: TUrlComponents;
    ErrorCode: Int32;
  end;

// Parse a URL string into components.
// Calls proven_url_parse via FFI and frees the C-allocated components.
function ParseUrl(const Url: AnsiString): TSafeUrlResult;

implementation

function ParseUrl(const Url: AnsiString): TSafeUrlResult;
var
  R: TProvenUrlResult;
begin
  if Length(Url) = 0 then
  begin
    Result.Success := False;
    Result.ErrorCode := PROVEN_ERR_INVALID_ARGUMENT;
    Result.Components.Scheme := '';
    Result.Components.Host := '';
    Result.Components.Port := 0;
    Result.Components.HasPort := False;
    Result.Components.Path := '';
    Result.Components.Query := '';
    Result.Components.Fragment := '';
    Exit;
  end;

  R := proven_url_parse(PByte(PAnsiChar(Url)), NativeUInt(Length(Url)));
  Result.Success := (R.Status = PROVEN_OK);

  if Result.Success then
  begin
    Result.ErrorCode := PROVEN_OK;

    // Copy each component into Pascal strings.
    if (R.Components.Scheme <> nil) and (R.Components.SchemeLen > 0) then
      SetString(Result.Components.Scheme, R.Components.Scheme, R.Components.SchemeLen)
    else
      Result.Components.Scheme := '';

    if (R.Components.Host <> nil) and (R.Components.HostLen > 0) then
      SetString(Result.Components.Host, R.Components.Host, R.Components.HostLen)
    else
      Result.Components.Host := '';

    Result.Components.Port := R.Components.Port;
    Result.Components.HasPort := Boolean(R.Components.HasPort);

    if (R.Components.Path <> nil) and (R.Components.PathLen > 0) then
      SetString(Result.Components.Path, R.Components.Path, R.Components.PathLen)
    else
      Result.Components.Path := '';

    if (R.Components.Query <> nil) and (R.Components.QueryLen > 0) then
      SetString(Result.Components.Query, R.Components.Query, R.Components.QueryLen)
    else
      Result.Components.Query := '';

    if (R.Components.Fragment <> nil) and (R.Components.FragmentLen > 0) then
      SetString(Result.Components.Fragment, R.Components.Fragment, R.Components.FragmentLen)
    else
      Result.Components.Fragment := '';

    // Free the C-allocated URL components.
    proven_url_free(@R.Components);
  end
  else
  begin
    Result.ErrorCode := R.Status;
    Result.Components.Scheme := '';
    Result.Components.Host := '';
    Result.Components.Port := 0;
    Result.Components.HasPort := False;
    Result.Components.Path := '';
    Result.Components.Query := '';
    Result.Components.Fragment := '';
  end;
end;

end.
