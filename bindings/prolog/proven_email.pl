%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeEmail - FFI bindings to libproven email validation.
%% All validation is performed in verified Idris 2 code via libproven.

:- module(proven_email, [
    is_valid_email/1,
    parse_email/3,
    normalize_email/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations
:- foreign(proven_email_is_valid_ffi, c,
           proven_email_is_valid(+string, +integer, [-integer], [-integer])).

%! is_valid_email(+Email) is semidet.
%
%  Validate email address via libproven (RFC 5321 simplified).
is_valid_email(Email) :-
    atom_string(Email, EmailStr),
    atom_length(Email, Len),
    proven_email_is_valid_ffi(EmailStr, Len, Status, Value),
    Status =:= 0,
    Value =:= 1.

%! parse_email(+Email, -LocalPart, -Domain) is semidet.
%
%  Parse email into local part and domain.
%  Validates via libproven first, then splits on @.
parse_email(Email, LocalPart, Domain) :-
    is_valid_email(Email),
    atom_string(Email, EmailStr),
    split_string(EmailStr, "@", "", [LocalStr, DomainStr]),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr).

%! normalize_email(+Email, -Normalized) is det.
%
%  Normalize email: validate via libproven, lowercase domain.
normalize_email(Email, Normalized) :-
    parse_email(Email, LocalPart, Domain),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr),
    string_lower(DomainStr, LowerDomain),
    format(string(NormStr), "~s@~s", [LocalStr, LowerDomain]),
    atom_string(Normalized, NormStr).
