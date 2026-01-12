%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeEmail - Email validation for Prolog

:- module(proven_email, [
    is_valid_email/1,
    parse_email/3,
    is_disposable_email/1,
    normalize_email/2
]).

:- use_module(library(lists)).

%% Disposable email domains
disposable_domain("tempmail.com").
disposable_domain("throwaway.com").
disposable_domain("mailinator.com").
disposable_domain("guerrillamail.com").
disposable_domain("10minutemail.com").
disposable_domain("trashmail.com").
disposable_domain("fakeinbox.com").
disposable_domain("tempinbox.com").

%% is_valid_email(+Email)
%% Succeeds if Email is a valid email address
is_valid_email(Email) :-
    parse_email(Email, _, _).

%% parse_email(+Email, -LocalPart, -Domain)
%% Parse email into local part and domain
parse_email(Email, LocalPart, Domain) :-
    atom_string(Email, EmailStr),
    split_string(EmailStr, "@", "", Parts),
    Parts = [LocalStr, DomainStr],
    LocalStr \= "",
    DomainStr \= "",
    valid_local_part(LocalStr),
    valid_domain(DomainStr),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr).

%% valid_local_part(+Local)
valid_local_part(Local) :-
    string_length(Local, Len),
    Len > 0,
    Len =< 64,
    string_chars(Local, Chars),
    Chars = [First|_],
    First \= '.',
    last(Chars, Last),
    Last \= '.',
    \+ sub_string(Local, _, _, _, ".."),
    forall(member(C, Chars), is_local_char(C)).

is_local_char(C) :- char_type(C, alnum), !.
is_local_char('.').
is_local_char('_').
is_local_char('-').
is_local_char('+').
is_local_char('=').

%% valid_domain(+Domain)
valid_domain(Domain) :-
    string_length(Domain, Len),
    Len > 0,
    Len =< 253,
    sub_string(Domain, _, _, _, "."),  % Must contain at least one dot
    string_chars(Domain, Chars),
    Chars = [First|_],
    First \= '.',
    First \= '-',
    last(Chars, Last),
    Last \= '.',
    Last \= '-',
    \+ sub_string(Domain, _, _, _, ".."),
    \+ sub_string(Domain, _, _, _, ".-"),
    \+ sub_string(Domain, _, _, _, "-."),
    forall(member(C, Chars), is_domain_char(C)).

is_domain_char(C) :- char_type(C, alnum), !.
is_domain_char('.').
is_domain_char('-').

%% is_disposable_email(+Email)
%% Succeeds if Email is from a disposable domain
is_disposable_email(Email) :-
    parse_email(Email, _, Domain),
    atom_string(Domain, DomainStr),
    string_lower(DomainStr, LowerDomain),
    disposable_domain(LowerDomain).

%% normalize_email(+Email, -Normalized)
%% Normalize email to lowercase domain
normalize_email(Email, Normalized) :-
    parse_email(Email, LocalPart, Domain),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr),
    string_lower(DomainStr, LowerDomain),
    format(string(NormStr), "~s@~s", [LocalStr, LowerDomain]),
    atom_string(Normalized, NormStr).
