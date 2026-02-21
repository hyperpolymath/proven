% SPDX-License-Identifier: PMPL-1.0-or-later
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeEmail - Email validation and parsing for Prolog
% RFC 5321/5322 compliant email handling.

:- module(safe_email, [
    is_valid_email/1,
    parse_email/3,
    is_disposable_email/1,
    normalize_email/2,
    email_local_part/2,
    email_domain/2,
    email_to_string/2,
    is_valid_local_part/1,
    is_valid_domain/1
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
disposable_domain("yopmail.com").
disposable_domain("sharklasers.com").
disposable_domain("dispostable.com").
disposable_domain("getnada.com").

%! is_valid_email(+Email) is semidet.
%
%  Validate email format.
%
%  @arg Email Email address to validate
is_valid_email(Email) :-
    parse_email(Email, _, _).

%! parse_email(+Email, -LocalPart, -Domain) is semidet.
%
%  Parse email into local part and domain.
%
%  @arg Email Email address
%  @arg LocalPart Local part (before @)
%  @arg Domain Domain part (after @)
parse_email(Email, LocalPart, Domain) :-
    atom_string(Email, EmailStr),
    split_string(EmailStr, "@", "", Parts),
    Parts = [LocalStr, DomainStr],
    LocalStr \= "",
    DomainStr \= "",
    is_valid_local_part_str(LocalStr),
    is_valid_domain_str(DomainStr),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr).

%! is_valid_local_part(+LocalPart) is semidet.
%
%  Validate email local part.
%
%  @arg LocalPart Local part string
is_valid_local_part(LocalPart) :-
    atom_string(LocalPart, LocalStr),
    is_valid_local_part_str(LocalStr).

is_valid_local_part_str(Local) :-
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
is_local_char('!').
is_local_char('#').
is_local_char('$').
is_local_char('%').
is_local_char('&').
is_local_char('*').
is_local_char('/').
is_local_char('?').
is_local_char('^').
is_local_char('`').
is_local_char('{').
is_local_char('|').
is_local_char('}').
is_local_char('~').

%! is_valid_domain(+Domain) is semidet.
%
%  Validate email domain.
%
%  @arg Domain Domain string
is_valid_domain(Domain) :-
    atom_string(Domain, DomainStr),
    is_valid_domain_str(DomainStr).

is_valid_domain_str(Domain) :-
    string_length(Domain, Len),
    Len > 0,
    Len =< 253,
    sub_string(Domain, _, _, _, "."),
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

%! is_disposable_email(+Email) is semidet.
%
%  Check if email is from a known disposable domain.
%
%  @arg Email Email address
is_disposable_email(Email) :-
    parse_email(Email, _, Domain),
    atom_string(Domain, DomainStr),
    string_lower(DomainStr, LowerDomain),
    disposable_domain(LowerDomain).

%! normalize_email(+Email, -Normalized) is det.
%
%  Normalize email (lowercase domain, preserve local part case).
%
%  @arg Email Email address
%  @arg Normalized Normalized email
normalize_email(Email, Normalized) :-
    parse_email(Email, LocalPart, Domain),
    atom_string(LocalPart, LocalStr),
    atom_string(Domain, DomainStr),
    string_lower(DomainStr, LowerDomain),
    format(string(NormStr), "~s@~s", [LocalStr, LowerDomain]),
    atom_string(Normalized, NormStr).

%! email_local_part(+Email, -LocalPart) is semidet.
%
%  Extract local part from email.
%
%  @arg Email Email address
%  @arg LocalPart Local part
email_local_part(Email, LocalPart) :-
    parse_email(Email, LocalPart, _).

%! email_domain(+Email, -Domain) is semidet.
%
%  Extract domain from email.
%
%  @arg Email Email address
%  @arg Domain Domain part
email_domain(Email, Domain) :-
    parse_email(Email, _, Domain).

%! email_to_string(+Email, -String) is det.
%
%  Convert email term to string.
%
%  @arg Email Email term email(LocalPart, Domain) or atom
%  @arg String String representation
email_to_string(email(LocalPart, Domain), String) :- !,
    format(atom(String), "~w@~w", [LocalPart, Domain]).
email_to_string(Email, String) :-
    atom_string(Email, String).
