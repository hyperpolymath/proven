# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Proven',
    VERSION_FROM     => 'lib/Proven.pm',
    ABSTRACT         => 'FFI binding to libproven - formally verified safety library',
    AUTHOR           => 'Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>',
    LICENSE          => 'open_source',
    MIN_PERL_VERSION => '5.020',
    PREREQ_PM        => {
        'FFI::Platypus'        => '2.00',
        'FFI::Platypus::Buffer' => 0,
        'Exporter'              => 0,
    },
    TEST_REQUIRES    => {
        'Test::More' => 0,
    },
    META_MERGE       => {
        'meta-spec' => { version => 2 },
        resources   => {
            repository => {
                type => 'git',
                url  => 'https://github.com/hyperpolymath/proven.git',
                web  => 'https://github.com/hyperpolymath/proven',
            },
        },
        keywords => [
            'safety', 'security', 'validation', 'ffi',
            'formally-verified', 'idris2', 'libproven'
        ],
    },
);
