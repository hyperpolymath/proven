# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Proven',
    VERSION_FROM     => 'lib/Proven.pm',
    ABSTRACT         => 'Code that cannot crash - safe arithmetic, string escaping, path validation',
    AUTHOR           => 'Hyperpolymath',
    LICENSE          => 'open_source',
    MIN_PERL_VERSION => '5.014',
    PREREQ_PM        => {
        'Encode'         => 0,
        'File::Spec'     => 0,
        'File::Basename' => 0,
        'Cwd'            => 0,
        'Exporter'       => 0,
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
            'safety', 'security', 'validation', 'escaping',
            'overflow', 'xss', 'sql-injection', 'path-traversal'
        ],
    },
);
