# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeJson;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse stringify is_valid get_value set_value
    get_type merge deep_clone validate_schema
    pretty_print minify
);

=head1 NAME

Proven::SafeJson - Safe JSON parsing and manipulation without exceptions

=head1 SYNOPSIS

    use Proven::SafeJson qw(parse stringify is_valid);

    my $data = parse('{"name": "test", "value": 42}');
    if (defined $data) {
        my $json = stringify($data, { pretty => 1 });
    }

=head1 DESCRIPTION

Provides safe JSON operations that return undef on error instead
of throwing exceptions. Uses JSON::PP for compatibility.

=cut

# Lazy load JSON module
my $json_module;

sub _get_json_encoder {
    return $json_module if defined $json_module;

    eval { require JSON::XS; $json_module = JSON::XS->new->utf8->allow_nonref; };
    return $json_module if defined $json_module;

    eval { require JSON::PP; $json_module = JSON::PP->new->utf8->allow_nonref; };
    return $json_module if defined $json_module;

    eval { require JSON; $json_module = JSON->new->utf8->allow_nonref; };
    return $json_module;
}

=head2 parse($json_string, $options)

Parse a JSON string into a Perl data structure.
Returns undef if parsing fails.

Options:
    max_depth => N    Maximum nesting depth (default: 512)
    max_size  => N    Maximum string size in bytes (default: 10MB)

=cut

sub parse {
    my ($json_string, $options) = @_;
    return undef unless defined $json_string;

    $options //= {};
    my $max_depth = $options->{max_depth} // 512;
    my $max_size = $options->{max_size} // 10 * 1024 * 1024;

    # Size check
    return undef if length($json_string) > $max_size;

    my $encoder = _get_json_encoder();
    return undef unless defined $encoder;

    my $result;
    eval {
        $encoder->max_depth($max_depth) if $encoder->can('max_depth');
        $result = $encoder->decode($json_string);
    };

    return $@ ? undef : $result;
}

=head2 stringify($data, $options)

Convert a Perl data structure to JSON string.
Returns undef if conversion fails.

Options:
    pretty    => 1    Pretty-print with indentation
    canonical => 1    Sort keys for consistent output

=cut

sub stringify {
    my ($data, $options) = @_;
    $options //= {};

    my $encoder = _get_json_encoder();
    return undef unless defined $encoder;

    my $result;
    eval {
        my $local_encoder = $encoder;
        if ($options->{pretty}) {
            $local_encoder = $local_encoder->pretty(1) if $local_encoder->can('pretty');
        }
        if ($options->{canonical}) {
            $local_encoder = $local_encoder->canonical(1) if $local_encoder->can('canonical');
        }
        $result = $local_encoder->encode($data);
    };

    return $@ ? undef : $result;
}

=head2 is_valid($json_string)

Check if a string is valid JSON.

=cut

sub is_valid {
    my ($json_string) = @_;
    return defined parse($json_string);
}

=head2 get_value($data, $path)

Get a value from a nested structure using dot notation.
Path example: "user.address.city" or "items.0.name"

=cut

sub get_value {
    my ($data, $path) = @_;
    return undef unless defined $data && defined $path;

    my @parts = split /\./, $path;
    my $current = $data;

    for my $part (@parts) {
        if (ref($current) eq 'HASH') {
            return undef unless exists $current->{$part};
            $current = $current->{$part};
        } elsif (ref($current) eq 'ARRAY') {
            return undef unless $part =~ /^\d+$/;
            my $index = int($part);
            return undef if $index < 0 || $index >= @$current;
            $current = $current->[$index];
        } else {
            return undef;
        }
    }

    return $current;
}

=head2 set_value($data, $path, $value)

Set a value in a nested structure using dot notation.
Returns the modified data, or undef if the path is invalid.

=cut

sub set_value {
    my ($data, $path, $value) = @_;
    return undef unless defined $data && defined $path;

    my @parts = split /\./, $path;
    my $last_key = pop @parts;
    my $current = $data;

    for my $part (@parts) {
        if (ref($current) eq 'HASH') {
            $current->{$part} //= {};
            $current = $current->{$part};
        } elsif (ref($current) eq 'ARRAY') {
            return undef unless $part =~ /^\d+$/;
            my $index = int($part);
            return undef if $index < 0;
            $current->[$index] //= {};
            $current = $current->[$index];
        } else {
            return undef;
        }
    }

    if (ref($current) eq 'HASH') {
        $current->{$last_key} = $value;
    } elsif (ref($current) eq 'ARRAY') {
        return undef unless $last_key =~ /^\d+$/;
        $current->[int($last_key)] = $value;
    } else {
        return undef;
    }

    return $data;
}

=head2 get_type($value)

Get the JSON type of a Perl value.
Returns: 'null', 'boolean', 'number', 'string', 'array', 'object'

=cut

sub get_type {
    my ($value) = @_;

    return 'null' unless defined $value;

    my $ref = ref($value);

    if ($ref eq 'HASH') {
        return 'object';
    } elsif ($ref eq 'ARRAY') {
        return 'array';
    } elsif ($ref eq 'JSON::PP::Boolean' || $ref eq 'JSON::XS::Boolean') {
        return 'boolean';
    } elsif ($ref eq '') {
        # Scalar - check if numeric
        if ($value =~ /^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?$/) {
            return 'number';
        }
        return 'string';
    }

    return 'object';  # Other references treated as objects
}

=head2 merge($base, $overlay)

Deep merge two data structures.
Overlay values take precedence.

=cut

sub merge {
    my ($base, $overlay) = @_;

    return deep_clone($overlay) unless defined $base;
    return deep_clone($base) unless defined $overlay;

    if (ref($base) eq 'HASH' && ref($overlay) eq 'HASH') {
        my %result = %$base;
        for my $key (keys %$overlay) {
            if (exists $result{$key}) {
                $result{$key} = merge($result{$key}, $overlay->{$key});
            } else {
                $result{$key} = deep_clone($overlay->{$key});
            }
        }
        return \%result;
    } elsif (ref($base) eq 'ARRAY' && ref($overlay) eq 'ARRAY') {
        return deep_clone($overlay);
    } else {
        return deep_clone($overlay);
    }
}

=head2 deep_clone($data)

Create a deep copy of a data structure.

=cut

sub deep_clone {
    my ($data) = @_;
    return undef unless defined $data;

    my $ref = ref($data);

    if ($ref eq 'HASH') {
        return { map { $_ => deep_clone($data->{$_}) } keys %$data };
    } elsif ($ref eq 'ARRAY') {
        return [ map { deep_clone($_) } @$data ];
    } elsif ($ref eq '') {
        return $data;
    } else {
        # For other references, try JSON round-trip
        my $json = stringify($data);
        return parse($json);
    }
}

=head2 validate_schema($data, $schema)

Basic JSON schema validation.
Returns 1 if valid, 0 otherwise.

Supports: type, required, properties, items, enum, minimum, maximum, minLength, maxLength

=cut

sub validate_schema {
    my ($data, $schema) = @_;
    return 1 unless defined $schema;

    my $schema_ref = ref($schema) eq 'HASH' ? $schema : parse($schema);
    return 0 unless defined $schema_ref;

    return _validate_node($data, $schema_ref);
}

sub _validate_node {
    my ($data, $schema) = @_;

    # Type check
    if (exists $schema->{type}) {
        my $actual_type = get_type($data);
        my $expected = $schema->{type};

        if (ref($expected) eq 'ARRAY') {
            return 0 unless grep { $_ eq $actual_type || ($_ eq 'integer' && $actual_type eq 'number') } @$expected;
        } else {
            return 0 unless $actual_type eq $expected || ($expected eq 'integer' && $actual_type eq 'number');
        }
    }

    # Enum check
    if (exists $schema->{enum}) {
        my $found = 0;
        for my $option (@{$schema->{enum}}) {
            if (!defined($option) && !defined($data)) {
                $found = 1;
                last;
            }
            if (defined($option) && defined($data) && $option eq $data) {
                $found = 1;
                last;
            }
        }
        return 0 unless $found;
    }

    # Object validation
    if (ref($data) eq 'HASH') {
        # Required fields
        if (exists $schema->{required}) {
            for my $field (@{$schema->{required}}) {
                return 0 unless exists $data->{$field};
            }
        }

        # Properties
        if (exists $schema->{properties}) {
            for my $key (keys %{$schema->{properties}}) {
                if (exists $data->{$key}) {
                    return 0 unless _validate_node($data->{$key}, $schema->{properties}{$key});
                }
            }
        }
    }

    # Array validation
    if (ref($data) eq 'ARRAY') {
        if (exists $schema->{items}) {
            for my $item (@$data) {
                return 0 unless _validate_node($item, $schema->{items});
            }
        }
        if (exists $schema->{minItems}) {
            return 0 if @$data < $schema->{minItems};
        }
        if (exists $schema->{maxItems}) {
            return 0 if @$data > $schema->{maxItems};
        }
    }

    # String validation
    if (!ref($data) && defined($data)) {
        if (exists $schema->{minLength}) {
            return 0 if length($data) < $schema->{minLength};
        }
        if (exists $schema->{maxLength}) {
            return 0 if length($data) > $schema->{maxLength};
        }
        if (exists $schema->{pattern}) {
            my $pattern = $schema->{pattern};
            return 0 unless $data =~ /$pattern/;
        }
    }

    # Number validation
    if (get_type($data) eq 'number') {
        if (exists $schema->{minimum}) {
            return 0 if $data < $schema->{minimum};
        }
        if (exists $schema->{maximum}) {
            return 0 if $data > $schema->{maximum};
        }
    }

    return 1;
}

=head2 pretty_print($data)

Pretty-print JSON with 2-space indentation.

=cut

sub pretty_print {
    my ($data) = @_;
    return stringify($data, { pretty => 1 });
}

=head2 minify($json_string)

Minify a JSON string by removing whitespace.

=cut

sub minify {
    my ($json_string) = @_;
    my $data = parse($json_string);
    return undef unless defined $data;
    return stringify($data);
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
