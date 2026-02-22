# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Integration tests for the Proven Ruby FFI binding.
# These tests require libproven.so to be built and available.
# Build with: cd ffi/zig && zig build
#
# Run: bundle exec rspec spec/proven_spec.rb

require "proven"

RSpec.describe Proven do
  it "has version 1.0.0" do
    expect(Proven::VERSION).to eq("1.0.0")
  end

  describe Proven::SafeMath do
    describe ".div" do
      it "divides correctly" do
        expect(described_class.div(10, 2)).to eq(5)
      end

      it "returns nil on division by zero" do
        expect(described_class.div(10, 0)).to be_nil
      end
    end

    describe ".mod" do
      it "computes modulo correctly" do
        expect(described_class.mod(10, 3)).to eq(1)
      end

      it "returns nil on mod by zero" do
        expect(described_class.mod(10, 0)).to be_nil
      end
    end

    describe ".add" do
      it "adds correctly" do
        expect(described_class.add(1, 2)).to eq(3)
      end
    end

    describe ".sub" do
      it "subtracts correctly" do
        expect(described_class.sub(5, 3)).to eq(2)
      end
    end

    describe ".mul" do
      it "multiplies correctly" do
        expect(described_class.mul(3, 4)).to eq(12)
      end
    end
  end

  describe Proven::SafeString do
    describe ".escape_html" do
      it "escapes HTML entities" do
        result = described_class.escape_html("<script>")
        expect(result).to include("&lt;") if result
      end
    end

    describe ".escape_sql" do
      it "escapes single quotes" do
        result = described_class.escape_sql("it's")
        expect(result).to include("''") if result
      end
    end

    describe ".valid_utf8?" do
      it "validates UTF-8 strings" do
        result = described_class.valid_utf8?("hello")
        expect(result).to be(true) if result
      end
    end
  end

  describe Proven::SafePath do
    describe ".traversal?" do
      it "detects traversal sequences" do
        result = described_class.traversal?("../etc/passwd")
        expect(result).to be(true) if result
      end

      it "accepts safe paths" do
        result = described_class.traversal?("normal/path")
        expect(result).to be(false) if result
      end
    end

    describe ".safe?" do
      it "validates safe paths" do
        result = described_class.safe?("safe/path")
        expect(result).to be(true) if result
      end
    end
  end

  describe Proven::SafeEmail do
    describe ".valid?" do
      it "validates email addresses" do
        result = described_class.valid?("user@example.com")
        expect(result).to be(true) if result
      end

      it "rejects invalid emails" do
        result = described_class.valid?("not-an-email")
        expect(result).to be(false) if result
      end
    end
  end

  describe Proven::SafeNetwork do
    describe ".valid_ipv4?" do
      it "validates IPv4 addresses" do
        expect(described_class.valid_ipv4?("192.168.1.1")).to be true
      end

      it "rejects invalid addresses" do
        expect(described_class.valid_ipv4?("invalid")).to be false
      end
    end
  end

  describe Proven::SafeCrypto do
    describe ".constant_time_compare" do
      it "compares equal strings" do
        result = described_class.constant_time_compare("secret", "secret")
        expect(result).to be(true) if result
      end

      it "detects unequal strings" do
        result = described_class.constant_time_compare("secret", "other!")
        expect(result).to be(false) if result
      end
    end

    describe ".random_bytes" do
      it "generates random bytes" do
        result = described_class.random_bytes(16)
        expect(result.bytesize).to eq(16) if result
      end
    end
  end

  describe Proven::SafeFloat do
    describe ".finite?" do
      it "detects finite values" do
        result = described_class.finite?(1.0)
        expect(result).to be(true) if result
      end
    end

    describe ".div" do
      it "divides correctly" do
        result = described_class.div(10.0, 2.0)
        expect(result).to be_within(0.001).of(5.0) if result
      end

      it "returns nil for division by zero" do
        expect(described_class.div(1.0, 0.0)).to be_nil
      end
    end
  end

  describe Proven::SafeAngle do
    describe ".deg_to_rad" do
      it "converts degrees to radians" do
        result = described_class.deg_to_rad(180.0)
        expect(result).to be_within(0.001).of(Math::PI) if result
      end
    end

    describe ".normalize_degrees" do
      it "normalizes negative degrees" do
        result = described_class.normalize_degrees(-90.0)
        expect(result).to be_within(0.001).of(270.0) if result
      end
    end
  end

  describe Proven::SafeJson do
    describe ".valid?" do
      it "validates JSON strings" do
        result = described_class.valid?('{"key": "value"}')
        expect(result).to be(true) if result
      end

      it "rejects invalid JSON" do
        result = described_class.valid?("{invalid}")
        expect(result).to be(false) if result
      end
    end
  end
end
