# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "proven"

RSpec.describe Proven do
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
        expect(described_class.escape_html("<script>")).to eq("&lt;script&gt;")
        expect(described_class.escape_html("a & b")).to eq("a &amp; b")
        expect(described_class.escape_html('"quoted"')).to eq("&quot;quoted&quot;")
      end
    end

    describe ".escape_sql" do
      it "escapes single quotes" do
        expect(described_class.escape_sql("it's")).to eq("it''s")
      end
    end

    describe ".escape_js" do
      it "escapes JavaScript special characters" do
        expect(described_class.escape_js("line\nbreak")).to eq("line\\nbreak")
        expect(described_class.escape_js("tab\there")).to eq("tab\\there")
      end
    end

    describe ".truncate_safe" do
      it "truncates with suffix" do
        expect(described_class.truncate_safe("hello world", 5)).to eq("he...")
        expect(described_class.truncate_safe("hi", 10)).to eq("hi")
      end
    end
  end

  describe Proven::SafePath do
    describe ".traversal?" do
      it "detects traversal sequences" do
        expect(described_class.traversal?("../etc/passwd")).to be true
        expect(described_class.traversal?("~/file")).to be true
        expect(described_class.traversal?("normal/path")).to be false
      end
    end

    describe ".safe?" do
      it "validates safe paths" do
        expect(described_class.safe?("safe/path")).to be true
        expect(described_class.safe?("../unsafe")).to be false
      end
    end

    describe ".sanitize_filename" do
      it "removes dangerous characters" do
        expect(described_class.sanitize_filename("file<>name")).to eq("file__name")
        expect(described_class.sanitize_filename("..secret")).to eq("__secret")
      end
    end

    describe ".safe_join" do
      it "joins paths safely" do
        expect(described_class.safe_join("/base", "a", "b")).to eq("/base/a/b")
        expect(described_class.safe_join("/base", "../etc")).to be_nil
      end
    end
  end

  describe Proven::SafeEmail do
    describe ".valid?" do
      it "validates email addresses" do
        expect(described_class.valid?("user@example.com")).to be true
        expect(described_class.valid?("not-an-email")).to be false
        expect(described_class.valid?("@invalid.com")).to be false
        expect(described_class.valid?("user@.com")).to be false
      end
    end

    describe ".split" do
      it "parses email parts" do
        parts = described_class.split("user@example.com")
        expect(parts.local_part).to eq("user")
        expect(parts.domain).to eq("example.com")
      end
    end

    describe ".normalize" do
      it "lowercases domain" do
        expect(described_class.normalize("User@EXAMPLE.COM")).to eq("User@example.com")
      end
    end
  end

  describe Proven::SafeNetwork do
    describe ".valid_ipv4?" do
      it "validates IPv4 addresses" do
        expect(described_class.valid_ipv4?("192.168.1.1")).to be true
        expect(described_class.valid_ipv4?("invalid")).to be false
        expect(described_class.valid_ipv4?("256.1.1.1")).to be false
      end
    end

    describe ".private?" do
      it "detects private addresses" do
        expect(described_class.private?("192.168.1.1")).to be true
        expect(described_class.private?("10.0.0.1")).to be true
        expect(described_class.private?("172.16.0.1")).to be true
        expect(described_class.private?("8.8.8.8")).to be false
      end
    end

    describe ".loopback?" do
      it "detects loopback addresses" do
        expect(described_class.loopback?("127.0.0.1")).to be true
        expect(described_class.loopback?("192.168.1.1")).to be false
      end
    end

    describe ".public?" do
      it "detects public addresses" do
        expect(described_class.public?("8.8.8.8")).to be true
        expect(described_class.public?("192.168.1.1")).to be false
      end
    end
  end

  describe Proven::SafeCrypto do
    describe ".constant_time_compare" do
      it "compares strings correctly" do
        expect(described_class.constant_time_compare("secret", "secret")).to be true
        expect(described_class.constant_time_compare("secret", "other!")).to be false
        expect(described_class.constant_time_compare("", "")).to be true
      end
    end

    describe ".secure_zero" do
      it "creates zeroed string" do
        zeroed = described_class.secure_zero(4)
        expect(zeroed.bytesize).to eq(4)
        expect(zeroed).to eq("\x00\x00\x00\x00")
      end
    end
  end
end
