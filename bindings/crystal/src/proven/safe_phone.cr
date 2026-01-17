# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Country calling codes per ITU-T E.164.
  enum CountryCode
    Unknown = 0
    Us      = 1   # USA, Canada, Caribbean
    Ru      = 7   # Russia, Kazakhstan
    Eg      = 20  # Egypt
    Za      = 27  # South Africa
    Gr      = 30  # Greece
    Nl      = 31  # Netherlands
    Be      = 32  # Belgium
    Fr      = 33  # France
    Es      = 34  # Spain
    Hu      = 36  # Hungary
    It      = 39  # Italy
    Ro      = 40  # Romania
    Ch      = 41  # Switzerland
    At      = 43  # Austria
    Uk      = 44  # United Kingdom
    Dk      = 45  # Denmark
    Se      = 46  # Sweden
    No      = 47  # Norway
    Pl      = 48  # Poland
    De      = 49  # Germany
    Mx      = 52  # Mexico
    Cu      = 53  # Cuba
    Ar      = 54  # Argentina
    Br      = 55  # Brazil
    Cl      = 56  # Chile
    Co      = 57  # Colombia
    Ve      = 58  # Venezuela
    My      = 60  # Malaysia
    Au      = 61  # Australia
    Id      = 62  # Indonesia
    Ph      = 63  # Philippines
    Nz      = 64  # New Zealand
    Sg      = 65  # Singapore
    Th      = 66  # Thailand
    Jp      = 81  # Japan
    Kr      = 82  # South Korea
    Vn      = 84  # Vietnam
    Cn      = 86  # China
    Tr      = 90  # Turkey
    In      = 91  # India
    Pk      = 92  # Pakistan
    Ae      = 971 # UAE
    Il      = 972 # Israel
    Hk      = 852 # Hong Kong
    Tw      = 886 # Taiwan

    # Get numeric value.
    def value : UInt16
      self.to_i.to_u16
    end

    # Get country name.
    def country_name : String
      case self
      when .us?      then "United States/Canada"
      when .ru?      then "Russia"
      when .eg?      then "Egypt"
      when .za?      then "South Africa"
      when .gr?      then "Greece"
      when .nl?      then "Netherlands"
      when .be?      then "Belgium"
      when .fr?      then "France"
      when .es?      then "Spain"
      when .hu?      then "Hungary"
      when .it?      then "Italy"
      when .ro?      then "Romania"
      when .ch?      then "Switzerland"
      when .at?      then "Austria"
      when .uk?      then "United Kingdom"
      when .dk?      then "Denmark"
      when .se?      then "Sweden"
      when .no?      then "Norway"
      when .pl?      then "Poland"
      when .de?      then "Germany"
      when .mx?      then "Mexico"
      when .cu?      then "Cuba"
      when .ar?      then "Argentina"
      when .br?      then "Brazil"
      when .cl?      then "Chile"
      when .co?      then "Colombia"
      when .ve?      then "Venezuela"
      when .my?      then "Malaysia"
      when .au?      then "Australia"
      when .id?      then "Indonesia"
      when .ph?      then "Philippines"
      when .nz?      then "New Zealand"
      when .sg?      then "Singapore"
      when .th?      then "Thailand"
      when .jp?      then "Japan"
      when .kr?      then "South Korea"
      when .vn?      then "Vietnam"
      when .cn?      then "China"
      when .tr?      then "Turkey"
      when .in?      then "India"
      when .pk?      then "Pakistan"
      when .ae?      then "United Arab Emirates"
      when .il?      then "Israel"
      when .hk?      then "Hong Kong"
      when .tw?      then "Taiwan"
      else                "Unknown"
      end
    end

    # Parse from numeric value.
    def self.from_value(value : UInt16) : CountryCode
      CountryCode.from_value?(value.to_i32) || CountryCode::Unknown
    end
  end

  # Validated phone number per E.164.
  struct PhoneNumber
    getter country_code : CountryCode
    getter national_number : String

    def initialize(@country_code : CountryCode, @national_number : String)
    end

    # Format in E.164 format (+15551234567).
    def to_e164 : String
      "+#{@country_code.value}#{@national_number}"
    end

    # Format in international format with spaces.
    def to_international : String
      cc = @country_code.value
      nat = @national_number
      len = nat.size

      if len <= 4
        "+#{cc} #{nat}"
      elsif len <= 7
        "+#{cc} #{nat[0, 3]} #{nat[3..]}"
      elsif len <= 10
        "+#{cc} #{nat[0, 3]} #{nat[3, 3]} #{nat[6..]}"
      else
        "+#{cc} #{nat}"
      end
    end

    # Format in national format (without country code).
    def to_national : String
      nat = @national_number
      len = nat.size

      if len <= 4
        nat
      elsif len <= 7
        "#{nat[0, 3]}-#{nat[3..]}"
      elsif len <= 10
        "(#{nat[0, 3]}) #{nat[3, 3]}-#{nat[6..]}"
      else
        nat
      end
    end

    # Get total digit count (country code + national number).
    def digit_count : Int32
      cc_digits = if @country_code.value >= 100
                    3
                  elsif @country_code.value >= 10
                    2
                  else
                    1
                  end
      cc_digits + @national_number.size
    end

    # Default string representation is E.164.
    def to_s : String
      to_e164
    end

    # Check equality.
    def ==(other : PhoneNumber) : Bool
      @country_code == other.country_code && @national_number == other.national_number
    end
  end

  # Safe phone number validation and parsing.
  module SafePhone
    # Known country codes for parsing (sorted by length descending for greedy matching).
    COUNTRY_CODES = [
      {971_u16, CountryCode::Ae},
      {972_u16, CountryCode::Il},
      {852_u16, CountryCode::Hk},
      {886_u16, CountryCode::Tw},
      {91_u16, CountryCode::In},
      {92_u16, CountryCode::Pk},
      {90_u16, CountryCode::Tr},
      {86_u16, CountryCode::Cn},
      {84_u16, CountryCode::Vn},
      {82_u16, CountryCode::Kr},
      {81_u16, CountryCode::Jp},
      {66_u16, CountryCode::Th},
      {65_u16, CountryCode::Sg},
      {64_u16, CountryCode::Nz},
      {63_u16, CountryCode::Ph},
      {62_u16, CountryCode::Id},
      {61_u16, CountryCode::Au},
      {60_u16, CountryCode::My},
      {58_u16, CountryCode::Ve},
      {57_u16, CountryCode::Co},
      {56_u16, CountryCode::Cl},
      {55_u16, CountryCode::Br},
      {54_u16, CountryCode::Ar},
      {53_u16, CountryCode::Cu},
      {52_u16, CountryCode::Mx},
      {49_u16, CountryCode::De},
      {48_u16, CountryCode::Pl},
      {47_u16, CountryCode::No},
      {46_u16, CountryCode::Se},
      {45_u16, CountryCode::Dk},
      {44_u16, CountryCode::Uk},
      {43_u16, CountryCode::At},
      {41_u16, CountryCode::Ch},
      {40_u16, CountryCode::Ro},
      {39_u16, CountryCode::It},
      {36_u16, CountryCode::Hu},
      {34_u16, CountryCode::Es},
      {33_u16, CountryCode::Fr},
      {32_u16, CountryCode::Be},
      {31_u16, CountryCode::Nl},
      {30_u16, CountryCode::Gr},
      {27_u16, CountryCode::Za},
      {20_u16, CountryCode::Eg},
      {7_u16, CountryCode::Ru},
      {1_u16, CountryCode::Us},
    ]

    # Parse phone number from string.
    def self.parse(input : String) : PhoneNumber?
      trimmed = input.strip
      return nil if trimmed.empty?

      # Extract digits only
      digits = String.build do |str|
        trimmed.each_char do |c|
          str << c if c.ascii_number?
        end
      end

      return nil if digits.size < 7
      return nil if digits.size > 15

      # Try to parse country code
      result = parse_country_code(digits)
      return nil if result.nil?

      country_code, national_start = result

      national_number = digits[national_start..]
      return nil if national_number.size < 4

      PhoneNumber.new(country_code, national_number)
    end

    # Parse with explicit country code.
    def self.parse_with_country(input : String, default_country : CountryCode) : PhoneNumber?
      trimmed = input.strip
      return nil if trimmed.empty?

      # If starts with +, parse normally
      if trimmed.starts_with?('+')
        return parse(trimmed)
      end

      # Extract digits only
      digits = String.build do |str|
        trimmed.each_char do |c|
          str << c if c.ascii_number?
        end
      end

      return nil if digits.size < 4
      return nil if digits.size > 15

      PhoneNumber.new(default_country, digits)
    end

    # Check if string is a valid phone number.
    def self.valid?(input : String) : Bool
      !parse(input).nil?
    end

    # Check if two phone numbers are equal (normalized comparison).
    def self.equal?(a : String, b : String) : Bool
      phone_a = parse(a)
      phone_b = parse(b)
      return false if phone_a.nil? || phone_b.nil?
      phone_a == phone_b
    end

    # Mask a phone number for display (+1 555 123 **** ).
    def self.mask(phone : PhoneNumber) : String
      nat = phone.national_number
      if nat.size <= 4
        "+#{phone.country_code.value} ****"
      elsif nat.size <= 7
        "+#{phone.country_code.value} #{nat[0, 3]} ****"
      else
        visible = nat[0, nat.size - 4]
        "+#{phone.country_code.value} #{visible}****"
      end
    end

    # Get country code from numeric value.
    def self.country_from_value(value : UInt16) : CountryCode
      COUNTRY_CODES.each do |(code_val, code)|
        return code if code_val == value
      end
      CountryCode::Unknown
    end

    # Parse country code from digit string.
    private def self.parse_country_code(digits : String) : Tuple(CountryCode, Int32)?
      # Try 3-digit codes first, then 2, then 1
      [3, 2, 1].each do |len|
        next if digits.size < len
        value = digits[0, len].to_u16?
        next if value.nil?

        COUNTRY_CODES.each do |(code_val, code)|
          if code_val == value
            return {code, len}
          end
        end
      end

      nil
    end
  end
end
