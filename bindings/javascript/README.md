# Proven JavaScript Bindings

Safe, formally verified library for JavaScript providing UUID, currency, phone number, and hexadecimal operations that cannot crash.

## Installation

```bash
npm install @proven/javascript
```

## Usage

```javascript
import {
  SafeUUID,
  Money,
  CurrencyCode,
  PhoneNumber,
  CountryCode,
  SafeHex,
} from '@proven/javascript';

// UUID operations
const uuidResult = SafeUUID.parse('550e8400-e29b-41d4-a716-446655440000');
if (uuidResult.ok) {
  console.log(uuidResult.value.toString());
  console.log(uuidResult.value.getVersion());  // 'v4'
}

// Money operations (no floating-point errors)
const moneyResult = Money.fromMajorUnits(10.50, CurrencyCode.USD);
if (moneyResult.ok) {
  const doubled = moneyResult.value.multiply(2);
  console.log(doubled.value.format());  // '$21.00'
}

// Phone number parsing
const phoneResult = PhoneNumber.parse('+14155551234');
if (phoneResult.ok) {
  console.log(phoneResult.value.formatInternational());  // '+1 415 555 1234'
  console.log(phoneResult.value.formatE164());  // '+14155551234'
}

// Hex encoding/decoding
const hexResult = SafeHex.encode(new Uint8Array([0xde, 0xad, 0xbe, 0xef]));
console.log(hexResult.value);  // 'deadbeef'

// Constant-time comparison for security
const isEqual = SafeHex.constantTimeEqual('secret1', 'secret2');
```

## Modules

- **SafeUUID**: UUID parsing, validation, and v4 generation
- **Money/CurrencyCode**: Money arithmetic using minor units
- **PhoneNumber/CountryCode**: Phone number parsing and formatting
- **SafeHex**: Hexadecimal encoding with constant-time comparison

## License

AGPL-3.0-or-later
