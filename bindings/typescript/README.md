# Proven TypeScript Adapter

TypeScript type definitions adapter for Proven.

**For JavaScript developers:** The ReScript binding (`@proven/rescript`) compiles to clean ES6 JavaScript modules (`.res.js` files) that you can import directly. No ReScript knowledge required to use the output.

**For TypeScript developers:** This package provides `.d.ts` type definitions for strict TypeScript type checking.

## JavaScript Usage (via ReScript output)

```javascript
// ReScript compiles to clean ES6 modules
import { SafeMath } from '@proven/rescript/SafeMath.res.js'

const result = SafeMath.checkedAdd(100, 200)
```

## TypeScript Usage

```typescript
import { SafeMath } from '@proven/typescript'

const result: Result<number, OverflowError> = SafeMath.checkedAdd(100, 200)
```

## Which Package to Use

| Your Project | Use This |
|--------------|----------|
| JavaScript (any) | `@proven/rescript` (import the `.res.js` files) |
| TypeScript | `@proven/typescript` for types |
| Deno | `@proven/deno` |

## Installation

```bash
npm install @proven/typescript
```

## See Also

- `../rescript/` - Primary JS ecosystem binding (recommended)
- `../deno/` - Deno-specific binding
