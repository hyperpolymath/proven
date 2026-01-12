# Publishing Guide

This document describes how to publish proven to package registries.

## Automated Publishing

Publishing is automated via GitHub Actions. When a version tag is pushed (e.g., `v1.0.0`), the release workflow automatically:

1. Validates version consistency across all packages
2. Creates a GitHub Release
3. Publishes to all configured registries

### Trigger a Release

```bash
# Tag a release
git tag v1.0.0
git push origin v1.0.0
```

Or use manual workflow dispatch in GitHub Actions.

## Registry Setup

### Required GitHub Secrets

Configure these secrets in your repository settings (Settings → Secrets → Actions):

| Secret | Registry | How to obtain |
|--------|----------|---------------|
| `CRATES_IO_TOKEN` | crates.io | [crates.io/settings/tokens](https://crates.io/settings/tokens) |
| `NPM_TOKEN` | npm | [npmjs.com/settings/tokens](https://www.npmjs.com/settings/~/tokens) - create Automation token |
| `JSR_TOKEN` | JSR | [jsr.io/account/tokens](https://jsr.io/account/tokens) |

### PyPI Trusted Publishing

PyPI uses OIDC trusted publishing - no token needed. Configure it:

1. Go to [pypi.org](https://pypi.org) → Your Projects → proven
2. Click "Publishing" → "Add a new publisher"
3. Configure:
   - **Owner**: hyperpolymath
   - **Repository**: proven
   - **Workflow**: publish-pypi.yml
   - **Environment**: pypi (optional)

## Package Configuration

### crates.io (Rust)

Location: `bindings/rust/Cargo.toml`

```toml
[package]
name = "proven"
version = "0.9.0"
edition = "2021"
license = "AGPL-3.0-or-later"
repository = "https://github.com/hyperpolymath/proven"
description = "Formally verified safety library - code that cannot crash"
keywords = ["safety", "verification", "idris", "dependent-types"]
categories = ["no-std", "parsing", "encoding"]
```

### PyPI (Python)

Location: `bindings/python/pyproject.toml`

```toml
[project]
name = "proven"
version = "0.9.0"
description = "Formally verified safety library"
license = {text = "AGPL-3.0-or-later"}
```

### npm (JavaScript/TypeScript)

Location: `bindings/javascript/package.json` and `bindings/typescript/package.json`

```json
{
  "name": "@proven/javascript",
  "version": "0.9.0",
  "license": "AGPL-3.0-or-later"
}
```

### JSR (Deno)

Location: `bindings/deno/deno.json`

```json
{
  "name": "@hyperpolymath/proven",
  "version": "0.9.0",
  "exports": "./mod.ts"
}
```

## Version Bumping

Before releasing, ensure all packages have the same version:

```bash
# Check current versions
grep '^version' bindings/rust/Cargo.toml
grep '^version' bindings/python/pyproject.toml
jq '.version' bindings/deno/deno.json
jq '.version' bindings/javascript/package.json
jq '.version' bindings/typescript/package.json
```

Update all packages to the new version before tagging.

## Dry Run

Test publishing without actually publishing:

1. Go to GitHub Actions
2. Select "Publish to [registry]" workflow
3. Click "Run workflow"
4. Enable "Dry run" option
5. Click "Run workflow"

## Troubleshooting

### crates.io: "crate already exists"

The crate name is taken. Either:
- Use a different name
- Claim ownership if you own it

### npm: "You do not have permission"

Ensure your npm token has publish permissions and the scope is correct.

### PyPI: "Invalid OIDC claim"

Check the trusted publisher configuration matches exactly:
- Repository owner (case-sensitive)
- Repository name (case-sensitive)
- Workflow filename

### JSR: "Package not found"

First-time publish requires creating the package:
```bash
cd bindings/deno
deno publish --dry-run  # Validates
deno publish            # Creates and publishes
```

## Manual Publishing

For manual publishing (not recommended):

```bash
# Rust
cd bindings/rust && cargo publish

# Python
cd bindings/python && python -m build && twine upload dist/*

# npm
cd bindings/javascript && npm publish --access public

# JSR
cd bindings/deno && deno publish
```
