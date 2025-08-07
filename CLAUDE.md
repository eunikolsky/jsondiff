# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`jsondiff` is a Haskell CLI tool for managing JSON localization files. It helps calculate translation diffs and integrate new translations while handling key movements, value changes, and removals.

The tool operates on hierarchical key-value JSON files containing only strings, arrays of strings, and nested objects.

## Build System and Commands

This project uses Haskell Stack:

- **Build**: `stack build`
- **Install**: `stack install jsondiff` 
- **Test**: `stack test`
- **Clean**: `stack clean`

The executable is built to `jsondiff` and supports two main commands:
- `jsondiff diff --english <file> --translation <file>` - Calculate what needs translation
- `jsondiff integrate --old-english <file> --current-english <file> --current-translation <file> --new-translation <file>` - Integrate new translations

## Core Architecture

The codebase follows a clean functional architecture with these key modules:

### Types.hs
Defines core data structures:
- `JKey`: Hierarchical key paths (e.g., `["group", "nested", "key"]`)
- `JValue`: String values or arrays of strings only
- `JKeyValues`: Map from keys to values representing flattened JSON

### Lib.hs  
Core JSON processing:
- `values`: Converts JSON `Value` to flat `JKeyValues` representation
- `unValues`: Reconstructs hierarchical JSON from flat representation
- `diff`: Calculates missing keys between two JSON files
- `jq`: Pretty-prints JSON with jq-style formatting

### Diff.hs
Translation integration logic:
- `findChanges`: Compares old vs current English to detect key changes
- `mergeTranslations`: Integrates new translations with change detection
- Handles moved values, changed values, and removed keys with detailed warnings

### Main.hs + Options.hs
CLI interface using `optparse-applicative` for command parsing.

## Key Concepts

**Flattened Representation**: JSON objects are converted to flat key-value maps where keys are dot-separated paths (e.g., `group.nested.key`). This enables precise diffing and change detection.

**Stateless Change Tracking**: Uses 4-file comparison (old English, current English, current translation, new translation) instead of maintaining state, similar to git's approach.

**Value Movement Detection**: Detects when English values move between keys and automatically applies translations to new locations.

## File Structure

- `app/` - CLI entry point and option parsing
- `src/` - Core library modules  
- `test/` - Hspec test suite
- `stack.yaml` - Stack configuration using LTS 19.0
- `package.yaml` - Hpack package configuration