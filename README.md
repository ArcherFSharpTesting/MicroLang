# Archer.MicroLang

Archer.MicroLang is an internal testing utility library used to test the Archer test framework itself. It provides helper functions and utilities for framework development and testing.

## Purpose

This library contains:
- Test execution helpers (`runAndReport`, `filterRunAndReport`)
- Test result counting utilities (`countFailures`, `countSuccesses`, `countIgnored`)
- Verification helpers (`expects`, `withMessage`, `by`)
- Test combinators (`andResult`, `orResult`)

## Note

This is primarily an internal development tool for the Archer framework. End users should use **Archer.Validations** (Archer.Fletching) for test assertions and validations in their test projects.

## For Archer Development

If you're contributing to the Archer framework, MicroLang provides utilities for writing meta-tests (tests that test the testing framework).

