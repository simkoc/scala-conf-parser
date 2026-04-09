# Project Description
This is a scala project that provides a collection of parser for known configuration files,
transforming them into an AST-like data structure for further processing.

## Coding Guide
- Follow the core UNIX paradigm for each implemented function: Do one thing and do it well.
- Follow Joe's five-finger rule: Check every function longer than ~25 lines if it follows the UNIX principle or can otherwise be refactored into smaller testable bits
- Everything needs to be unit tested, unit tests should both cover specific configuration snippets testing individual sub-parser behavior but also cover larger examples.

## Testing Guide
- Each parser gets their own test class
- Each subparser gets their own unit test set to test its functionality
- Each parser gets their own set of real-world test cases for regression testing contained in `src/test/resources/<PARSER>/` that one
  unit test called `realWorldRegressionTests` iterates over and parses to check for any occurring parsing errors.