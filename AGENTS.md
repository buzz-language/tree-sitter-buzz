# AGENTS.md

## Build

- Install Node dependencies with `npm install`.
- Regenerate parser artifacts with `npx -y tree-sitter-cli@0.25.2 generate --abi 14`.
- Build the C parser library with `make`.
- Remove build outputs with `make clean`.

## Test

- Run Tree-sitter tests with `make test` or `tree-sitter test`.
- Parse a Buzz file with `tree-sitter parse path/to/file.buzz`.
- Check highlight captures with `tree-sitter highlight --check path/to/file.buzz`.
- Test valid Buzz syntax with `buzz --check file.buzz`.

## Notes

- Keep generated files (`src/parser.c`, `src/grammar.json`, and `src/node-types.json`) in sync with `grammar.js`.
- Use ABI 14 when regenerating parser artifacts for compatibility with consumers that have not moved to newer Tree-sitter ABIs.
