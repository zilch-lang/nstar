# CLI compiler configuration options

| Option name         | Description                                                  | Possible values        | Default |
|---------------------|--------------------------------------------------------------|------------------------|---------|
| `color-diagnostics` | Choose whether to enable colors in error/warning diagnostics | `yes            \| no` | `yes`   |

# CLI compiler debugging options

| Option name      | Description                                                                       |
| `dump-ast`       | Dumps the AST (after post-processing) to the file `.nsc/dump/ast.debug`           |
| `dump-typed-ast` | Dumps the typed AST (after type-checking) to the file `.nsc/dump/typed-ast.debug` |
