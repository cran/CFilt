# CFilt 1.0.0

## Breaking changes

- The package architecture has been redesigned: CFilt is now fully based on R6 classes, replacing the previous S3/OO implementation.
- Several legacy methods have been removed, including `setnewuser()` and `setnewitem()`. Equivalent functionality is now handled by updated class methods.
- The internal data structures have changed, and objects created in previous versions are not fully compatible with this release.

## Improvements

- Significant performance improvements through the adoption of sparse matrix representations (`Matrix` package), reducing memory usage and computation time.
- Optimized update procedures for user-item and item-item similarity matrices.
- Improved scalability for larger datasets.

## Documentation

- Updated function documentation to reflect the new R6-based architecture.
- Examples revised to match the current class usage.

## Internal changes

- Refactored internal methods to improve efficiency and maintainability.
- Updated dependency usage and removed unused imports.
