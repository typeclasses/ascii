## Contributing to ASCII support in Haskell

### Convenience

The purpose of this project is to make it easy to work with ASCII text in Haskell. The flagship package, called [`ascii`](https://hackage.haskell.org/package/ascii), is intended to be largely inclusive of features. If it is missing something that you want, please request it or contribute.

- Even very small additions can be great usability improvements. This can include simple compositions of existing functions, or commonly-used specializations of polymorphic functions. If you cannot quickly figure out how to do something, there is probably something we can do to improve the library.

- Substantial new features are also be welcome. We'd rather incorporate more ASCII-related features into this repository and into the `ascii` package than to see other separately-maintained packages pop up. A [discussion](https://github.com/typeclasses/ascii/discussions) might be a good way to start.

### Stability

The project is broken into several packages so that core concepts can be provided in stable libraries that are not released often. In particular, the [`ascii-char`](https://hackage.haskell.org/package/ascii-char) package, which defines the 128-constructor `Char` type, should see releases, even minor ones, very sparingly. The goal in keeping this package stable is to allow other libraries to depend on it without introducing any ongoing maintenance hassle.

The larger the package, the more willing we should be to make minor and major releases. The [`ascii`](https://hackage.haskell.org/package/ascii) package in particular should prioritize improvement over stability.

Brief summary of [PVP](https://pvp.haskell.org/): In the version "A.B.C.D", the first two numbers "A.B" represent the *major* version, "C" is the *minor* version, and "D" is the *patch* version. Patch releases do not change the API at all (bug fixes, performance improvements, supporting new dependency versions), and minor releases are limited to backwards-compatible changes (such as adding a new function).

The `ascii` package re-exports definitions and entire modules from other packages. Its dependency bounds for these packages are always written in the form `== A.B.C.*`, and anything larger than a patch release of the other packages requires a new release of the `ascii` package as well.

### Dependencies

The goal is to support new releases of dependencies as quickly as possible. If there is a new version of a library that we do not yet support, please open an [issue](https://github.com/typeclasses/ascii/issues), and consider submitting a fix.

We support any version of GHC that was released within the last two years, plus one older version. Support for anything older will be dropped, even if there is no compelling reason, to make things easier to maintain. (If you have some need for support of older compilers, [talk to us](https://github.com/typeclasses/ascii/discussions) about changing this policy.)

Version bounds (the `build-depends` files in Cabal files) should reflect the versions that have been tested; a package should not declare support for a version unless there is an [Action](https://github.com/typeclasses/ascii/actions) that actually builds with it. When submitting a bounds change pull request, please update the [build configurations](https://github.com/typeclasses/ascii/tree/master/configurations) so that the automated testing can verify that everything works with the newly-included version.

These packages are all included in the [Stackage](https://github.com/commercialhaskell/stackage/) package set.

### Performance

Little effort so far has been put into making the libraries fast, but submissions of benchmarks and performance improvements would be welcome.

### Making changes

When making a change that should be mentioned in the release notes, add some mention of what changed in the `changelog.md` file within the relevant package directory (create the file if it does not already exist). In general, a package should be released as soon as it has something new that users might want.
