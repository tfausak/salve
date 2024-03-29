-- | This module defines types and functions for working with versions as
-- defined by [Semantic Versioning](http://semver.org/spec/v2.0.0.html). It
-- also provides types and functions for working with version constraints as
-- described by [npm](https://github.com/npm/npm/blob/d081cc6/doc/misc/semver.md#ranges).
module Salve
  ( -- | This module doesn't export anything that conflicts with the "Prelude", so
    -- you can import it unqualified.
    --
    -- >>> import Salve
    --
    -- This module provides lenses for modifying versions. If you want to modify
    -- versions, consider importing a lens library like
    -- [microlens](https://www.stackage.org/lts-9.3/package/microlens-0.4.8.1).
    --
    -- The 'Version' data type is the core of this module. Use 'parseVersion' to
    -- make versions and 'renderVersion' to convert them into strings.
    --
    -- >>> renderVersion <$> parseVersion "1.2.3"
    -- Just "1.2.3"
    --
    -- The 'Constraint' data type allows you to specify version constraints. Use
    -- 'parseConstraint' to make constraints and 'renderConstraint' to convert them
    -- into strings.
    --
    -- >>> renderConstraint <$> parseConstraint ">1.2.0"
    -- Just ">1.2.0"
    --
    -- Use 'satisfiesConstraint' to see if a version satisfiesConstraint a
    -- constraint.
    --
    -- >>> satisfiesConstraint <$> parseConstraint ">1.2.0" <*> parseVersion "1.2.3"
    -- Just True

    -- * Cheat sheet

    -- | If you're coming from Cabal, you might not be familiar with npm's version
    -- range syntax. This table shows you how npm version ranges map to Cabal's
    -- version constraints.
    --
    -- > Salve           | Cabal              | Notes
    -- > -----           | -----              | -----
    -- > <1.2.3          | <1.2.3             | -
    -- > <=1.2.3         | <=1.2.3            | -
    -- > =1.2.3          | ==1.2.3            | equals sign is optional
    -- > >=1.2.3         | >=1.2.3            | -
    -- > >1.2.3          | >1.2.3             | -
    -- > 1.2.3 || >1.2.3 | ==1.2.3 || >1.2.3  | lower precedence than and
    -- > >=1.2.3 <2.0.0  | >=1.2.3 && <2.0.0  | higher precedence than or
    -- > 1.2.3 - 2.3.4   | >=1.2.3 && <=2.3.4 | inclusive ranges
    -- > 1.2.x           | ==1.2.*            | can use X or * instead of x
    -- > 1.x.x           | ==1.*              | -
    -- > x.x.x           | ==*                | same as -any
    -- > ~1.2.3          | ^>=1.2.3           | same as >=1.2.3 && <1.3.0
    -- > ^1.2.3          | >=1.2.3 && <2      | -
    -- > ^0.2.3          | >=0.2.3 && <0.3    | -
    -- > ^0.0.3          | >=0.0.3 && <0.0.4  | -

    -- * Rationale

    -- ** The PVP

    -- | Haskell's
    -- [Package Versioning Policy](https://github.com/haskell/pvp/blob/176bb14/pvp-specification.md)
    -- (PVP) defines three things:
    --
    -- 1.  A spec for versioning your package, which includes how version numbers
    --     look and how to encode breaking changes.
    -- 2.  A spec for constraining the versions of your dependencies, which
    --     incldues how version ranges look.
    -- 3.  A prescription for how to constrain the versions of your dependencies,
    --     which includes how the ranges of your dependencies should be.
    --
    -- By comparison, Semantic Versioning only deals with the first thing. npm's
    -- version ranges only deal with the second thing. This module deals with the
    -- first and second things but leaves the third up to you.
    --
    -- Looking at the first point, why might you want to use SemVer instead of the
    -- PVP? The PVP has many problems, as described by the
    -- [Problematic versioning policy](http://taylor.fausak.me/2016/12/28/problematic-versioning-policy/)
    -- blog post. In short, the PVP is too flexible and it's unique to Haskell,
    -- which causes unnecessary friction with developers from other languages.
    --
    -- Moving on to the second point, why should we use npm's version ranges
    -- instead of the PVP's? This is a less clear cut. The two syntaxes are broadly
    -- compatible. Really the only gains here are compatibility with a widely-used
    -- syntax and convenient shorthand for common constraints (like hyphens
    -- @1.2.3 - 2.3.4@, tildes @~1.2.3@, and carets @^1.2.3@).

    -- ** Other modules

    -- | There are already a few modules that provide version numbers. Why do we
    -- need another one? Let's take a look at the options.
    --
    -- -   [Data.Version](https://www.stackage.org/haddock/lts-9.3/base-4.9.1.0/Data-Version.html)
    --     from the @base@ package:
    --
    --     -   Exposes constructors, which allows creating versions that cannot be
    --         parsed.
    --     -   Allows any number of components, from zero to inifinity.
    --     -   Deprecated tags on versions.
    --     -   Does not support build metadata on versions.
    --     -   Does not support constraints.
    --
    -- -   [Distribution.Version](https://www.stackage.org/haddock/lts-9.3/Cabal-1.24.2.0/Distribution-Version.html)
    --     from the @Cabal@ package:
    --
    --     -   Has the same problems as Data.Version because it re-uses that
    --         version type.
    --     -   Depends on the @array@, @binary@, @bytestring@, @containers@,
    --         @deepseq@, @directory@, @filepath@, @pretty@, @process@, @time@, and
    --         @unix@ packages.
    --
    -- -   [Data.SemVer](https://www.stackage.org/haddock/lts-9.3/semver-0.3.3.1/Data-SemVer.html)
    --     from the @semver@ package:
    --
    --     -   Depends on the @attoparsec@, @deepseq@, and @text@ packages.
    --     -   Does not support version constraints.
    --
    -- -   [Data.SemVer](https://hackage.haskell.org/package/semver-range-0.2.2/docs/Data-SemVer.html)
    --     from the @semver-range@ package:
    --
    --     -   Depends on the @classy-prelude@, @parsec@, @text@, and
    --         @unordered-containers@ packages.
    --     -   Module name collides with the @semver@ package.
    --     -   Supports constraints, but does not provide a way to render them.
    --
    -- -   [Data.Versions](https://www.stackage.org/haddock/lts-9.3/versions-3.1.1/Data-Versions.html)
    --     from the @versions@ package:
    --
    --     -   Depends on the @deepseq@, @hashable@, @megaparsec@, and @text@
    --         packages.
    --     -   Intentially allows weird versions.
    --     -   Does not support constraints.
    --
    -- By comparison, this module:
    --
    -- -   Does not expose constructors. Any version you create can be rendered and
    --     parsed without issue.
    -- -   Requires exactly three components. You won't have to wonder if version
    --     @1.2.0@ is greater than @1.2@.
    -- -   Allows pre-release identifiers on versions. Go ahead and release version
    --     @1.0.0-alpha@ for early adopters.
    -- -   Allows build metadata on versions. Show when a release was made with
    --     versions like @1.0.0+2001-02-03@.
    -- -   Supports version constraints. Just like versions, rendering and parsing
    --     constraints is no problem.
    -- -   Only depends on the @base@ package. You can use all the functionality
    --     without installing any other packages.
    -- -   Has a unique module name. You won't have to use the @PackageImports@
    --     extension simply to deal with version numbers.

    -- * Types
    Salve.Version,
    Salve.PreRelease,
    Salve.Build,
    Salve.Constraint,

    -- * Constructors
    Salve.makeVersion,
    Salve.initialVersion,

    -- * Parsing
    Salve.parseVersion,
    Salve.parsePreRelease,
    Salve.parseBuild,
    Salve.parseConstraint,

    -- ** Unsafe

    -- | These functions can be used to unsafely parse strings. Instead of
    -- returning 'Nothing', they raise an exception. Only use these if you are sure
    -- the string can be successfully parsed!
    Salve.unsafeParseVersion,
    Salve.unsafeParsePreRelease,
    Salve.unsafeParseBuild,
    Salve.unsafeParseConstraint,

    -- * Rendering
    Salve.renderVersion,
    Salve.renderPreRelease,
    Salve.renderBuild,
    Salve.renderConstraint,

    -- * Predicates
    Salve.isUnstable,
    Salve.isStable,

    -- * Conversions
    Salve.fromBaseVersion,
    Salve.toBaseVersion,

    -- * Helpers
    Salve.bumpMajor,
    Salve.bumpMinor,
    Salve.bumpPatch,
    Salve.satisfiesConstraint,

    -- * Lenses

    -- | These lenses can be used to access and modify specific parts of a
    -- 'Version'.
    --
    -- Don't be scared by these type signatures. They are provided in full to avoid
    -- the @RankNTypes@ language extension. The type signature
    -- @'Functor' f => (a -> f a) -> 'Version' -> f 'Version'@ is the same as
    -- @Lens' 'Version' a@, which you may already be familiar with.
    Salve.majorLens,
    Salve.minorLens,
    Salve.patchLens,
    Salve.preReleasesLens,
    Salve.buildsLens,

    -- * Examples

    -- | These examples are provided to showcase functionality and explain weird
    -- behavior. If something isn't clear, please open a pull request adding an
    -- example!

    -- ** Versions

    -- | Leading zeros are not allowed.
    --
    -- >>> parseVersion "01.0.0"
    -- Nothing
    -- >>> parseVersion "0.01.0"
    -- Nothing
    -- >>> parseVersion "0.0.01"
    -- Nothing
    --
    -- Negative numbers are not allowed.
    --
    -- >>> parseVersion "-1.0.0"
    -- Nothing
    -- >>> parseVersion "0.-1.0"
    -- Nothing
    -- >>> parseVersion "0.0.-1"
    -- Nothing
    --
    -- Non-digits are not allowed.
    --
    -- >>> parseVersion "a.0.0"
    -- Nothing
    -- >>> parseVersion "0.a.0"
    -- Nothing
    -- >>> parseVersion "0.0.a"
    -- Nothing
    --
    -- Partial version numbers are not allowed.
    --
    -- >>> parseVersion "0.0"
    -- Nothing
    --
    -- Extra version numbers are not allowed.
    --
    -- >>> parseVersion "0.0.0.0"
    -- Nothing
    --
    -- Spaces are allowed
    --
    -- >>> parseVersion " 0.0.0"
    -- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
    -- >>> parseVersion "0.0.0 "
    -- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
    --
    -- Interior spaces are not allowed.
    --
    -- >>> parseVersion "0 .0.0"
    -- Nothing
    -- >>> parseVersion "0. 0.0"
    -- Nothing
    --
    -- Each version component cannot be larger than a 64-bit unsigned integer.
    --
    -- >>> parseVersion "18446744073709551615.0.0"
    -- Just (Version {versionMajor = 18446744073709551615, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
    -- >>> parseVersion "18446744073709551616.0.0"
    -- Nothing
    --
    -- Numeric pre-releases tags cannot be larger than a 64-bit unsigned integer.
    --
    -- >>> parseVersion "0.0.0-18446744073709551615"
    -- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [PreReleaseNumeric 18446744073709551615], versionBuilds = []})
    -- >>> parseVersion "0.0.0-18446744073709551616"
    -- Nothing
    --
    -- Build metadata is not numeric so it does not have any limit.
    --
    -- >>> parseVersion "0.0.0+18446744073709551615"
    -- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = [Build "18446744073709551615"]})
    -- >>> parseVersion "0.0.0+18446744073709551616"
    -- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = [Build "18446744073709551616"]})

    -- ** Constraints

    -- | Partial version numbers are not allowed.
    --
    -- >>> parseConstraint "1.2"
    -- Nothing
    --
    -- Wildcards (also known as "x-ranges") are allowed. The exact character used
    -- for the wildcard is not round-tripped.
    --
    -- >>> renderConstraint <$> parseConstraint "1.2.x"
    -- Just "1.2.x"
    -- >>> renderConstraint <$> parseConstraint "1.2.X"
    -- Just "1.2.x"
    -- >>> renderConstraint <$> parseConstraint "1.2.*"
    -- Just "1.2.x"
    --
    -- An optional equals sign can be included with wildcard constraints.
    --
    -- >>> renderConstraint <$> parseConstraint "=1.2.x"
    -- Just "1.2.x"
    --
    -- Wildcards can be combined with other constraints.
    --
    -- >>> renderConstraint <$> parseConstraint "1.2.x 2.3.4"
    -- Just "1.2.x 2.3.4"
    -- >>> renderConstraint <$> parseConstraint "1.2.x || 2.3.4"
    -- Just "1.2.x || 2.3.4"
    --
    -- Wildcards are allowed at any position.
    --
    -- >>> renderConstraint <$> parseConstraint "1.2.x"
    -- Just "1.2.x"
    -- >>> renderConstraint <$> parseConstraint "1.x.x"
    -- Just "1.x.x"
    -- >>> renderConstraint <$> parseConstraint "x.x.x"
    -- Just "x.x.x"
    --
    -- Non-wildcards cannot come after wildcards.
    --
    -- >>> parseConstraint "1.x.3"
    -- Nothing
    -- >>> parseConstraint "x.2.3"
    -- Nothing
    -- >>> parseConstraint "x.x.3"
    -- Nothing
    -- >>> parseConstraint "x.2.x"
    -- Nothing
    --
    -- Wildcards cannot be used with other operators.
    --
    -- >>> parseConstraint "<1.2.x"
    -- Nothing
    -- >>> parseConstraint "<=1.2.x"
    -- Nothing
    -- >>> parseConstraint ">=1.2.x"
    -- Nothing
    -- >>> parseConstraint ">1.2.x"
    -- Nothing
    -- >>> parseConstraint "~1.2.x"
    -- Nothing
    -- >>> parseConstraint "^1.2.x"
    -- Nothing
    -- >>> parseConstraint "1.2.x - 2.3.4"
    -- Nothing
    -- >>> parseConstraint "1.2.3 - 2.3.x"
    -- Nothing
    --
    -- Spaces are allowed in most places. Extra spaces are not round-tripped.
    --
    -- >>> renderConstraint <$> parseConstraint " 1.2.3 "
    -- Just "1.2.3"
    -- >>> renderConstraint <$> parseConstraint "> 1.2.3"
    -- Just ">1.2.3"
    -- >>> renderConstraint <$> parseConstraint "1.2.3  -  2.3.4"
    -- Just "1.2.3 - 2.3.4"
    -- >>> renderConstraint <$> parseConstraint "1.2.3  2.3.4"
    -- Just "1.2.3 2.3.4"
    -- >>> renderConstraint <$> parseConstraint "1.2.3  ||  2.3.4"
    -- Just "1.2.3 || 2.3.4"
    --
    -- Parentheses are not allowed. Note that combining two constraints with a
    -- space (and) has higher precedence than combining them with pipes (or). In
    -- other words, @"a b || c"@ parses as @"(a b) || c"@, not @"a (b || c)"@.
    --
    -- >>> parseConstraint "(1.2.3)"
    -- Nothing
    -- >>> parseConstraint "(1.2.3 || >1.2.3) <1.3.0"
    -- Nothing
    -- >>> parseConstraint "(>1.2.3 <1.3.0) || 1.2.3"
    -- Nothing
    --
    -- Most constraints can be round-tripped through parsing and rendering.
    --
    -- >>> renderConstraint <$> parseConstraint "<1.2.3"
    -- Just "<1.2.3"
    -- >>> renderConstraint <$> parseConstraint "<=1.2.3"
    -- Just "<=1.2.3"
    -- >>> renderConstraint <$> parseConstraint "1.2.3"
    -- Just "1.2.3"
    -- >>> renderConstraint <$> parseConstraint ">=1.2.3"
    -- Just ">=1.2.3"
    -- >>> renderConstraint <$> parseConstraint ">1.2.3"
    -- Just ">1.2.3"
    -- >>> renderConstraint <$> parseConstraint "1.2.3 - 2.3.4"
    -- Just "1.2.3 - 2.3.4"
    -- >>> renderConstraint <$> parseConstraint "~1.2.3"
    -- Just "~1.2.3"
    -- >>> renderConstraint <$> parseConstraint "^1.2.3"
    -- Just "^1.2.3"
    -- >>> renderConstraint <$> parseConstraint ">1.2.3 <2.0.0"
    -- Just ">1.2.3 <2.0.0"
    -- >>> renderConstraint <$> parseConstraint "1.2.3 || >1.2.3"
    -- Just "1.2.3 || >1.2.3"
    --
    -- Explicit equal signs do not get round-tripped.
    --
    -- >>> renderConstraint <$> parseConstraint "=1.2.3"
    -- Just "1.2.3"
    --
    -- Pre-releases and builds are allowed on any constraints except wildcards.
    --
    -- >>> renderConstraint <$> parseConstraint "1.2.3-p+b"
    -- Just "1.2.3-p+b"
    -- >>> renderConstraint <$> parseConstraint ">1.2.3-p+b"
    -- Just ">1.2.3-p+b"
    -- >>> renderConstraint <$> parseConstraint "1.2.3-p+b - 2.3.4-p+b"
    -- Just "1.2.3-p+b - 2.3.4-p+b"
    -- >>> renderConstraint <$> parseConstraint "~1.2.3-p+b"
    -- Just "~1.2.3-p+b"
    -- >>> renderConstraint <$> parseConstraint "^1.2.3-p+b"
    -- Just "^1.2.3-p+b"
    --
    -- >>> parseConstraint "1.2.x-p+b"
    -- Nothing
    --
    -- These examples show every type of constraint in a single expression.
    --
    -- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 =1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9 1.2.x"
    -- Just "<1.2.0 <=1.2.1 1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9 1.2.x"
    -- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9 1.2.x"
    -- Just "<1.2.0 <=1.2.1 || 1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9 1.2.x"
    -- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 =1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9 || 1.2.x"
    -- Just "<1.2.0 || <=1.2.1 1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9 || 1.2.x"
    -- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 || =1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9 || 1.2.x"
    -- Just "<1.2.0 || <=1.2.1 || 1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9 || 1.2.x"

    -- ** Satisfying constraints

    -- | Although in general you should use 'satisfiesConstraint', 'parseVersion',
    -- and 'parseConstraint', doing that here makes it hard to tell what the
    -- examples are doing. An operator makes things clearer.
    --
    -- >>> satisfiesConstraint <$> parseConstraint "=1.2.3" <*> parseVersion "1.2.3"
    -- Just True
    -- >>> let version ? constraint = satisfiesConstraint (unsafeParseConstraint constraint) (unsafeParseVersion version)
    -- >>> "1.2.3" ? "=1.2.3"
    -- True
    --
    -- -   Less than:
    --
    --     >>> "1.2.2" ? "<1.2.3"
    --     True
    --     >>> "1.2.3" ? "<1.2.3"
    --     False
    --     >>> "1.2.4" ? "<1.2.3"
    --     False
    --     >>> "1.2.3-pre" ? "<1.2.3"
    --     True
    --
    -- -   Less than or equal to:
    --
    --     >>> "1.2.2" ? "<=1.2.3"
    --     True
    --     >>> "1.2.3" ? "<=1.2.3"
    --     True
    --     >>> "1.2.4" ? "<=1.2.3"
    --     False
    --
    -- -   Equal to:
    --
    --     >>> "1.2.2" ? "=1.2.3"
    --     False
    --     >>> "1.2.3" ? "=1.2.3"
    --     True
    --     >>> "1.2.4" ? "=1.2.3"
    --     False
    --     >>> "1.2.3-pre" ? "=1.2.3"
    --     False
    --     >>> "1.2.3+build" ? "=1.2.3"
    --     True
    --
    -- -   Greater than or equal to:
    --
    --     >>> "1.2.2" ? ">=1.2.3"
    --     False
    --     >>> "1.2.3" ? ">=1.2.3"
    --     True
    --     >>> "1.2.4" ? ">=1.2.3"
    --     True
    --
    -- -   Greater than:
    --
    --     >>> "1.2.2" ? ">1.2.3"
    --     False
    --     >>> "1.2.3" ? ">1.2.3"
    --     False
    --     >>> "1.2.4" ? ">1.2.3"
    --     True
    --     >>> "1.2.4-pre" ? ">1.2.3"
    --     True
    --
    --     >>> "1.2.4" ? ">1.2.3-pre"
    --     True
    --
    -- -   And:
    --
    --     >>> "1.2.3" ? ">1.2.3 <1.2.5"
    --     False
    --     >>> "1.2.4" ? ">1.2.3 <1.2.5"
    --     True
    --     >>> "1.2.5" ? ">1.2.3 <1.2.5"
    --     False
    --
    -- -   Or:
    --
    --     >>> "1.2.2" ? "1.2.3 || 1.2.4"
    --     False
    --     >>> "1.2.3" ? "1.2.3 || 1.2.4"
    --     True
    --     >>> "1.2.4" ? "1.2.3 || 1.2.4"
    --     True
    --     >>> "1.2.5" ? "1.2.3 || 1.2.4"
    --     False
    --
    -- -   And & or:
    --
    --     >>> "1.2.2" ? "1.2.2 || >1.2.3 <1.3.0"
    --     True
    --     >>> "1.2.3" ? "1.2.2 || >1.2.3 <1.3.0"
    --     False
    --     >>> "1.2.4" ? "1.2.2 || >1.2.3 <1.3.0"
    --     True
    --     >>> "1.3.0" ? "1.2.2 || >1.2.3 <1.3.0"
    --     False
    --
    -- -   Hyphen:
    --
    --     >>> "1.2.2" ? "1.2.3 - 1.2.4"
    --     False
    --     >>> "1.2.3" ? "1.2.3 - 1.2.4"
    --     True
    --     >>> "1.2.4" ? "1.2.3 - 1.2.4"
    --     True
    --     >>> "1.2.5" ? "1.2.3 - 1.2.4"
    --     False
    --
    -- -   Tilde:
    --
    --     >>> "1.2.2" ? "~1.2.3"
    --     False
    --     >>> "1.2.3" ? "~1.2.3"
    --     True
    --     >>> "1.2.4" ? "~1.2.3"
    --     True
    --     >>> "1.3.0" ? "~1.2.3"
    --     False
    --
    -- -   Caret:
    --
    --     >>> "1.2.2" ? "^1.2.3"
    --     False
    --     >>> "1.2.3" ? "^1.2.3"
    --     True
    --     >>> "1.2.4" ? "^1.2.3"
    --     True
    --     >>> "1.3.0" ? "^1.2.3"
    --     True
    --     >>> "2.0.0" ? "^1.2.3"
    --     False
    --
    --     >>> "0.2.2" ? "^0.2.3"
    --     False
    --     >>> "0.2.3" ? "^0.2.3"
    --     True
    --     >>> "0.2.4" ? "^0.2.3"
    --     True
    --     >>> "0.3.0" ? "^0.2.3"
    --     False
    --
    --     >>> "0.0.2" ? "^0.0.3"
    --     False
    --     >>> "0.0.3" ? "^0.0.3"
    --     True
    --     >>> "0.0.4" ? "^0.0.3"
    --     False
    --
    -- -   Wildcard:
    --
    --     >>> "1.1.0" ? "1.2.x"
    --     False
    --     >>> "1.2.3" ? "1.2.x"
    --     True
    --     >>> "1.3.0" ? "1.2.x"
    --     False
    --
    --     >>> "0.1.0" ? "1.x.x"
    --     False
    --     >>> "1.0.0" ? "1.x.x"
    --     True
    --     >>> "1.2.3" ? "1.x.x"
    --     True
    --     >>> "2.0.0" ? "1.x.x"
    --     False
    --
    --     >>> "0.0.0" ? "x.x.x"
    --     True
    --     >>> "1.2.3" ? "x.x.x"
    --     True
    --     >>> "2.0.0" ? "x.x.x"
    --     True
  )
where

import qualified Salve.Internal as Salve

-- $setup
-- >>> import Control.Applicative
-- >>> import Salve
