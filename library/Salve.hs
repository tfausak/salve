-- | This module defines types and functions for working with versions as
-- defined by [Semantic Versioning](http://semver.org/spec/v2.0.0.html). It
-- also provides types and functions for working with version constraints as
-- described by [npm](https://docs.npmjs.com/misc/semver#ranges).
module Salve (
-- | This module doesn't export anything that conflicts with the "Prelude", so
-- you can import it unqualified.
--
-- >>> import Salve
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
-- Use 'satisfies' to see if a version satisfies a constraint.
--
-- >>> satisfies <$> parseVersion "1.2.3" <*> parseConstraint ">1.2.0"
-- Just True

-- * Types
Version,
PreRelease,
Build,
Constraint,

-- * Constructors
makeVersion,
initialVersion,
constraintLT,
constraintLE,
constraintEQ,
constraintGE,
constraintGT,
constraintAnd,
constraintOr,

-- * Parsing
parseVersion,
parsePreRelease,
parseBuild,
parseConstraint,

-- ** Unsafe
-- | These functions can be used to unsafely parse strings. Instead of
-- returning 'Nothing', they raise an exception. Only use these if you are sure
-- the string can be successfully parsed!
unsafeParseVersion,
unsafeParsePreRelease,
unsafeParseBuild,
unsafeParseConstraint,

-- * Rendering
renderVersion,
renderPreRelease,
renderBuild,
renderConstraint,

-- * Predicates
isUnstable,
isStable,

-- * Helpers
bumpMajor,
bumpMinor,
bumpPatch,
satisfies,

-- * Lenses
-- | These lenses can be used to access and modify specific parts of a
-- 'Version'.
--
-- Don't be scared by these type signatures. They are provided in full to avoid
-- the @RankNTypes@ language extension. The type signature
-- @'Functor' f => (a -> f a) -> 'Version' -> f 'Version'@ is the same as
-- @'Lens.Micro.Lens'' 'Version' a@ (from "Lens.Micro"), which you may already
-- be familiar with.
majorLens,
minorLens,
patchLens,
preReleasesLens,
buildsLens,
) where

import Salve.Internal
