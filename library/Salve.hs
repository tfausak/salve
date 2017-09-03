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
-- This module provides lenses for modifying versions. If you want to modify
-- versions, consider importing a lens library like "Lens.Micro".
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
--
-- == __Examples__
--
-- === Versions
--
-- No leading zeros.
--
-- >>> parseVersion "01.0.0"
-- Nothing
-- >>> parseVersion "0.01.0"
-- Nothing
-- >>> parseVersion "0.0.01"
-- Nothing
--
-- No negative numbers.
--
-- >>> parseVersion "-1.0.0"
-- Nothing
-- >>> parseVersion "0.-1.0"
-- Nothing
-- >>> parseVersion "0.0.-1"
-- Nothing
--
-- No non-digits.
--
-- >>> parseVersion "a.0.0"
-- Nothing
-- >>> parseVersion "0.a.0"
-- Nothing
-- >>> parseVersion "0.0.a"
-- Nothing
--
-- No partial version numbers.
--
-- >>> parseVersion "0.0"
-- Nothing
--
-- No extra version numbers.
--
-- >>> parseVersion "0.0.0.0"
-- Nothing
--
-- === Constraints
--
-- No partial version numbers.
--
-- >>> parseConstraint "1.2"
-- Nothing
--
-- No wildcards.
--
-- >>> parseConstraint "1.2.x"
-- Nothing
-- >>> parseConstraint "1.2.X"
-- Nothing
-- >>> parseConstraint "1.2.*"
-- Nothing
--
-- Round-tripping.
--
-- >>> renderConstraint <$> parseConstraint "<1.2.3"
-- Just "<1.2.3"
-- >>> renderConstraint <$> parseConstraint "<=1.2.3"
-- Just "<=1.2.3"
-- >>> renderConstraint <$> parseConstraint "=1.2.3"
-- Just "1.2.3"
-- >>> renderConstraint <$> parseConstraint ">=1.2.3"
-- Just ">=1.2.3"
-- >>> renderConstraint <$> parseConstraint ">1.2.3"
-- Just ">1.2.3"
-- >>> renderConstraint <$> parseConstraint ">1.2.3 <2.0.0"
-- Just ">1.2.3 <2.0.0"
-- >>> renderConstraint <$> parseConstraint "1.2.3 || >1.2.3"
-- Just "1.2.3 || >1.2.3"
--
-- Implicit equals.
--
-- >>> renderConstraint <$> parseConstraint "1.2.3"
-- Just "1.2.3"
--
-- Hyphens.
--
-- >>> renderConstraint <$> parseConstraint "1.2.3 - 2.3.4"
-- Just "1.2.3 - 2.3.4"
--
-- Tildes.
--
-- >>> renderConstraint <$> parseConstraint "~1.2.3"
-- Just "~1.2.3"
-- >>> renderConstraint <$> parseConstraint "~1.2.0"
-- Just "~1.2.0"
-- >>> renderConstraint <$> parseConstraint "~1.0.0"
-- Just "~1.0.0"
--
-- Carets.
--
-- >>> renderConstraint <$> parseConstraint "^1.2.3"
-- Just "^1.2.3"
-- >>> renderConstraint <$> parseConstraint "^0.2.3"
-- Just "^0.2.3"
-- >>> renderConstraint <$> parseConstraint "^0.0.3"
-- Just "^0.0.3"
--
-- Pre-releases and builds.
--
-- >>> renderConstraint <$> parseConstraint "1.2.3-p+b"
-- Just "1.2.3-p+b"
-- >>> renderConstraint <$> parseConstraint "1.2.3-p+b - 2.3.4-p+b"
-- Just "1.2.3-p+b - 2.3.4-p+b"
-- >>> renderConstraint <$> parseConstraint "~1.2.3-p+b"
-- Just "~1.2.3-p+b"
-- >>> renderConstraint <$> parseConstraint "^1.2.3-p+b"
-- Just "^1.2.3-p+b"
--
-- Kitchen sink.
--
-- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 =1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9"
-- Just "<1.2.0 <=1.2.1 1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
-- Just "<1.2.0 <=1.2.1 || 1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 =1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9"
-- Just "<1.2.0 || <=1.2.1 1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 || =1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9"
-- Just "<1.2.0 || <=1.2.1 || 1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9"
--
-- === Satisfying constraints
--
-- >>> let unsafeSatisfies version constraint = satisfies (unsafeParseVersion version) (unsafeParseConstraint constraint)
--
-- Less than.
--
-- >>> unsafeSatisfies "1.2.2" "<1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.3" "<1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.4" "<1.2.3"
-- False
--
-- Less than or equal to.
--
-- >>> unsafeSatisfies "1.2.2" "<=1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.3" "<=1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" "<=1.2.3"
-- False
--
-- Equal to.
--
-- >>> unsafeSatisfies "1.2.2" "=1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" "=1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" "=1.2.3"
-- False
--
-- Greater than or equal to.
--
-- >>> unsafeSatisfies "1.2.2" ">=1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" ">=1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" ">=1.2.3"
-- True
--
-- Greater than.
--
-- >>> unsafeSatisfies "1.2.2" ">1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" ">1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.4" ">1.2.3"
-- True
--
-- And.
--
-- >>> unsafeSatisfies "1.2.3" ">1.2.3 <2.0.0"
-- False
-- >>> unsafeSatisfies "1.2.4" ">1.2.3 <2.0.0"
-- True
-- >>> unsafeSatisfies "1.3.0" ">1.2.3 <2.0.0"
-- True
-- >>> unsafeSatisfies "2.0.0" ">1.2.3 <2.0.0"
-- False
--
-- Or.
--
-- >>> unsafeSatisfies "1.2.2" "1.2.3 || >1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" "1.2.3 || >1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" "1.2.3 || >1.2.3"
-- True
--
-- Hyphen.
--
-- >>> unsafeSatisfies "1.2.2" "1.2.3 - 1.2.4"
-- False
-- >>> unsafeSatisfies "1.2.3" "1.2.3 - 1.2.4"
-- True
-- >>> unsafeSatisfies "1.2.4" "1.2.3 - 1.2.4"
-- True
-- >>> unsafeSatisfies "1.2.5" "1.2.3 - 1.2.4"
-- False
--
-- Tilde.
--
-- >>> unsafeSatisfies "1.2.2" "~1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" "~1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" "~1.2.3"
-- True
-- >>> unsafeSatisfies "1.3.0" "~1.2.3"
-- False
--
-- Caret.
--
-- >>> unsafeSatisfies "0.0.2" "^0.0.3"
-- False
-- >>> unsafeSatisfies "0.0.3" "^0.0.3"
-- True
-- >>> unsafeSatisfies "0.0.4" "^0.0.3"
-- False
--
-- >>> unsafeSatisfies "0.2.2" "^0.2.3"
-- False
-- >>> unsafeSatisfies "0.2.3" "^0.2.3"
-- True
-- >>> unsafeSatisfies "0.2.4" "^0.2.3"
-- True
-- >>> unsafeSatisfies "0.3.0" "^0.2.3"
-- False
--
-- >>> unsafeSatisfies "1.2.2" "^1.2.3"
-- False
-- >>> unsafeSatisfies "1.2.3" "^1.2.3"
-- True
-- >>> unsafeSatisfies "1.2.4" "^1.2.3"
-- True
-- >>> unsafeSatisfies "1.3.0" "^1.2.3"
-- True
-- >>> unsafeSatisfies "2.0.0" "^1.2.3"
-- False

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
constraintHyphen,
constraintTilde,
constraintCaret,

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
