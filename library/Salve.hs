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

-- * Examples
-- | TODO

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

-- ** Constraints
-- | Partial version numbers are not allowed.
--
-- >>> parseConstraint "1.2"
-- Nothing
--
-- Wildcards (also known as "x-ranges") are not allowed.
--
-- >>> parseConstraint "1.2.x"
-- Nothing
-- >>> parseConstraint "1.2.X"
-- Nothing
-- >>> parseConstraint "1.2.*"
-- Nothing
--
-- Extra spaces are not allowed.
--
-- >>> parseConstraint " 1.2.3 "
-- Nothing
-- >>> parseConstraint "> 1.2.3"
-- Nothing
-- >>> parseConstraint "1.2.3  -  2.3.4"
-- Nothing
-- >>> parseConstraint "1.2.3  2.3.4"
-- Nothing
-- >>> parseConstraint "1.2.3  ||  2.3.4"
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
-- Pre-releases and builds are allowed on any constraints.
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
-- These examples show every type of constraint in a single expression.
--
-- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 =1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9"
-- Just "<1.2.0 <=1.2.1 1.2.2 >=1.2.3 >1.2.4 1.2.5 1.2.6 - 1.2.7 ~1.2.8 ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 <=1.2.1 || =1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
-- Just "<1.2.0 <=1.2.1 || 1.2.2 >=1.2.3 || >1.2.4 1.2.5 || 1.2.6 - 1.2.7 ~1.2.8 || ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 =1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9"
-- Just "<1.2.0 || <=1.2.1 1.2.2 || >=1.2.3 >1.2.4 || 1.2.5 1.2.6 - 1.2.7 || ~1.2.8 ^1.2.9"
-- >>> renderConstraint <$> parseConstraint "<1.2.0 || <=1.2.1 || =1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9"
-- Just "<1.2.0 || <=1.2.1 || 1.2.2 || >=1.2.3 || >1.2.4 || 1.2.5 || 1.2.6 - 1.2.7 || ~1.2.8 || ^1.2.9"

-- ** Satisfying constraints
-- | Although in general you should use 'satisfies', 'parseVersion', and
-- 'parseConstraint', doing that here makes it hard to tell what the examples
-- are doing. An operator makes things clearer.
--
-- >>> satisfies <$> parseVersion "1.2.3" <*> parseConstraint "=1.2.3"
-- Just True
-- >>> let version ? constraint = satisfies (unsafeParseVersion version) (unsafeParseConstraint constraint)
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
) where

import Salve.Internal
