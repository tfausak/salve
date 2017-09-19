-- | WARNING: This module should be considered private! If you find yourself
-- wanting to import something from this module, please open an issue to get
-- that thing exported from "Salve".
module Salve.Internal where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord
import qualified Data.Word as Word
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Version

-- $setup
-- >>> import Control.Applicative
-- >>> import Lens.Micro
-- >>> let view l x = x ^. l

-- * Public

-- | A semantic version number. Versions have five parts:
--
-- 1. 'majorLens': The major version number.
-- 2. 'minorLens': The minor version number.
-- 3. 'patchLens': The patch version number.
-- 4. 'preReleasesLens': A list of pre-release identifiers.
-- 5. 'buildsLens': A list of build metadata.
--
-- Use 'parseVersion' to create versions.
data Version = Version
  { versionMajor :: Word.Word64
  , versionMinor :: Word.Word64
  , versionPatch :: Word.Word64
  , versionPreReleases :: [PreRelease]
  , versionBuilds :: [Build]
  } deriving (Eq, Show)

-- | In general, versions compare in the way that you would expect. First the
-- major version numbers are compared, then the minors, then the patches.
--
-- >>> compare <$> parseVersion "1.2.3" <*> parseVersion "2.0.0"
-- Just LT
-- >>> compare <$> parseVersion "1.2.3" <*> parseVersion "1.3.0"
-- Just LT
-- >>> compare <$> parseVersion "1.2.3" <*> parseVersion "1.2.4"
-- Just LT
--
-- Numbers are compared numerically, not alphabetically.
--
-- >>> compare <$> parseVersion "0.0.9" <*> parseVersion "0.0.10"
-- Just LT
--
-- If all the numbers are the same, the pre-releases are compared.
--
-- >>> compare <$> parseVersion "1.2.3-a" <*> parseVersion "1.2.3-b"
-- Just LT
--
-- A version with a pre-release is always less than a version without one as
-- long as the other parts are the same.
--
-- >>> compare <$> parseVersion "1.2.3-pre" <*> parseVersion "1.2.3"
-- Just LT
-- >>> compare <$> parseVersion "1.2.4-pre" <*> parseVersion "1.2.3"
-- Just GT
--
-- Builds are not considered when comparing versions.
--
-- >>> compare <$> parseVersion "1.2.3+a" <*> parseVersion "1.2.3+b"
-- Just EQ
-- >>> (==) <$> parseVersion "1.2.3+a" <*> parseVersion "1.2.3+b"
-- Just False
instance Ord Version where
  compare x y = Monoid.mconcat
    [ Ord.comparing versionMajor x y
    , Ord.comparing versionMinor x y
    , Ord.comparing versionPatch x y
    , case both versionPreReleases (x, y) of
      ([], []) -> EQ
      ([], _) -> GT
      (_, []) -> LT
      (p, q) -> compare p q
    ]

-- | Pre-release information attached to a version. These can either be numeric
-- or textual. They must not be empty.
--
-- - Numeric: Can be any non-negative integer. Cannot have leading zeros.
--
-- - Textual: Can be any string of ASCII digits, letters, or hyphens. Cannot be
--   all digits, as that would be numeric.
--
-- In general, pre-releases must match the regular expression
-- @\/^[-0-9A-Za-z]+$\/@.
--
-- Use 'parsePreRelease' to create pre-releases.
data PreRelease
  = PreReleaseNumeric Word.Word64
  | PreReleaseTextual String
  deriving (Eq, Show)

-- | Numeric pre-releases are always less than textual pre-releases.
--
-- >>> compare <$> parsePreRelease "1" <*> parsePreRelease "a"
-- Just LT
--
-- Numeric pre-releases are compared numerically.
--
-- >>> compare <$> parsePreRelease "9" <*> parsePreRelease "10"
-- Just LT
--
-- Textual pre-releases are compared alphabetically.
--
-- >>> compare <$> parsePreRelease "p10" <*> parsePreRelease "p9"
-- Just LT
instance Ord PreRelease where
  compare x y = case (x, y) of
    (PreReleaseNumeric n, PreReleaseNumeric m) -> compare n m
    (PreReleaseNumeric _, PreReleaseTextual _) -> LT
    (PreReleaseTextual _, PreReleaseNumeric _) -> GT
    (PreReleaseTextual s, PreReleaseTextual t) -> compare s t

-- | Build metadata attached to a version. These are similar to
-- 'PreRelease's with some key differences:
--
-- 1. There is no such thing as numeric builds. Even though builds can look
--    like numbers, all builds are textual.
-- 2. As a result, builds that look numeric are allowed to have leading zeros.
-- 3. Builds cannot be compared. That is, they do not have an 'Ord' instance.
--
-- Use 'parseBuild' to create builds.
newtype Build = Build String deriving (Eq, Show)

-- | Constrains allowable version numbers.
--
-- Use 'parseConstraint' to create constraints and 'satisfiesConstraint' to see
-- if a version number satisfies a constraint.
data Constraint
  = ConstraintOperator Operator Version
  | ConstraintHyphen Version Version
  | ConstraintWildcard Wildcard
  | ConstraintAnd Constraint Constraint
  | ConstraintOr Constraint Constraint
  deriving (Eq, Show)

-- | Makes a new version number.
--
-- >>> makeVersion 1 2 3 [unsafeParsePreRelease "pre"] [unsafeParseBuild "build"]
-- Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [PreReleaseTextual "pre"], versionBuilds = [Build "build"]}
--
-- This can be a useful alternative to 'parseVersion' if you want a total way
-- to create a version.
makeVersion :: Word.Word64 -> Word.Word64 -> Word.Word64 -> [PreRelease] -> [Build] -> Version
makeVersion major minor patch preReleases builds = Version
  { versionMajor = major
  , versionMinor = minor
  , versionPatch = patch
  , versionPreReleases = preReleases
  , versionBuilds = builds
  }

-- | The initial version number for development.
--
-- >>> initialVersion
-- Version {versionMajor = 0, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []}
initialVersion :: Version
initialVersion = makeVersion 0 0 0 [] []

-- | Attempts to parse a version. This parser follows
-- [SemVer's BNF](https://github.com/mojombo/semver/blob/eb9aac5/semver.md#backusnaur-form-grammar-for-valid-semver-versions).
--
-- >>> parseVersion "1.2.3-p.4+b.5"
-- Just (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [PreReleaseTextual "p",PreReleaseNumeric 4], versionBuilds = [Build "b",Build "5"]})
--
-- Returns 'Nothing' if the parse fails.
--
-- >>> parseVersion "wrong"
-- Nothing
--
-- Whitespace is not allowed and will cause the parser to fail.
--
-- >>> parseVersion " 1.2.3 "
-- Nothing
parseVersion :: String -> Maybe Version
parseVersion s = parse versionP s

-- | Attempts to parse a pre-release.
--
-- >>> parsePreRelease "pre"
-- Just (PreReleaseTextual "pre")
-- >>> parsePreRelease "1"
-- Just (PreReleaseNumeric 1)
--
-- Returns 'Nothing' if the parse fails.
--
-- >>> parsePreRelease "wrong!"
-- Nothing
--
-- Numeric pre-releases cannot contain leading zeros.
--
-- >>> parsePreRelease "01"
-- Nothing
parsePreRelease :: String -> Maybe PreRelease
parsePreRelease s = parse preReleaseP s

-- | Attempts to parse a build.
--
-- >>> parseBuild "build"
-- Just (Build "build")
-- >>> parseBuild "1"
-- Just (Build "1")
--
-- Returns 'Nothing' if the parse fails.
--
-- >>> parseBuild "wrong!"
-- Nothing
--
-- Unlike pre-releases, numeric builds can have leading zeros.
--
-- >>> parseBuild "01"
-- Just (Build "01")
parseBuild :: String -> Maybe Build
parseBuild s = parse buildP s

-- | Attempts to parse a constraint. This parser mostly follows
-- [npm's BNF](https://github.com/npm/npm/blob/d081cc6/doc/misc/semver.md#range-grammar).
--
-- >>> parseConstraint ">1.2.3"
-- Just (ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
--
-- Returns 'Nothing' if the parse fails.
--
-- >>> parseConstraint "wrong"
-- Nothing
--
-- The two departures from npm's BNF are that x-ranges cannot be used with
-- other operators and partial version numbers are not allowed.
--
-- >>> parseConstraint "1.2.x"
-- Just (ConstraintWildcard (WildcardPatch 1 2))
-- >>> parseConstraint ">=1.2.x"
-- Nothing
--
-- >>> parseConstraint "1.2"
-- Nothing
-- >>> parseConstraint ">=1.2"
-- Nothing
parseConstraint :: String -> Maybe Constraint
parseConstraint s = parse constraintsP s

-- | Parses a version.
--
-- >>> unsafeParseVersion "1.2.3-p.4+b.5"
-- Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [PreReleaseTextual "p",PreReleaseNumeric 4], versionBuilds = [Build "b",Build "5"]}
--
-- Raises an exception if the parse fails.
--
-- >>> unsafeParseVersion "wrong"
-- *** Exception: unsafeParseVersion: invalid version: "wrong"
-- ...
--
-- See 'parseVersion' for a safe version of this function.
unsafeParseVersion :: String -> Version
unsafeParseVersion s = case parseVersion s of
  Nothing -> error ("unsafeParseVersion: invalid version: " ++ show s)
  Just v -> v

-- | Parses a pre-release.
--
-- >>> unsafeParsePreRelease "pre"
-- PreReleaseTextual "pre"
--
-- Raises an exception if the parse fails.
--
-- >>> unsafeParsePreRelease "wrong!"
-- *** Exception: unsafeParsePreRelease: invalid pre-release: "wrong!"
-- ...
--
-- See 'parsePreRelease' for a safe version of this function.
unsafeParsePreRelease :: String -> PreRelease
unsafeParsePreRelease s = case parsePreRelease s of
  Nothing -> error ("unsafeParsePreRelease: invalid pre-release: " ++ show s)
  Just p -> p

-- | Parses a build.
--
-- >>> unsafeParseBuild "build"
-- Build "build"
--
-- Raises an exception if the parse fails.
--
-- >>> unsafeParseBuild "wrong!"
-- Build "*** Exception: unsafeParseBuild: invalid build: "wrong!"
-- ...
--
-- See 'parseBuild' for a safe version of this function.
unsafeParseBuild :: String -> Build
unsafeParseBuild s = case parseBuild s of
  Nothing -> error ("unsafeParseBuild: invalid build: " ++ show s)
  Just b -> b

-- | Parses a constraint.
--
-- >>> unsafeParseConstraint ">1.2.3"
-- ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})
--
-- Raises an exception if the parse fails.
--
-- >>> unsafeParseConstraint "wrong"
-- *** Exception: unsafeParseConstraint: invalid constraint: "wrong"
-- ...
--
-- See 'parseConstraint' for a safe version of this function.
unsafeParseConstraint :: String -> Constraint
unsafeParseConstraint s = case parseConstraint s of
  Nothing -> error ("unsafeParseConstraint: invalid constraint: " ++ show s)
  Just c -> c

-- | Renders a version.
--
-- >>> renderVersion <$> parseVersion "1.2.3-p.4+b.5"
-- Just "1.2.3-p.4+b.5"
renderVersion :: Version -> String
renderVersion v = concat
  [ show (versionMajor v)
  , "."
  , show (versionMinor v)
  , "."
  , show (versionPatch v)
  , renderPreReleases (versionPreReleases v)
  , renderBuilds (versionBuilds v)
  ]

-- | Renders a pre-release.
--
-- >>> renderPreRelease <$> parsePreRelease "pre"
-- Just "pre"
-- >>> renderPreRelease <$> parsePreRelease "1"
-- Just "1"
renderPreRelease :: PreRelease -> String
renderPreRelease p = case p of
  PreReleaseNumeric n -> show n
  PreReleaseTextual s -> s

-- | Renders a build.
--
-- >>> renderBuild <$> parseBuild "build"
-- Just "build"
-- >>> renderBuild <$> parseBuild "1"
-- Just "1"
renderBuild :: Build -> String
renderBuild (Build b) = b

-- | Renders a constraint.
--
-- >>> renderConstraint <$> parseConstraint ">1.2.3"
-- Just ">1.2.3"
--
-- Parsing and rendering a constraint doesn't always return what you started
-- with.
--
-- >>> renderConstraint <$> parseConstraint "=1.2.3"
-- Just "1.2.3"
renderConstraint :: Constraint -> String
renderConstraint c = case c of
  ConstraintOperator o v ->
    let s = renderVersion v
    in case o of
      OperatorLT -> '<' : s
      OperatorLE -> '<' : '=' : s
      OperatorEQ -> s
      OperatorGE -> '>' : '=' : s
      OperatorGT -> '>' : s
      OperatorTilde -> '~' : s
      OperatorCaret -> '^' : s
  ConstraintHyphen l r -> unwords [renderVersion l, "-", renderVersion r]
  ConstraintWildcard w -> case w of
    WildcardMajor -> "x.x.x"
    WildcardMinor m -> show m ++ ".x.x"
    WildcardPatch m n -> List.intercalate "." [show m, show n, "x"]
  ConstraintAnd l r -> unwords (map renderConstraint [l, r])
  ConstraintOr l r -> unwords [renderConstraint l, "||", renderConstraint r]

-- | Returns 'True' if the major version number is zero, 'False' otherwise.
--
-- >>> isUnstable <$> parseVersion "0.1.2"
-- Just True
-- >>> isUnstable <$> parseVersion "1.0.0"
-- Just False
isUnstable :: Version -> Bool
isUnstable v = versionMajor v == 0

-- | Returns 'True' if the major version number is not zero, 'False' otherwise.
--
-- >>> isStable <$> parseVersion "1.0.0"
-- Just True
-- >>> isStable <$> parseVersion "0.1.2"
-- Just False
isStable :: Version -> Bool
isStable v = not (isUnstable v)

-- | Convert from 'Data.Version'
--
-- >>> convertFromBaseVersion $ Data.Version.Version [1,2,3,4] []
-- Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}
convertFromBaseVersion :: Data.Version.Version -> Version
convertFromBaseVersion (Data.Version.Version v _) = case v of
  (m:i:p:_) -> go m i p
  (m:i:_)   -> go m i 0
  (m:_)     -> go m 0 0
  _         -> go (0 ::Int) 0 0
  where go :: Integral n => n -> n -> n -> Version
        go m i p = makeVersion (fromIntegral m) (fromIntegral i) (fromIntegral p) [] []

-- | Increments the major version number.
--
-- >>> bumpMajor <$> parseVersion "0.0.0"
-- Just (Version {versionMajor = 1, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- The minor and patch numbers are reset to zero.
--
-- >>> bumpMajor <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 2, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- The pre-releases and builds are removed.
--
-- >>> bumpMajor <$> parseVersion "0.0.0-pre+build"
-- Just (Version {versionMajor = 1, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- Consider using 'majorLens' if you want to arbitrarily change the major
-- number, or if you don't want the other parts of the version to change.
bumpMajor :: Version -> Version
bumpMajor v = makeVersion (versionMajor v + 1) 0 0 [] []

-- | Increments the minor version number.
--
-- >>> bumpMinor <$> parseVersion "0.0.0"
-- Just (Version {versionMajor = 0, versionMinor = 1, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- The patch number is reset to zero.
--
-- >>> bumpMinor <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 1, versionMinor = 3, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- The pre-releases and builds are removed.
--
-- >>> bumpMinor <$> parseVersion "0.0.0-pre+build"
-- Just (Version {versionMajor = 0, versionMinor = 1, versionPatch = 0, versionPreReleases = [], versionBuilds = []})
--
-- Consider using 'minorLens' if you want to arbitrarily change the minor
-- number, or if you don't want the other parts of the version to change.
bumpMinor :: Version -> Version
bumpMinor v = makeVersion (versionMajor v) (versionMinor v + 1) 0 [] []

-- | Increments the patch number.
--
-- >>> bumpPatch <$> parseVersion "0.0.0"
-- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 1, versionPreReleases = [], versionBuilds = []})
--
-- The major and minor numbers are not changed.
--
-- >>> bumpPatch <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 1, versionMinor = 2, versionPatch = 4, versionPreReleases = [], versionBuilds = []})
--
-- The pre-releases and builds are removed.
--
-- >>> bumpPatch <$> parseVersion "0.0.0-pre+build"
-- Just (Version {versionMajor = 0, versionMinor = 0, versionPatch = 1, versionPreReleases = [], versionBuilds = []})
--
-- Consider using 'patchLens' if you want to arbitrarily change the patch
-- number, or if you don't want the other parts of the version to change.
bumpPatch :: Version -> Version
bumpPatch v = makeVersion
  (versionMajor v) (versionMinor v) (versionPatch v + 1) [] []

-- | Returns 'True' if the version satisfies the constraint, 'False' otherwise.
--
-- >>> satisfiesConstraint <$> parseConstraint ">1.2.0" <*> parseVersion "1.2.3"
-- Just True
satisfiesConstraint :: Constraint -> Version -> Bool
satisfiesConstraint c v = satisfiesSC (toSC c) v

-- | Focuses on the major version number.
--
-- >>> view majorLens <$> parseVersion "1.2.3-pre.4+build.5"
-- Just 1
-- >>> set majorLens 4 <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 4, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})
majorLens :: Functor f => (Word.Word64 -> f Word.Word64) -> Version -> f Version
majorLens f v = fmap
  (\ m -> v { versionMajor = m })
  (f (versionMajor v))

-- | Focuses on the minor version number.
--
-- >>> view minorLens <$> parseVersion "1.2.3-pre.4+build.5"
-- Just 2
-- >>> set minorLens 4 <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 1, versionMinor = 4, versionPatch = 3, versionPreReleases = [], versionBuilds = []})
minorLens :: Functor f => (Word.Word64 -> f Word.Word64) -> Version -> f Version
minorLens f v = fmap
  (\ n -> v { versionMinor = n })
  (f (versionMinor v))

-- | Focuses on the patch version number.
--
-- >>> view patchLens <$> parseVersion "1.2.3-pre.4+build.5"
-- Just 3
-- >>> set patchLens 4 <$> parseVersion "1.2.3"
-- Just (Version {versionMajor = 1, versionMinor = 2, versionPatch = 4, versionPreReleases = [], versionBuilds = []})
patchLens :: Functor f => (Word.Word64 -> f Word.Word64) -> Version -> f Version
patchLens f v = fmap
  (\ p -> v { versionPatch = p })
  (f (versionPatch v))

-- | Focuses on the pre-release identifiers.
--
-- >>> view preReleasesLens <$> parseVersion "1.2.3-pre.4+build.5"
-- Just [PreReleaseTextual "pre",PreReleaseNumeric 4]
-- >>> set preReleasesLens [] <$> parseVersion "1.2.3-pre"
-- Just (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})
preReleasesLens :: Functor f => ([PreRelease] -> f [PreRelease]) -> Version -> f Version
preReleasesLens f v = fmap
  (\ ps -> v { versionPreReleases = ps })
  (f (versionPreReleases v))

-- | Focuses on the build metadata.
--
-- >>> view buildsLens <$> parseVersion "1.2.3-pre.4+build.5"
-- Just [Build "build",Build "5"]
-- >>> set buildsLens [] <$> parseVersion "1.2.3+build"
-- Just (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})
buildsLens :: Functor f => ([Build] -> f [Build]) -> Version -> f Version
buildsLens f v = fmap
  (\ bs -> v { versionBuilds = bs })
  (f (versionBuilds v))

-- * Private

-- ** Types

data Operator
  = OperatorLT
  | OperatorLE
  | OperatorEQ
  | OperatorGE
  | OperatorGT
  | OperatorTilde
  | OperatorCaret
  deriving (Eq, Show)

data Wildcard
  = WildcardMajor
  | WildcardMinor Word.Word64
  | WildcardPatch Word.Word64 Word.Word64
  deriving (Eq, Show)

-- ** Constructors

-- | Makes a new constraint that must be less than the version number.
--
-- >>> constraintLT <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorLT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "<1.2.3"
-- Just (ConstraintOperator OperatorLT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintLT :: Version -> Constraint
constraintLT v = ConstraintOperator OperatorLT v

-- | Makes a new constraint that must be less than or euqal to the version
-- number.
--
-- >>> constraintLE <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorLE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "<=1.2.3"
-- Just (ConstraintOperator OperatorLE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintLE :: Version -> Constraint
constraintLE v = ConstraintOperator OperatorLE v

-- | Makes a new constraint that must be equal to the version number.
--
-- >>> constraintEQ <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorEQ (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "=1.2.3"
-- Just (ConstraintOperator OperatorEQ (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintEQ :: Version -> Constraint
constraintEQ v = ConstraintOperator OperatorEQ v

-- | Makes a new constraint that must be greater than or equal to the version
-- number.
--
-- >>> constraintGE <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorGE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint ">=1.2.3"
-- Just (ConstraintOperator OperatorGE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintGE :: Version -> Constraint
constraintGE v = ConstraintOperator OperatorGE v

-- | Makes a new constraint that must be greater than the version number.
--
-- >>> constraintGT <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint ">1.2.3"
-- Just (ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintGT :: Version -> Constraint
constraintGT v = ConstraintOperator OperatorGT v

-- | Makes a new constraint that must satisfy both constraints.
--
-- >>> constraintAnd <$> (constraintGE <$> parseVersion "1.2.3") <*> (constraintLT <$> parseVersion "2.0.0")
-- Just (ConstraintAnd (ConstraintOperator OperatorGE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})) (ConstraintOperator OperatorLT (Version {versionMajor = 2, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})))
-- >>> parseConstraint ">=1.2.3 <2.0.0"
-- Just (ConstraintAnd (ConstraintOperator OperatorGE (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})) (ConstraintOperator OperatorLT (Version {versionMajor = 2, versionMinor = 0, versionPatch = 0, versionPreReleases = [], versionBuilds = []})))
constraintAnd :: Constraint -> Constraint -> Constraint
constraintAnd l r = ConstraintAnd l r

-- | Makes a new constraint that must satisfy either constraint.
--
-- >>> constraintOr <$> (constraintEQ <$> parseVersion "1.2.3") <*> (constraintGT <$> parseVersion "1.2.3")
-- Just (ConstraintOr (ConstraintOperator OperatorEQ (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})) (ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})))
-- >>> parseConstraint "=1.2.3 || >1.2.3"
-- Just (ConstraintOr (ConstraintOperator OperatorEQ (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})) (ConstraintOperator OperatorGT (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []})))
constraintOr :: Constraint -> Constraint -> Constraint
constraintOr l r = ConstraintOr l r

-- | Makes a new constraint that must be between the versions, inclusive.
--
-- >>> constraintHyphen <$> parseVersion "1.2.3" <*> parseVersion "2.3.4"
-- Just (ConstraintHyphen (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}) (Version {versionMajor = 2, versionMinor = 3, versionPatch = 4, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "1.2.3 - 2.3.4"
-- Just (ConstraintHyphen (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}) (Version {versionMajor = 2, versionMinor = 3, versionPatch = 4, versionPreReleases = [], versionBuilds = []}))
constraintHyphen :: Version -> Version -> Constraint
constraintHyphen v w = ConstraintHyphen v w

-- | Makes a new constraint that allows changes to the patch version number.
--
-- >>> constraintTilde <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorTilde (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "~1.2.3"
-- Just (ConstraintOperator OperatorTilde (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintTilde :: Version -> Constraint
constraintTilde v = ConstraintOperator OperatorTilde v

-- | Makes a new constraint that allows changes that do not modify the
-- left-most non-zero version number.
--
-- >>> constraintCaret <$> parseVersion "1.2.3"
-- Just (ConstraintOperator OperatorCaret (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
-- >>> parseConstraint "^1.2.3"
-- Just (ConstraintOperator OperatorCaret (Version {versionMajor = 1, versionMinor = 2, versionPatch = 3, versionPreReleases = [], versionBuilds = []}))
constraintCaret :: Version -> Constraint
constraintCaret v = ConstraintOperator OperatorCaret v

-- ** Parsing

versionP :: ReadP.ReadP Version
versionP = do
  major <- numberP
  Monad.void (ReadP.char '.')
  minor <- numberP
  Monad.void (ReadP.char '.')
  patch <- numberP
  preReleases <- preReleasesP
  builds <- buildsP
  return (makeVersion major minor patch preReleases builds)

preReleasesP :: ReadP.ReadP [PreRelease]
preReleasesP = ReadP.option [] (do
  Monad.void (ReadP.char '-')
  ReadP.sepBy1 preReleaseP (ReadP.char '.'))

preReleaseP :: ReadP.ReadP PreRelease
preReleaseP = preReleaseNumberP ReadP.<++ preReleaseStringP

preReleaseNumberP :: ReadP.ReadP PreRelease
preReleaseNumberP = do
  n <- numberP
  return (PreReleaseNumeric n)

preReleaseStringP :: ReadP.ReadP PreRelease
preReleaseStringP = do
  s <- ReadP.munch1 isIdentifier
  if all Char.isDigit s
    then ReadP.pfail
    else return (PreReleaseTextual s)

buildsP :: ReadP.ReadP [Build]
buildsP = ReadP.option [] (do
  Monad.void (ReadP.char '+')
  ReadP.sepBy1 buildP (ReadP.char '.'))

buildP :: ReadP.ReadP Build
buildP = do
  b <- ReadP.munch1 isIdentifier
  return (Build b)

numberP :: ReadP.ReadP Word.Word64
numberP = zeroP ReadP.<++ nonZeroP

zeroP :: ReadP.ReadP Word.Word64
zeroP = do
  Monad.void (ReadP.char '0')
  return 0

nonZeroP :: ReadP.ReadP Word.Word64
nonZeroP = do
  x <- ReadP.satisfy isAsciiDigitNonZero
  ys <- ReadP.munch Char.isDigit
  case toWord64 (stringToIntegral (x : ys)) of
    Nothing -> ReadP.pfail
    Just n -> pure n

constraintsP :: ReadP.ReadP Constraint
constraintsP = do
  spacesP
  cs <- ReadP.sepBy1 constraintP orP
  spacesP
  return (foldr1 constraintOr cs)

constraintP :: ReadP.ReadP Constraint
constraintP = do
  cs <- ReadP.sepBy1 simpleP spaces1P
  return (foldr1 constraintAnd cs)

hyphenatedP :: ReadP.ReadP Constraint
hyphenatedP = do
  v <- versionP
  hyphenP
  w <- versionP
  return (constraintHyphen v w)

simpleP :: ReadP.ReadP Constraint
simpleP = ReadP.choice [hyphenatedP, wildcardConstraintP, primitiveP]

wildcardConstraintP :: ReadP.ReadP Constraint
wildcardConstraintP = do
  ReadP.optional (ReadP.char '=')
  w <- wildcardP
  return (ConstraintWildcard w)

wildcardP :: ReadP.ReadP Wildcard
wildcardP = ReadP.choice [wildcardPatchP, wildcardMinorP, wildcardMajorP]

wildcardPatchP :: ReadP.ReadP Wildcard
wildcardPatchP = do
  m <- numberP
  Monad.void (ReadP.char '.')
  n <- numberP
  Monad.void (ReadP.char '.')
  Monad.void (ReadP.satisfy isWildcard)
  return (WildcardPatch m n)

wildcardMinorP :: ReadP.ReadP Wildcard
wildcardMinorP = do
  m <- numberP
  Monad.void (ReadP.char '.')
  Monad.void (ReadP.satisfy isWildcard)
  Monad.void (ReadP.char '.')
  Monad.void (ReadP.satisfy isWildcard)
  return (WildcardMinor m)

wildcardMajorP :: ReadP.ReadP Wildcard
wildcardMajorP = do
  Monad.void (ReadP.satisfy isWildcard)
  Monad.void (ReadP.char '.')
  Monad.void (ReadP.satisfy isWildcard)
  Monad.void (ReadP.char '.')
  Monad.void (ReadP.satisfy isWildcard)
  return WildcardMajor

primitiveP :: ReadP.ReadP Constraint
primitiveP = do
  o <- operatorP
  spacesP
  v <- versionP
  return (ConstraintOperator o v)

operatorP :: ReadP.ReadP Operator
operatorP = ReadP.choice
  [ ReadP.string "<=" >> return OperatorLE
  , ReadP.string ">=" >> return OperatorGE
  , ReadP.char '<' >> return OperatorLT
  , ReadP.char '>' >> return OperatorGT
  , ReadP.char '=' >> return OperatorEQ
  , ReadP.char '^' >> return OperatorCaret
  , ReadP.char '~' >> return OperatorTilde
  , return OperatorEQ
  ]

hyphenP :: ReadP.ReadP ()
hyphenP = do
  spaces1P
  Monad.void (ReadP.char '-')
  spaces1P

orP :: ReadP.ReadP ()
orP = do
  spaces1P
  Monad.void (ReadP.string "||")
  spaces1P

spaces1P :: ReadP.ReadP ()
spaces1P = Monad.void (ReadP.munch1 (== ' '))

spacesP :: ReadP.ReadP ()
spacesP = Monad.void (ReadP.munch (== ' '))

-- ** Rendering

renderPreReleases :: [PreRelease] -> String
renderPreReleases ps = if null ps
  then ""
  else '-' : List.intercalate "." (map renderPreRelease ps)

renderBuilds :: [Build] -> String
renderBuilds bs = if null bs
  then ""
  else '+' : List.intercalate "." (map renderBuild bs)

-- ** Helpers

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

isAsciiDigitNonZero :: Char -> Bool
isAsciiDigitNonZero c = Char.isDigit c && (c /= '0')

isIdentifier :: Char -> Bool
isIdentifier c = (Char.isAscii c && Char.isAlphaNum c) || (c == '-')

isWildcard :: Char -> Bool
isWildcard c = (c == 'x') || (c == '*') || (c == 'X')

parse :: ReadP.ReadP a -> String -> Maybe a
parse p s =
  let p' = ReadP.readP_to_S p
  in Maybe.listToMaybe (do
    (x, "") <- p' s
    return x)

stringToIntegral :: Integral a => String -> a
stringToIntegral s = foldl
  (\ n d -> (n * 10) + (fromIntegral (fromEnum d) - 48)) 0 s

toWord64 :: Integer -> Maybe Word.Word64
toWord64 n =
  if n < 0
  then Nothing
  else if n > fromIntegral (maxBound :: Word.Word64)
  then Nothing
  else Just (fromIntegral n)

-- * Simple constraints
-- | Simple constraints are just as expressive as 'Constraint's, but they are
-- easier to reason about. You can think of them as the desugared version of
-- 'Constraint's.

data SimpleConstraint
  = SCLT Version
  | SCEQ Version
  | SCGT Version
  | SCAnd SimpleConstraint SimpleConstraint
  | SCOr SimpleConstraint SimpleConstraint
  deriving (Eq, Show)

mkV :: Word.Word64 -> Word.Word64 -> Word.Word64 -> Version
mkV m n p = makeVersion m n p [] []

satisfiesSC :: SimpleConstraint -> Version -> Bool
satisfiesSC c v = case c of
  SCLT u -> v < u
  -- This uses `compare` rather than `==` to ignore build metadata.
  SCEQ u -> compare v u == EQ
  SCGT u -> v > u
  SCAnd l r -> satisfiesSC l v && satisfiesSC r v
  SCOr l r -> satisfiesSC l v || satisfiesSC r v

scLE :: Version -> SimpleConstraint
scLE v = SCOr (SCLT v) (SCEQ v)

scGE :: Version -> SimpleConstraint
scGE v = SCOr (SCGT v) (SCEQ v)

toSC :: Constraint -> SimpleConstraint
toSC c = case c of
  ConstraintOperator o v -> case o of
    OperatorLT -> SCLT v
    OperatorLE -> scLE v
    OperatorEQ -> SCEQ v
    OperatorGE -> scGE v
    OperatorGT -> SCGT v
    OperatorTilde -> SCAnd
      (scGE v)
      (SCLT (mkV (versionMajor v) (versionMinor v + 1) 0))
    OperatorCaret -> SCAnd
      (scGE v)
      (SCLT (case (versionMajor v, versionMinor v, versionPatch v) of
        (0, 0, p) -> mkV 0 0 (p + 1)
        (0, n, _) -> mkV 0 (n + 1) 0
        (m, _, _) -> mkV (m + 1) 0 0))
  ConstraintHyphen l h -> SCAnd (scGE l) (scLE h)
  ConstraintWildcard w -> case w of
    WildcardMajor -> scGE initialVersion
    WildcardMinor m -> SCAnd (scGE (mkV m 0 0)) (SCLT (mkV (m + 1) 0 0))
    WildcardPatch m n -> SCAnd (scGE (mkV m n 0)) (SCLT (mkV m (n + 1) 0))
  ConstraintAnd l r -> SCAnd (toSC l) (toSC r)
  ConstraintOr l r -> SCOr (toSC l) (toSC r)
