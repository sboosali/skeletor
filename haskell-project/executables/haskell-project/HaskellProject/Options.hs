--------------------------------------------------
--------------------------------------------------

{-|



-}

module HaskellProject.Options where

--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative as P

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-|  

-}

data Options = Options

  { verbosity :: Verbosity
  , filepath  :: Maybe FilePath
  , project   :: Maybe String
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultOptions'@

instance Default Options where
  def = defaultOptions

{-|

-}

defaultOptions :: Options
defaultOptions = Options{..}
  where
  filepath  = def
  verbosity = def

--------------------------------------------------

{-|

-}

data Verbosity

  = Concise
  | Verbose

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultVerbosity'@

instance Default Verbosity where
  def = defaultVerbosity

-- | @= 'Concise'@

defaultVerbosity :: Verbosity
defaultVerbosity = Concise

--------------------------------------------------

{-| 

-}

options = Options

  <$> (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.metavar "VERBOSE"
        , P.help    "Enable verbose messages."
        ])

  <*> () (mconcat

        [ P.long    "project-filepath"
        , P.short   'f'
        , P.metavar "PROJECT_PATH"
        , P.help    "Which project skeleton, by path."
        ])

  <*> () (mconcat

        [ P.long    "project-name"
        , P.short   'p'
        , P.metavar "PROJECT_NAME"
        , P.help    "Which project skeleton, by name."
        , P.completeWith knownProjectNames
        ])

  -- <*> P.auto
  --       ( P.long    "--subdir"
  --      --- <> P.short   'd'
  --      <> P.metavar "SUBDIR"
  --      <> P.help    "Whether the (singleton-package) project has a separate subdirectory for its package."
  --      <> P.value   def
  --       )


--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
{- Notes

--------------------------------------------------

http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html 

    switch = flag False True



--------------------------------------------------

-}
--------------------------------------------------