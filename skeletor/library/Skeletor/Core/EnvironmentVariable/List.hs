{-# LANGUAGE CPP #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.EnvironmentVariable.List where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Text

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

getEnvironmentList :: EnvironmentName -> IO (Maybe [Text])

getEnvironmentList ev = do

  text <- getEnvironmentText ev

  let list = text <&> parseEnvironmentList

  pure list

--------------------------------------------------

parseEnvironmentList :: Text -> Maybe [Text]

parseEnvironmentList text = list
  where

  list = go text

  go = T.splitOn environmentListSeparator

--------------------------------------------------
--------------------------------------------------

{-| The character which separates items in environment variables of type list.

Platform-Specific.

For example, the @$PATH@ environment variable is a list,
separated by @':'@ on @POSIX@ platforms,
and by @';'@ on the @Windows@ platform.

One of:

* 'environmentListSeparator_POSIX' — on @Linux@, @Mac@, etc.
* 'environmentListSeparator_WIN32' — on @Windows@.
* 'environmentListSeparator_HS_ENVIRONMENT_LIST_SEPARATOR' — if (the @CPP@ macro) @HS_ENVIRONMENT_LIST_SEPARATOR@ is defined (when this package is built), and has length one (@1@, i.e. a single character).

-}

environmentListSeparator :: Char

#if defined(HS_ENVIRONMENT_LIST_SEPARATOR)
environmentListSeparator = environmentListSeparator_HS_ENVIRONMENT_LIST_SEPARATOR
#elif defined(IS_OS_POSIX) && IS_OS_POSIX
environmentListSeparator = environmentListSeparator_POSIX
#elif defined(IS_OS_WINDOWS) && IS_OS_WINDOWS
environmentListSeparator = environmentListSeparator_WIN32
#else
environmentListSeparator = ':'
#endif

--------------------------------------------------

{-| @POSIX@ separator.

is the colon:

>>> environmentListSeparator_POSIX
':'

-}

environmentListSeparator_POSIX :: Char
environmentListSeparator_POSIX = ':'

--------------------------------------------------

{-| @WIN32@ separator.

is the semicolon:

>>> environmentListSeparator_WIN32
';'

-}

environmentListSeparator_WIN32 :: Char
environmentListSeparator_WIN32 = ';'

--------------------------------------------------

{-| Configurable (at build-time) separator.

-}

environmentListSeparator_HS_ENVIRONMENT_LIST_SEPARATOR :: Char
environmentListSeparator_HS_ENVIRONMENT_LIST_SEPARATOR =
#if defined(HS_ENVIRONMENT_LIST_SEPARATOR)
  'HS_ENVIRONMENT_LIST_SEPARATOR'
#else
  ','
#endif

--------------------------------------------------
--------------------------------------------------