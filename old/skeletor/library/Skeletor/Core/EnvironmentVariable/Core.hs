--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.EnvironmentVariable.Core where

--------------------------------------------------
-- Imports (Project/Internal) --------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-| Lookup and parse the current value
of the (given) "well-named" Environment Variable.

= Platform-Compatibility

NOTE Because .

-}

getEnvironment
  :: (FromEnvironment a)
  => EnvironmentName -> IO (Maybe a)

getEnvironment (EnvironmentName name) = do

  return Nothing                -- TODO

--------------------------------------------------
--------------------------------------------------

{-| Set the (given) "well-named" Environment Variable
to the (given) "well-valued" value.

= Platform-Compatibility

NOTE Because .

-}

setEnvironment
  :: (ToEnvironment a)
  => EnvironmentName -> a -> IO ()

setEnvironment (EnvironmentName name) x = do

  let string = T.unpack value

  unsafelySetEnvironmentVariable name (Just string)
--------------------------------------------------
--------------------------------------------------

{-| Unset the (given) "well-named" Environment Variable.

= Platform-Compatibility

NOTE Because .

-}

unsetEnvironment :: EnvironmentName -> IO ()

unsetEnvironment (EnvironmentName name) = do

  let string = T.unpack value

  unsafelySetEnvironmentVariable name Nothing

--------------------------------------------------
--------------------------------------------------