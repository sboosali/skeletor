{-# LANGUAGE TypeApplications #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.License

  ( module Skeletor.Haskell.License.Types
  , module Skeletor.Haskell.License
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell.License.Types

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
--import           "containers" Data.Map (Map)

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{- | the default "permissive" (or "lax") license, 'Apache_2_0'.

'Apache_2_0' is a permissive license.

Apache 2.0 vs BSD 3 Clause:

* both are permissive
* Apache 2.0 prevents patent trolling (TODO)

See <https://www.gnu.org/licenses/license-list.html#apache2>

NOTE the BSD 3 Clause license, a.k.a. the Modified BSD license,
is the most common license on Hackage. See this script:

@
$ tar -xvf stackage-lts-13.7/00-index.tar

$ ls stackage-lts-13.7 | wc -l
2327

$ find -L ./stackage-lts-13.7 -type f -name "*.cabal" -printf "| %p\n" | wc -l
2326

$ find -L ./stackage-lts-13.7 -type f -name "*.cabal" -exec grep -i 'license:' '{}'  ';'
...

$ find -L ./stackage-lts-13.7 -type f -name "*.cabal" -exec grep -i '^license:' '{}' ';' | tr -d " \t\r" | cut -c 9- | sort | uniq -c | sort -rn

   1467 BSD3
    471 MIT
    100 MPL-2.0
     49 PublicDomain
     36 Apache-2.0
     35 GPL-3
     31 BSD2
     24 
     22 GPL
     19 OtherLicense
     18 LGPL-2.1
     13 GPL-2
     11 LGPL
      8 LGPL-3
      8 Apache
      5 ISC
      4 BSD-3-Clause
@

-}

defaultLicense :: License
defaultLicense = Apache_2_0

--------------------------------------------------


{- | the default free (or "restrictive") license, 'GPL_3_0_or_later'.

'GPL_3_0_or_later' is a permissive license.

-}

defaultFLOSSLicense :: License
defaultFLOSSLicense = GPL_3_0_or_later

--------------------------------------------------
--------------------------------------------------

-- | @≡ 'constructors'@

allLicenses :: [SpdxLicenseIdentifier]
allLicenses = constructors (Proxy @SpdxLicenseIdentifier)

--------------------------------------------------

{- | all licenses approved by the Open Source Initiative.

'allLicenses' filtered by 'licenseIsOsiApproved'.

See <https://opensource.org/about>.

-}

allOSILicenses :: [SpdxLicenseIdentifier]
allOSILicenses = allLicenses & filter licenseIsOsiApproved

--------------------------------------------------

{- | all licenses approved by the Open Source Initiative.

'allLicenses' filtered by 'licenseIsFlossCompatible'.

See <https://www.gnu.org/licenses/license-list.html>. (TODO)

-}

allFLOSSLicenses :: [SpdxLicenseIdentifier]
allFLOSSLicenses = allLicenses & filter licenseIsFlossCompatible

--------------------------------------------------
--------------------------------------------------

-- | the 'licenseId' of 'allLicenses'.

knownLicenseIds :: [String]
knownLicenseIds = (licenseId <$> allLicenses)

--------------------------------------------------

-- | the 'licenseId' of 'allOSILicenses'.

knownOSILicenseIds :: [String]
knownOSILicenseIds = (licenseId <$> allOSILicenses)

--------------------------------------------------

-- | the 'licenseId' of 'allFLOSSLicenses'.

knownFLOSSLicenseIds :: [String]
knownFLOSSLicenseIds = (licenseId <$> allFLOSSLicenses)

--------------------------------------------------
--------------------------------------------------

-- | @≡ 'parseLicense'@

parseSpdxLicenseIdentifier :: (MonadThrow m) => String -> m SpdxLicenseIdentifier
parseSpdxLicenseIdentifier = parseLicense

--------------------------------------------------

-- | Inverts 'licenseId'.

parseLicense :: (MonadThrow m) => String -> m License
parseLicense =

  (mkParserFromPrinterWith "SPDX License" licenseId) allLicenses

--------------------------------------------------

-- | Inverts 'licenseId', but limited to 'allOSILicenses'.

parseOSILicense :: (MonadThrow m) => String -> m License
parseOSILicense =

  (mkParserFromPrinterWith "OSI-Approved SPDX License" licenseId) allOSILicenses

--------------------------------------------------

-- | Inverts 'licenseId', but limited to 'allFLOSSLicenses'.

parseFLOSSLicense :: (MonadThrow m) => String -> m License
parseFLOSSLicense =

  (mkParserFromPrinterWith "FLOSS-Conformant SPDX License" licenseId) allFLOSSLicenses

--------------------------------------------------
--------------------------------------------------

{- | License SPDX identifier, e.g. @"BSD-3-Clause"@.

-}

licenseId :: SpdxLicenseIdentifier -> String

licenseId = \case

  NullBSD                                          -> "0BSD"
  AAL                                              -> "AAL"
  Abstyles                                         -> "Abstyles"
  Adobe_2006                                       -> "Adobe-2006"
  Adobe_Glyph                                      -> "Adobe-Glyph"
  ADSL                                             -> "ADSL"
  AFL_1_1                                          -> "AFL-1.1"
  AFL_1_2                                          -> "AFL-1.2"
  AFL_2_0                                          -> "AFL-2.0"
  AFL_2_1                                          -> "AFL-2.1"
  AFL_3_0                                          -> "AFL-3.0"
  Afmparse                                         -> "Afmparse"
  AGPL_1_0                                         -> "AGPL-1.0"
  AGPL_1_0_only                                    -> "AGPL-1.0-only"
  AGPL_1_0_or_later                                -> "AGPL-1.0-or-later"
  AGPL_3_0_only                                    -> "AGPL-3.0-only"
  AGPL_3_0_or_later                                -> "AGPL-3.0-or-later"
  Aladdin                                          -> "Aladdin"
  AMDPLPA                                          -> "AMDPLPA"
  AML                                              -> "AML"
  AMPAS                                            -> "AMPAS"
  ANTLR_PD                                         -> "ANTLR-PD"
  Apache_1_0                                       -> "Apache-1.0"
  Apache_1_1                                       -> "Apache-1.1"
  Apache_2_0                                       -> "Apache-2.0"
  APAFML                                           -> "APAFML"
  APL_1_0                                          -> "APL-1.0"
  APSL_1_0                                         -> "APSL-1.0"
  APSL_1_1                                         -> "APSL-1.1"
  APSL_1_2                                         -> "APSL-1.2"
  APSL_2_0                                         -> "APSL-2.0"
  Artistic_1_0_cl8                                 -> "Artistic-1.0-cl8"
  Artistic_1_0_Perl                                -> "Artistic-1.0-Perl"
  Artistic_1_0                                     -> "Artistic-1.0"
  Artistic_2_0                                     -> "Artistic-2.0"
  Bahyph                                           -> "Bahyph"
  Barr                                             -> "Barr"
  Beerware                                         -> "Beerware"
  BitTorrent_1_0                                   -> "BitTorrent-1.0"
  BitTorrent_1_1                                   -> "BitTorrent-1.1"
  Borceux                                          -> "Borceux"
  BSD_1_Clause                                     -> "BSD-1-Clause"
  BSD_2_Clause_FreeBSD                             -> "BSD-2-Clause-FreeBSD"
  BSD_2_Clause_NetBSD                              -> "BSD-2-Clause-NetBSD"
  BSD_2_Clause_Patent                              -> "BSD-2-Clause-Patent"
  BSD_2_Clause                                     -> "BSD-2-Clause"
  BSD_3_Clause_Attribution                         -> "BSD-3-Clause-Attribution"
  BSD_3_Clause_Clear                               -> "BSD-3-Clause-Clear"
  BSD_3_Clause_LBNL                                -> "BSD-3-Clause-LBNL"
  BSD_3_Clause_No_Nuclear_License_2014             -> "BSD-3-Clause-No-Nuclear-License-2014"
  BSD_3_Clause_No_Nuclear_License                  -> "BSD-3-Clause-No-Nuclear-License"
  BSD_3_Clause_No_Nuclear_Warranty                 -> "BSD-3-Clause-No-Nuclear-Warranty"
  BSD_3_Clause                                     -> "BSD-3-Clause"
  BSD_4_Clause_UC                                  -> "BSD-4-Clause-UC"
  BSD_4_Clause                                     -> "BSD-4-Clause"
  BSD_Protection                                   -> "BSD-Protection"
  BSD_Source_Code                                  -> "BSD-Source-Code"
  BSL_1_0                                          -> "BSL-1.0"
  Bzip2_1_0_5                                      -> "bzip2-1.0.5"
  Bzip2_1_0_6                                      -> "bzip2-1.0.6"
  Caldera                                          -> "Caldera"
  CATOSL_1_1                                       -> "CATOSL-1.1"
  CC_BY_1_0                                        -> "CC-BY-1.0"
  CC_BY_2_0                                        -> "CC-BY-2.0"
  CC_BY_2_5                                        -> "CC-BY-2.5"
  CC_BY_3_0                                        -> "CC-BY-3.0"
  CC_BY_4_0                                        -> "CC-BY-4.0"
  CC_BY_NC_1_0                                     -> "CC-BY-NC-1.0"
  CC_BY_NC_2_0                                     -> "CC-BY-NC-2.0"
  CC_BY_NC_2_5                                     -> "CC-BY-NC-2.5"
  CC_BY_NC_3_0                                     -> "CC-BY-NC-3.0"
  CC_BY_NC_4_0                                     -> "CC-BY-NC-4.0"
  CC_BY_NC_ND_1_0                                  -> "CC-BY-NC-ND-1.0"
  CC_BY_NC_ND_2_0                                  -> "CC-BY-NC-ND-2.0"
  CC_BY_NC_ND_2_5                                  -> "CC-BY-NC-ND-2.5"
  CC_BY_NC_ND_3_0                                  -> "CC-BY-NC-ND-3.0"
  CC_BY_NC_ND_4_0                                  -> "CC-BY-NC-ND-4.0"
  CC_BY_NC_SA_1_0                                  -> "CC-BY-NC-SA-1.0"
  CC_BY_NC_SA_2_0                                  -> "CC-BY-NC-SA-2.0"
  CC_BY_NC_SA_2_5                                  -> "CC-BY-NC-SA-2.5"
  CC_BY_NC_SA_3_0                                  -> "CC-BY-NC-SA-3.0"
  CC_BY_NC_SA_4_0                                  -> "CC-BY-NC-SA-4.0"
  CC_BY_ND_1_0                                     -> "CC-BY-ND-1.0"
  CC_BY_ND_2_0                                     -> "CC-BY-ND-2.0"
  CC_BY_ND_2_5                                     -> "CC-BY-ND-2.5"
  CC_BY_ND_3_0                                     -> "CC-BY-ND-3.0"
  CC_BY_ND_4_0                                     -> "CC-BY-ND-4.0"
  CC_BY_SA_1_0                                     -> "CC-BY-SA-1.0"
  CC_BY_SA_2_0                                     -> "CC-BY-SA-2.0"
  CC_BY_SA_2_5                                     -> "CC-BY-SA-2.5"
  CC_BY_SA_3_0                                     -> "CC-BY-SA-3.0"
  CC_BY_SA_4_0                                     -> "CC-BY-SA-4.0"
  CC0_1_0                                          -> "CC0-1.0"
  CDDL_1_0                                         -> "CDDL-1.0"
  CDDL_1_1                                         -> "CDDL-1.1"
  CDLA_Permissive_1_0                              -> "CDLA-Permissive-1.0"
  CDLA_Sharing_1_0                                 -> "CDLA-Sharing-1.0"
  CECILL_1_0                                       -> "CECILL-1.0"
  CECILL_1_1                                       -> "CECILL-1.1"
  CECILL_2_0                                       -> "CECILL-2.0"
  CECILL_2_1                                       -> "CECILL-2.1"
  CECILL_B                                         -> "CECILL-B"
  CECILL_C                                         -> "CECILL-C"
  ClArtistic                                       -> "ClArtistic"
  CNRI_Jython                                      -> "CNRI-Jython"
  CNRI_Python_GPL_Compatible                       -> "CNRI-Python-GPL-Compatible"
  CNRI_Python                                      -> "CNRI-Python"
  Condor_1_1                                       -> "Condor-1.1"
  CPAL_1_0                                         -> "CPAL-1.0"
  CPL_1_0                                          -> "CPL-1.0"
  CPOL_1_02                                        -> "CPOL-1.02"
  Crossword                                        -> "Crossword"
  CrystalStacker                                   -> "CrystalStacker"
  CUA_OPL_1_0                                      -> "CUA-OPL-1.0"
  Cube                                             -> "Cube"
  Curl                                             -> "curl"
  D_FSL_1_0                                        -> "D-FSL-1.0"
  Diffmark                                         -> "diffmark"
  DOC                                              -> "DOC"
  Dotseqn                                          -> "Dotseqn"
  DSDP                                             -> "DSDP"
  Dvipdfm                                          -> "dvipdfm"
  ECL_1_0                                          -> "ECL-1.0"
  ECL_2_0                                          -> "ECL-2.0"
  EFL_1_0                                          -> "EFL-1.0"
  EFL_2_0                                          -> "EFL-2.0"
  EGenix                                           -> "eGenix"
  Entessa                                          -> "Entessa"
  EPL_1_0                                          -> "EPL-1.0"
  EPL_2_0                                          -> "EPL-2.0"
  ErlPL_1_1                                        -> "ErlPL-1.1"
  EUDatagrid                                       -> "EUDatagrid"
  EUPL_1_0                                         -> "EUPL-1.0"
  EUPL_1_1                                         -> "EUPL-1.1"
  EUPL_1_2                                         -> "EUPL-1.2"
  Eurosym                                          -> "Eurosym"
  Fair                                             -> "Fair"
  Frameworx_1_0                                    -> "Frameworx-1.0"
  FreeImage                                        -> "FreeImage"
  FSFAP                                            -> "FSFAP"
  FSFUL                                            -> "FSFUL"
  FSFULLR                                          -> "FSFULLR"
  FTL                                              -> "FTL"
  GFDL_1_1_only                                    -> "GFDL-1.1-only"
  GFDL_1_1_or_later                                -> "GFDL-1.1-or-later"
  GFDL_1_2_only                                    -> "GFDL-1.2-only"
  GFDL_1_2_or_later                                -> "GFDL-1.2-or-later"
  GFDL_1_3_only                                    -> "GFDL-1.3-only"
  GFDL_1_3_or_later                                -> "GFDL-1.3-or-later"
  Giftware                                         -> "Giftware"
  GL2PS                                            -> "GL2PS"
  Glide                                            -> "Glide"
  Glulxe                                           -> "Glulxe"
  Gnuplot                                          -> "gnuplot"
  GPL_1_0_only                                     -> "GPL-1.0-only"
  GPL_1_0_or_later                                 -> "GPL-1.0-or-later"
  GPL_2_0_only                                     -> "GPL-2.0-only"
  GPL_2_0_or_later                                 -> "GPL-2.0-or-later"
  GPL_3_0_only                                     -> "GPL-3.0-only"
  GPL_3_0_or_later                                 -> "GPL-3.0-or-later"
  GSOAP_1_3b                                       -> "gSOAP-1.3b"
  HaskellReport                                    -> "HaskellReport"
  HPND                                             -> "HPND"
  IBM_pibs                                         -> "IBM-pibs"
  ICU                                              -> "ICU"
  IJG                                              -> "IJG"
  ImageMagick                                      -> "ImageMagick"
  IMatix                                           -> "iMatix"
  Imlib2                                           -> "Imlib2"
  Info_ZIP                                         -> "Info-ZIP"
  Intel_ACPI                                       -> "Intel-ACPI"
  Intel                                            -> "Intel"
  Interbase_1_0                                    -> "Interbase-1.0"
  IPA                                              -> "IPA"
  IPL_1_0                                          -> "IPL-1.0"
  ISC                                              -> "ISC"
  JasPer_2_0                                       -> "JasPer-2.0"
  JSON                                             -> "JSON"
  LAL_1_2                                          -> "LAL-1.2"
  LAL_1_3                                          -> "LAL-1.3"
  Latex2e                                          -> "Latex2e"
  Leptonica                                        -> "Leptonica"
  LGPL_2_0_only                                    -> "LGPL-2.0-only"
  LGPL_2_0_or_later                                -> "LGPL-2.0-or-later"
  LGPL_2_1_only                                    -> "LGPL-2.1-only"
  LGPL_2_1_or_later                                -> "LGPL-2.1-or-later"
  LGPL_3_0_only                                    -> "LGPL-3.0-only"
  LGPL_3_0_or_later                                -> "LGPL-3.0-or-later"
  LGPLLR                                           -> "LGPLLR"
  Libpng                                           -> "Libpng"
  Libtiff                                          -> "libtiff"
  LiLiQ_P_1_1                                      -> "LiLiQ-P-1.1"
  LiLiQ_R_1_1                                      -> "LiLiQ-R-1.1"
  LiLiQ_Rplus_1_1                                  -> "LiLiQ-Rplus-1.1"
  Linux_OpenIB                                     -> "Linux-OpenIB"
  LPL_1_0                                          -> "LPL-1.0"
  LPL_1_02                                         -> "LPL-1.02"
  LPPL_1_0                                         -> "LPPL-1.0"
  LPPL_1_1                                         -> "LPPL-1.1"
  LPPL_1_2                                         -> "LPPL-1.2"
  LPPL_1_3a                                        -> "LPPL-1.3a"
  LPPL_1_3c                                        -> "LPPL-1.3c"
  MakeIndex                                        -> "MakeIndex"
  MirOS                                            -> "MirOS"
  MIT_0                                            -> "MIT-0"
  MIT_advertising                                  -> "MIT-advertising"
  MIT_CMU                                          -> "MIT-CMU"
  MIT_enna                                         -> "MIT-enna"
  MIT_feh                                          -> "MIT-feh"
  MIT                                              -> "MIT"
  MITNFA                                           -> "MITNFA"
  Motosoto                                         -> "Motosoto"
  Mpich2                                           -> "mpich2"
  MPL_1_0                                          -> "MPL-1.0"
  MPL_1_1                                          -> "MPL-1.1"
  MPL_2_0_no_copyleft_exception                    -> "MPL-2.0-no-copyleft-exception"
  MPL_2_0                                          -> "MPL-2.0"
  MS_PL                                            -> "MS-PL"
  MS_RL                                            -> "MS-RL"
  MTLL                                             -> "MTLL"
  Multics                                          -> "Multics"
  Mup                                              -> "Mup"
  NASA_1_3                                         -> "NASA-1.3"
  Naumen                                           -> "Naumen"
  NBPL_1_0                                         -> "NBPL-1.0"
  NCSA                                             -> "NCSA"
  Net_SNMP                                         -> "Net-SNMP"
  NetCDF                                           -> "NetCDF"
  Newsletr                                         -> "Newsletr"
  NGPL                                             -> "NGPL"
  NLOD_1_0                                         -> "NLOD-1.0"
  NLPL                                             -> "NLPL"
  Nokia                                            -> "Nokia"
  NOSL                                             -> "NOSL"
  Noweb                                            -> "Noweb"
  NPL_1_0                                          -> "NPL-1.0"
  NPL_1_1                                          -> "NPL-1.1"
  NPOSL_3_0                                        -> "NPOSL-3.0"
  NRL                                              -> "NRL"
  NTP                                              -> "NTP"
  OCCT_PL                                          -> "OCCT-PL"
  OCLC_2_0                                         -> "OCLC-2.0"
  ODbL_1_0                                         -> "ODbL-1.0"
  ODC_By_1_0                                       -> "ODC-By-1.0"
  OFL_1_0                                          -> "OFL-1.0"
  OFL_1_1                                          -> "OFL-1.1"
  OGTSL                                            -> "OGTSL"
  OLDAP_1_1                                        -> "OLDAP-1.1"
  OLDAP_1_2                                        -> "OLDAP-1.2"
  OLDAP_1_3                                        -> "OLDAP-1.3"
  OLDAP_1_4                                        -> "OLDAP-1.4"
  OLDAP_2_0_1                                      -> "OLDAP-2.0.1"
  OLDAP_2_0                                        -> "OLDAP-2.0"
  OLDAP_2_1                                        -> "OLDAP-2.1"
  OLDAP_2_2_1                                      -> "OLDAP-2.2.1"
  OLDAP_2_2_2                                      -> "OLDAP-2.2.2"
  OLDAP_2_2                                        -> "OLDAP-2.2"
  OLDAP_2_3                                        -> "OLDAP-2.3"
  OLDAP_2_4                                        -> "OLDAP-2.4"
  OLDAP_2_5                                        -> "OLDAP-2.5"
  OLDAP_2_6                                        -> "OLDAP-2.6"
  OLDAP_2_7                                        -> "OLDAP-2.7"
  OLDAP_2_8                                        -> "OLDAP-2.8"
  OML                                              -> "OML"
  OpenSSL                                          -> "OpenSSL"
  OPL_1_0                                          -> "OPL-1.0"
  OSET_PL_2_1                                      -> "OSET-PL-2.1"
  OSL_1_0                                          -> "OSL-1.0"
  OSL_1_1                                          -> "OSL-1.1"
  OSL_2_0                                          -> "OSL-2.0"
  OSL_2_1                                          -> "OSL-2.1"
  OSL_3_0                                          -> "OSL-3.0"
  PDDL_1_0                                         -> "PDDL-1.0"
  PHP_3_0                                          -> "PHP-3.0"
  PHP_3_01                                         -> "PHP-3.01"
  Plexus                                           -> "Plexus"
  PostgreSQL                                       -> "PostgreSQL"
  Psfrag                                           -> "psfrag"
  Psutils                                          -> "psutils"
  Python_2_0                                       -> "Python-2.0"
  Qhull                                            -> "Qhull"
  QPL_1_0                                          -> "QPL-1.0"
  Rdisc                                            -> "Rdisc"
  RHeCos_1_1                                       -> "RHeCos-1.1"
  RPL_1_1                                          -> "RPL-1.1"
  RPL_1_5                                          -> "RPL-1.5"
  RPSL_1_0                                         -> "RPSL-1.0"
  RSA_MD                                           -> "RSA-MD"
  RSCPL                                            -> "RSCPL"
  Ruby                                             -> "Ruby"
  SAX_PD                                           -> "SAX-PD"
  Saxpath                                          -> "Saxpath"
  SCEA                                             -> "SCEA"
  Sendmail                                         -> "Sendmail"
  SGI_B_1_0                                        -> "SGI-B-1.0"
  SGI_B_1_1                                        -> "SGI-B-1.1"
  SGI_B_2_0                                        -> "SGI-B-2.0"
  SimPL_2_0                                        -> "SimPL-2.0"
  SISSL_1_2                                        -> "SISSL-1.2"
  SISSL                                            -> "SISSL"
  Sleepycat                                        -> "Sleepycat"
  SMLNJ                                            -> "SMLNJ"
  SMPPL                                            -> "SMPPL"
  SNIA                                             -> "SNIA"
  Spencer_86                                       -> "Spencer-86"
  Spencer_94                                       -> "Spencer-94"
  Spencer_99                                       -> "Spencer-99"
  SPL_1_0                                          -> "SPL-1.0"
  SugarCRM_1_1_3                                   -> "SugarCRM-1.1.3"
  SWL                                              -> "SWL"
  TCL                                              -> "TCL"
  TCP_wrappers                                     -> "TCP-wrappers"
  TMate                                            -> "TMate"
  TORQUE_1_1                                       -> "TORQUE-1.1"
  TOSL                                             -> "TOSL"
  TU_Berlin_1_0                                    -> "TU-Berlin-1.0"
  TU_Berlin_2_0                                    -> "TU-Berlin-2.0"
  Unicode_DFS_2015                                 -> "Unicode-DFS-2015"
  Unicode_DFS_2016                                 -> "Unicode-DFS-2016"
  Unicode_TOU                                      -> "Unicode-TOU"
  Unlicense                                        -> "Unlicense"
  UPL_1_0                                          -> "UPL-1.0"
  Vim                                              -> "Vim"
  VOSTROM                                          -> "VOSTROM"
  VSL_1_0                                          -> "VSL-1.0"
  W3C_19980720                                     -> "W3C-19980720"
  W3C_20150513                                     -> "W3C-20150513"
  W3C                                              -> "W3C"
  Watcom_1_0                                       -> "Watcom-1.0"
  Wsuipa                                           -> "Wsuipa"
  WTFPL                                            -> "WTFPL"
  X11                                              -> "X11"
  Xerox                                            -> "Xerox"
  XFree86_1_1                                      -> "XFree86-1.1"
  Xinetd                                           -> "xinetd"
  Xnet                                             -> "Xnet"
  Xpp                                              -> "xpp"
  XSkat                                            -> "XSkat"
  YPL_1_0                                          -> "YPL-1.0"
  YPL_1_1                                          -> "YPL-1.1"
  Zed                                              -> "Zed"
  Zend_2_0                                         -> "Zend-2.0"
  Zimbra_1_3                                       -> "Zimbra-1.3"
  Zimbra_1_4                                       -> "Zimbra-1.4"
  Zlib_acknowledgement                             -> "zlib-acknowledgement"
  Zlib                                             -> "Zlib"
  ZPL_1_1                                          -> "ZPL-1.1"
  ZPL_2_0                                          -> "ZPL-2.0"
  ZPL_2_1                                          -> "ZPL-2.1"

--------------------------------------------------

{- | License name, e.g. @"GNU General Public License v2.0 only"@

-}

licenseName :: SpdxLicenseIdentifier -> String
licenseName = \case

  NullBSD                                        -> "BSD Zero Clause License"
  AAL                                            -> "Attribution Assurance License"
  Abstyles                                       -> "Abstyles License"
  Adobe_2006                                     -> "Adobe Systems Incorporated Source Code License Agreement"
  Adobe_Glyph                                    -> "Adobe Glyph List License"
  ADSL                                           -> "Amazon Digital Services License"
  AFL_1_1                                        -> "Academic Free License v1.1"
  AFL_1_2                                        -> "Academic Free License v1.2"
  AFL_2_0                                        -> "Academic Free License v2.0"
  AFL_2_1                                        -> "Academic Free License v2.1"
  AFL_3_0                                        -> "Academic Free License v3.0"
  Afmparse                                       -> "Afmparse License"
  AGPL_1_0                                       -> "Affero General Public License v1.0"
  AGPL_1_0_only                                  -> "Affero General Public License v1.0 only"
  AGPL_1_0_or_later                              -> "Affero General Public License v1.0 or later"
  AGPL_3_0_only                                  -> "GNU Affero General Public License v3.0 only"
  AGPL_3_0_or_later                              -> "GNU Affero General Public License v3.0 or later"
  Aladdin                                        -> "Aladdin Free Public License"
  AMDPLPA                                        -> "AMD's plpa_map.c License"
  AML                                            -> "Apple MIT License"
  AMPAS                                          -> "Academy of Motion Picture Arts and Sciences BSD"
  ANTLR_PD                                       -> "ANTLR Software Rights Notice"
  Apache_1_0                                     -> "Apache License 1.0"
  Apache_1_1                                     -> "Apache License 1.1"
  Apache_2_0                                     -> "Apache License 2.0"
  APAFML                                         -> "Adobe Postscript AFM License"
  APL_1_0                                        -> "Adaptive Public License 1.0"
  APSL_1_0                                       -> "Apple Public Source License 1.0"
  APSL_1_1                                       -> "Apple Public Source License 1.1"
  APSL_1_2                                       -> "Apple Public Source License 1.2"
  APSL_2_0                                       -> "Apple Public Source License 2.0"
  Artistic_1_0_cl8                               -> "Artistic License 1.0 w/clause 8"
  Artistic_1_0_Perl                              -> "Artistic License 1.0 (Perl)"
  Artistic_1_0                                   -> "Artistic License 1.0"
  Artistic_2_0                                   -> "Artistic License 2.0"
  Bahyph                                         -> "Bahyph License"
  Barr                                           -> "Barr License"
  Beerware                                       -> "Beerware License"
  BitTorrent_1_0                                 -> "BitTorrent Open Source License v1.0"
  BitTorrent_1_1                                 -> "BitTorrent Open Source License v1.1"
  Borceux                                        -> "Borceux license"
  BSD_1_Clause                                   -> "BSD 1-Clause License"
  BSD_2_Clause_FreeBSD                           -> "BSD 2-Clause FreeBSD License"
  BSD_2_Clause_NetBSD                            -> "BSD 2-Clause NetBSD License"
  BSD_2_Clause_Patent                            -> "BSD-2-Clause Plus Patent License"
  BSD_2_Clause                                   -> "BSD 2-Clause \"Simplified\" License"
  BSD_3_Clause_Attribution                       -> "BSD with attribution"
  BSD_3_Clause_Clear                             -> "BSD 3-Clause Clear License"
  BSD_3_Clause_LBNL                              -> "Lawrence Berkeley National Labs BSD variant license"
  BSD_3_Clause_No_Nuclear_License_2014           -> "BSD 3-Clause No Nuclear License 2014"
  BSD_3_Clause_No_Nuclear_License                -> "BSD 3-Clause No Nuclear License"
  BSD_3_Clause_No_Nuclear_Warranty               -> "BSD 3-Clause No Nuclear Warranty"
  BSD_3_Clause                                   -> "BSD 3-Clause \"New\" or \"Revised\" License"
  BSD_4_Clause_UC                                -> "BSD-4-Clause (University of California-Specific)"
  BSD_4_Clause                                   -> "BSD 4-Clause \"Original\" or \"Old\" License"
  BSD_Protection                                 -> "BSD Protection License"
  BSD_Source_Code                                -> "BSD Source Code Attribution"
  BSL_1_0                                        -> "Boost Software License 1.0"
  Bzip2_1_0_5                                    -> "bzip2 and libbzip2 License v1.0.5"
  Bzip2_1_0_6                                    -> "bzip2 and libbzip2 License v1.0.6"
  Caldera                                        -> "Caldera License"
  CATOSL_1_1                                     -> "Computer Associates Trusted Open Source License 1.1"
  CC_BY_1_0                                      -> "Creative Commons Attribution 1.0 Generic"
  CC_BY_2_0                                      -> "Creative Commons Attribution 2.0 Generic"
  CC_BY_2_5                                      -> "Creative Commons Attribution 2.5 Generic"
  CC_BY_3_0                                      -> "Creative Commons Attribution 3.0 Unported"
  CC_BY_4_0                                      -> "Creative Commons Attribution 4.0 International"
  CC_BY_NC_1_0                                   -> "Creative Commons Attribution Non Commercial 1.0 Generic"
  CC_BY_NC_2_0                                   -> "Creative Commons Attribution Non Commercial 2.0 Generic"
  CC_BY_NC_2_5                                   -> "Creative Commons Attribution Non Commercial 2.5 Generic"
  CC_BY_NC_3_0                                   -> "Creative Commons Attribution Non Commercial 3.0 Unported"
  CC_BY_NC_4_0                                   -> "Creative Commons Attribution Non Commercial 4.0 International"
  CC_BY_NC_ND_1_0                                -> "Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic"
  CC_BY_NC_ND_2_0                                -> "Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic"
  CC_BY_NC_ND_2_5                                -> "Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic"
  CC_BY_NC_ND_3_0                                -> "Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported"
  CC_BY_NC_ND_4_0                                -> "Creative Commons Attribution Non Commercial No Derivatives 4.0 International"
  CC_BY_NC_SA_1_0                                -> "Creative Commons Attribution Non Commercial Share Alike 1.0 Generic"
  CC_BY_NC_SA_2_0                                -> "Creative Commons Attribution Non Commercial Share Alike 2.0 Generic"
  CC_BY_NC_SA_2_5                                -> "Creative Commons Attribution Non Commercial Share Alike 2.5 Generic"
  CC_BY_NC_SA_3_0                                -> "Creative Commons Attribution Non Commercial Share Alike 3.0 Unported"
  CC_BY_NC_SA_4_0                                -> "Creative Commons Attribution Non Commercial Share Alike 4.0 International"
  CC_BY_ND_1_0                                   -> "Creative Commons Attribution No Derivatives 1.0 Generic"
  CC_BY_ND_2_0                                   -> "Creative Commons Attribution No Derivatives 2.0 Generic"
  CC_BY_ND_2_5                                   -> "Creative Commons Attribution No Derivatives 2.5 Generic"
  CC_BY_ND_3_0                                   -> "Creative Commons Attribution No Derivatives 3.0 Unported"
  CC_BY_ND_4_0                                   -> "Creative Commons Attribution No Derivatives 4.0 International"
  CC_BY_SA_1_0                                   -> "Creative Commons Attribution Share Alike 1.0 Generic"
  CC_BY_SA_2_0                                   -> "Creative Commons Attribution Share Alike 2.0 Generic"
  CC_BY_SA_2_5                                   -> "Creative Commons Attribution Share Alike 2.5 Generic"
  CC_BY_SA_3_0                                   -> "Creative Commons Attribution Share Alike 3.0 Unported"
  CC_BY_SA_4_0                                   -> "Creative Commons Attribution Share Alike 4.0 International"
  CC0_1_0                                        -> "Creative Commons Zero v1.0 Universal"
  CDDL_1_0                                       -> "Common Development and Distribution License 1.0"
  CDDL_1_1                                       -> "Common Development and Distribution License 1.1"
  CDLA_Permissive_1_0                            -> "Community Data License Agreement Permissive 1.0"
  CDLA_Sharing_1_0                               -> "Community Data License Agreement Sharing 1.0"
  CECILL_1_0                                     -> "CeCILL Free Software License Agreement v1.0"
  CECILL_1_1                                     -> "CeCILL Free Software License Agreement v1.1"
  CECILL_2_0                                     -> "CeCILL Free Software License Agreement v2.0"
  CECILL_2_1                                     -> "CeCILL Free Software License Agreement v2.1"
  CECILL_B                                       -> "CeCILL-B Free Software License Agreement"
  CECILL_C                                       -> "CeCILL-C Free Software License Agreement"
  ClArtistic                                     -> "Clarified Artistic License"
  CNRI_Jython                                    -> "CNRI Jython License"
  CNRI_Python_GPL_Compatible                     -> "CNRI Python Open Source GPL Compatible License Agreement"
  CNRI_Python                                    -> "CNRI Python License"
  Condor_1_1                                     -> "Condor Public License v1.1"
  CPAL_1_0                                       -> "Common Public Attribution License 1.0"
  CPL_1_0                                        -> "Common Public License 1.0"
  CPOL_1_02                                      -> "Code Project Open License 1.02"
  Crossword                                      -> "Crossword License"
  CrystalStacker                                 -> "CrystalStacker License"
  CUA_OPL_1_0                                    -> "CUA Office Public License v1.0"
  Cube                                           -> "Cube License"
  Curl                                           -> "curl License"
  D_FSL_1_0                                      -> "Deutsche Freie Software Lizenz"
  Diffmark                                       -> "diffmark license"
  DOC                                            -> "DOC License"
  Dotseqn                                        -> "Dotseqn License"
  DSDP                                           -> "DSDP License"
  Dvipdfm                                        -> "dvipdfm License"
  ECL_1_0                                        -> "Educational Community License v1.0"
  ECL_2_0                                        -> "Educational Community License v2.0"
  EFL_1_0                                        -> "Eiffel Forum License v1.0"
  EFL_2_0                                        -> "Eiffel Forum License v2.0"
  EGenix                                         -> "eGenix.com Public License 1.1.0"
  Entessa                                        -> "Entessa Public License v1.0"
  EPL_1_0                                        -> "Eclipse Public License 1.0"
  EPL_2_0                                        -> "Eclipse Public License 2.0"
  ErlPL_1_1                                      -> "Erlang Public License v1.1"
  EUDatagrid                                     -> "EU DataGrid Software License"
  EUPL_1_0                                       -> "European Union Public License 1.0"
  EUPL_1_1                                       -> "European Union Public License 1.1"
  EUPL_1_2                                       -> "European Union Public License 1.2"
  Eurosym                                        -> "Eurosym License"
  Fair                                           -> "Fair License"
  Frameworx_1_0                                  -> "Frameworx Open License 1.0"
  FreeImage                                      -> "FreeImage Public License v1.0"
  FSFAP                                          -> "FSF All Permissive License"
  FSFUL                                          -> "FSF Unlimited License"
  FSFULLR                                        -> "FSF Unlimited License (with License Retention)"
  FTL                                            -> "Freetype Project License"
  GFDL_1_1_only                                  -> "GNU Free Documentation License v1.1 only"
  GFDL_1_1_or_later                              -> "GNU Free Documentation License v1.1 or later"
  GFDL_1_2_only                                  -> "GNU Free Documentation License v1.2 only"
  GFDL_1_2_or_later                              -> "GNU Free Documentation License v1.2 or later"
  GFDL_1_3_only                                  -> "GNU Free Documentation License v1.3 only"
  GFDL_1_3_or_later                              -> "GNU Free Documentation License v1.3 or later"
  Giftware                                       -> "Giftware License"
  GL2PS                                          -> "GL2PS License"
  Glide                                          -> "3dfx Glide License"
  Glulxe                                         -> "Glulxe License"
  Gnuplot                                        -> "gnuplot License"
  GPL_1_0_only                                   -> "GNU General Public License v1.0 only"
  GPL_1_0_or_later                               -> "GNU General Public License v1.0 or later"
  GPL_2_0_only                                   -> "GNU General Public License v2.0 only"
  GPL_2_0_or_later                               -> "GNU General Public License v2.0 or later"
  GPL_3_0_only                                   -> "GNU General Public License v3.0 only"
  GPL_3_0_or_later                               -> "GNU General Public License v3.0 or later"
  GSOAP_1_3b                                     -> "gSOAP Public License v1.3b"
  HaskellReport                                  -> "Haskell Language Report License"
  HPND                                           -> "Historical Permission Notice and Disclaimer"
  IBM_pibs                                       -> "IBM PowerPC Initialization and Boot Software"
  ICU                                            -> "ICU License"
  IJG                                            -> "Independent JPEG Group License"
  ImageMagick                                    -> "ImageMagick License"
  IMatix                                         -> "iMatix Standard Function Library Agreement"
  Imlib2                                         -> "Imlib2 License"
  Info_ZIP                                       -> "Info-ZIP License"
  Intel_ACPI                                     -> "Intel ACPI Software License Agreement"
  Intel                                          -> "Intel Open Source License"
  Interbase_1_0                                  -> "Interbase Public License v1.0"
  IPA                                            -> "IPA Font License"
  IPL_1_0                                        -> "IBM Public License v1.0"
  ISC                                            -> "ISC License"
  JasPer_2_0                                     -> "JasPer License"
  JSON                                           -> "JSON License"
  LAL_1_2                                        -> "Licence Art Libre 1.2"
  LAL_1_3                                        -> "Licence Art Libre 1.3"
  Latex2e                                        -> "Latex2e License"
  Leptonica                                      -> "Leptonica License"
  LGPL_2_0_only                                  -> "GNU Library General Public License v2 only"
  LGPL_2_0_or_later                              -> "GNU Library General Public License v2 or later"
  LGPL_2_1_only                                  -> "GNU Lesser General Public License v2.1 only"
  LGPL_2_1_or_later                              -> "GNU Lesser General Public License v2.1 or later"
  LGPL_3_0_only                                  -> "GNU Lesser General Public License v3.0 only"
  LGPL_3_0_or_later                              -> "GNU Lesser General Public License v3.0 or later"
  LGPLLR                                         -> "Lesser General Public License For Linguistic Resources"
  Libpng                                         -> "libpng License"
  Libtiff                                        -> "libtiff License"
  LiLiQ_P_1_1                                    -> "Licence Libre du Qu\233bec \8211 Permissive version 1.1"
  LiLiQ_R_1_1                                    -> "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 version 1.1"
  LiLiQ_Rplus_1_1                                -> "Licence Libre du Qu\233bec \8211 R\233ciprocit\233 forte version 1.1"
  Linux_OpenIB                                   -> "Linux Kernel Variant of OpenIB.org license"
  LPL_1_0                                        -> "Lucent Public License Version 1.0"
  LPL_1_02                                       -> "Lucent Public License v1.02"
  LPPL_1_0                                       -> "LaTeX Project Public License v1.0"
  LPPL_1_1                                       -> "LaTeX Project Public License v1.1"
  LPPL_1_2                                       -> "LaTeX Project Public License v1.2"
  LPPL_1_3a                                      -> "LaTeX Project Public License v1.3a"
  LPPL_1_3c                                      -> "LaTeX Project Public License v1.3c"
  MakeIndex                                      -> "MakeIndex License"
  MirOS                                          -> "MirOS License"
  MIT_0                                          -> "MIT No Attribution"
  MIT_advertising                                -> "Enlightenment License (e16)"
  MIT_CMU                                        -> "CMU License"
  MIT_enna                                       -> "enna License"
  MIT_feh                                        -> "feh License"
  MIT                                            -> "MIT License"
  MITNFA                                         -> "MIT +no-false-attribs license"
  Motosoto                                       -> "Motosoto License"
  Mpich2                                         -> "mpich2 License"
  MPL_1_0                                        -> "Mozilla Public License 1.0"
  MPL_1_1                                        -> "Mozilla Public License 1.1"
  MPL_2_0_no_copyleft_exception                  -> "Mozilla Public License 2.0 (no copyleft exception)"
  MPL_2_0                                        -> "Mozilla Public License 2.0"
  MS_PL                                          -> "Microsoft Public License"
  MS_RL                                          -> "Microsoft Reciprocal License"
  MTLL                                           -> "Matrix Template Library License"
  Multics                                        -> "Multics License"
  Mup                                            -> "Mup License"
  NASA_1_3                                       -> "NASA Open Source Agreement 1.3"
  Naumen                                         -> "Naumen Public License"
  NBPL_1_0                                       -> "Net Boolean Public License v1"
  NCSA                                           -> "University of Illinois/NCSA Open Source License"
  Net_SNMP                                       -> "Net-SNMP License"
  NetCDF                                         -> "NetCDF license"
  Newsletr                                       -> "Newsletr License"
  NGPL                                           -> "Nethack General Public License"
  NLOD_1_0                                       -> "Norwegian Licence for Open Government Data"
  NLPL                                           -> "No Limit Public License"
  Nokia                                          -> "Nokia Open Source License"
  NOSL                                           -> "Netizen Open Source License"
  Noweb                                          -> "Noweb License"
  NPL_1_0                                        -> "Netscape Public License v1.0"
  NPL_1_1                                        -> "Netscape Public License v1.1"
  NPOSL_3_0                                      -> "Non-Profit Open Software License 3.0"
  NRL                                            -> "NRL License"
  NTP                                            -> "NTP License"
  OCCT_PL                                        -> "Open CASCADE Technology Public License"
  OCLC_2_0                                       -> "OCLC Research Public License 2.0"
  ODbL_1_0                                       -> "ODC Open Database License v1.0"
  ODC_By_1_0                                     -> "Open Data Commons Attribution License v1.0"
  OFL_1_0                                        -> "SIL Open Font License 1.0"
  OFL_1_1                                        -> "SIL Open Font License 1.1"
  OGTSL                                          -> "Open Group Test Suite License"
  OLDAP_1_1                                      -> "Open LDAP Public License v1.1"
  OLDAP_1_2                                      -> "Open LDAP Public License v1.2"
  OLDAP_1_3                                      -> "Open LDAP Public License v1.3"
  OLDAP_1_4                                      -> "Open LDAP Public License v1.4"
  OLDAP_2_0_1                                    -> "Open LDAP Public License v2.0.1"
  OLDAP_2_0                                      -> "Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)"
  OLDAP_2_1                                      -> "Open LDAP Public License v2.1"
  OLDAP_2_2_1                                    -> "Open LDAP Public License v2.2.1"
  OLDAP_2_2_2                                    -> "Open LDAP Public License 2.2.2"
  OLDAP_2_2                                      -> "Open LDAP Public License v2.2"
  OLDAP_2_3                                      -> "Open LDAP Public License v2.3"
  OLDAP_2_4                                      -> "Open LDAP Public License v2.4"
  OLDAP_2_5                                      -> "Open LDAP Public License v2.5"
  OLDAP_2_6                                      -> "Open LDAP Public License v2.6"
  OLDAP_2_7                                      -> "Open LDAP Public License v2.7"
  OLDAP_2_8                                      -> "Open LDAP Public License v2.8"
  OML                                            -> "Open Market License"
  OpenSSL                                        -> "OpenSSL License"
  OPL_1_0                                        -> "Open Public License v1.0"
  OSET_PL_2_1                                    -> "OSET Public License version 2.1"
  OSL_1_0                                        -> "Open Software License 1.0"
  OSL_1_1                                        -> "Open Software License 1.1"
  OSL_2_0                                        -> "Open Software License 2.0"
  OSL_2_1                                        -> "Open Software License 2.1"
  OSL_3_0                                        -> "Open Software License 3.0"
  PDDL_1_0                                       -> "ODC Public Domain Dedication & License 1.0"
  PHP_3_0                                        -> "PHP License v3.0"
  PHP_3_01                                       -> "PHP License v3.01"
  Plexus                                         -> "Plexus Classworlds License"
  PostgreSQL                                     -> "PostgreSQL License"
  Psfrag                                         -> "psfrag License"
  Psutils                                        -> "psutils License"
  Python_2_0                                     -> "Python License 2.0"
  Qhull                                          -> "Qhull License"
  QPL_1_0                                        -> "Q Public License 1.0"
  Rdisc                                          -> "Rdisc License"
  RHeCos_1_1                                     -> "Red Hat eCos Public License v1.1"
  RPL_1_1                                        -> "Reciprocal Public License 1.1"
  RPL_1_5                                        -> "Reciprocal Public License 1.5"
  RPSL_1_0                                       -> "RealNetworks Public Source License v1.0"
  RSA_MD                                         -> "RSA Message-Digest License "
  RSCPL                                          -> "Ricoh Source Code Public License"
  Ruby                                           -> "Ruby License"
  SAX_PD                                         -> "Sax Public Domain Notice"
  Saxpath                                        -> "Saxpath License"
  SCEA                                           -> "SCEA Shared Source License"
  Sendmail                                       -> "Sendmail License"
  SGI_B_1_0                                      -> "SGI Free Software License B v1.0"
  SGI_B_1_1                                      -> "SGI Free Software License B v1.1"
  SGI_B_2_0                                      -> "SGI Free Software License B v2.0"
  SimPL_2_0                                      -> "Simple Public License 2.0"
  SISSL_1_2                                      -> "Sun Industry Standards Source License v1.2"
  SISSL                                          -> "Sun Industry Standards Source License v1.1"
  Sleepycat                                      -> "Sleepycat License"
  SMLNJ                                          -> "Standard ML of New Jersey License"
  SMPPL                                          -> "Secure Messaging Protocol Public License"
  SNIA                                           -> "SNIA Public License 1.1"
  Spencer_86                                     -> "Spencer License 86"
  Spencer_94                                     -> "Spencer License 94"
  Spencer_99                                     -> "Spencer License 99"
  SPL_1_0                                        -> "Sun Public License v1.0"
  SugarCRM_1_1_3                                 -> "SugarCRM Public License v1.1.3"
  SWL                                            -> "Scheme Widget Library (SWL) Software License Agreement"
  TCL                                            -> "TCL/TK License"
  TCP_wrappers                                   -> "TCP Wrappers License"
  TMate                                          -> "TMate Open Source License"
  TORQUE_1_1                                     -> "TORQUE v2.5+ Software License v1.1"
  TOSL                                           -> "Trusster Open Source License"
  TU_Berlin_1_0                                  -> "Technische Universitaet Berlin License 1.0"
  TU_Berlin_2_0                                  -> "Technische Universitaet Berlin License 2.0"
  Unicode_DFS_2015                               -> "Unicode License Agreement - Data Files and Software (2015)"
  Unicode_DFS_2016                               -> "Unicode License Agreement - Data Files and Software (2016)"
  Unicode_TOU                                    -> "Unicode Terms of Use"
  Unlicense                                      -> "The Unlicense"
  UPL_1_0                                        -> "Universal Permissive License v1.0"
  Vim                                            -> "Vim License"
  VOSTROM                                        -> "VOSTROM Public License for Open Source"
  VSL_1_0                                        -> "Vovida Software License v1.0"
  W3C_19980720                                   -> "W3C Software Notice and License (1998-07-20)"
  W3C_20150513                                   -> "W3C Software Notice and Document License (2015-05-13)"
  W3C                                            -> "W3C Software Notice and License (2002-12-31)"
  Watcom_1_0                                     -> "Sybase Open Watcom Public License 1.0"
  Wsuipa                                         -> "Wsuipa License"
  WTFPL                                          -> "Do What The F*ck You Want To Public License"
  X11                                            -> "X11 License"
  Xerox                                          -> "Xerox License"
  XFree86_1_1                                    -> "XFree86 License 1.1"
  Xinetd                                         -> "xinetd License"
  Xnet                                           -> "X.Net License"
  Xpp                                            -> "XPP License"
  XSkat                                          -> "XSkat License"
  YPL_1_0                                        -> "Yahoo! Public License v1.0"
  YPL_1_1                                        -> "Yahoo! Public License v1.1"
  Zed                                            -> "Zed License"
  Zend_2_0                                       -> "Zend License v2.0"
  Zimbra_1_3                                     -> "Zimbra Public License v1.3"
  Zimbra_1_4                                     -> "Zimbra Public License v1.4"
  Zlib_acknowledgement                           -> "zlib/libpng License with Acknowledgement"
  Zlib                                           -> "zlib License"
  ZPL_1_1                                        -> "Zope Public License 1.1"
  ZPL_2_0                                        -> "Zope Public License 2.0"
  ZPL_2_1                                        -> "Zope Public License 2.1"

--------------------------------------------------

{- | Whether the license is approved by Open Source Initiative (OSI).

See <https://opensource.org/licenses/alphabetical>.

-}

licenseIsOsiApproved :: SpdxLicenseIdentifier -> Bool

licenseIsOsiApproved = \case
  
  NullBSD                               -> False
  AAL                                   -> True
  Abstyles                              -> False
  Adobe_2006                            -> False
  Adobe_Glyph                           -> False
  ADSL                                  -> False
  AFL_1_1                               -> True
  AFL_1_2                               -> True
  AFL_2_0                               -> True
  AFL_2_1                               -> True
  AFL_3_0                               -> True
  Afmparse                              -> False
  AGPL_1_0                              -> False
  AGPL_1_0_only                         -> False
  AGPL_1_0_or_later                     -> False
  AGPL_3_0_only                         -> True
  AGPL_3_0_or_later                     -> True
  Aladdin                               -> False
  AMDPLPA                               -> False
  AML                                   -> False
  AMPAS                                 -> False
  ANTLR_PD                              -> False
  Apache_1_0                            -> False
  Apache_1_1                            -> True
  Apache_2_0                            -> True
  APAFML                                -> False
  APL_1_0                               -> True
  APSL_1_0                              -> True
  APSL_1_1                              -> True
  APSL_1_2                              -> True
  APSL_2_0                              -> True
  Artistic_1_0_cl8                      -> True
  Artistic_1_0_Perl                     -> True
  Artistic_1_0                          -> True
  Artistic_2_0                          -> True
  Bahyph                                -> False
  Barr                                  -> False
  Beerware                              -> False
  BitTorrent_1_0                        -> False
  BitTorrent_1_1                        -> False
  Borceux                               -> False
  BSD_1_Clause                          -> False
  BSD_2_Clause_FreeBSD                  -> False
  BSD_2_Clause_NetBSD                   -> False
  BSD_2_Clause_Patent                   -> True
  BSD_2_Clause                          -> True
  BSD_3_Clause_Attribution              -> False
  BSD_3_Clause_Clear                    -> False
  BSD_3_Clause_LBNL                     -> False
  BSD_3_Clause_No_Nuclear_License_2014  -> False
  BSD_3_Clause_No_Nuclear_License       -> False
  BSD_3_Clause_No_Nuclear_Warranty      -> False
  BSD_3_Clause                          -> True
  BSD_4_Clause_UC                       -> False
  BSD_4_Clause                          -> False
  BSD_Protection                        -> False
  BSD_Source_Code                       -> False
  BSL_1_0                               -> True
  Bzip2_1_0_5                           -> False
  Bzip2_1_0_6                           -> False
  Caldera                               -> False
  CATOSL_1_1                            -> True
  CC_BY_1_0                             -> False
  CC_BY_2_0                             -> False
  CC_BY_2_5                             -> False
  CC_BY_3_0                             -> False
  CC_BY_4_0                             -> False
  CC_BY_NC_1_0                          -> False
  CC_BY_NC_2_0                          -> False
  CC_BY_NC_2_5                          -> False
  CC_BY_NC_3_0                          -> False
  CC_BY_NC_4_0                          -> False
  CC_BY_NC_ND_1_0                       -> False
  CC_BY_NC_ND_2_0                       -> False
  CC_BY_NC_ND_2_5                       -> False
  CC_BY_NC_ND_3_0                       -> False
  CC_BY_NC_ND_4_0                       -> False
  CC_BY_NC_SA_1_0                       -> False
  CC_BY_NC_SA_2_0                       -> False
  CC_BY_NC_SA_2_5                       -> False
  CC_BY_NC_SA_3_0                       -> False
  CC_BY_NC_SA_4_0                       -> False
  CC_BY_ND_1_0                          -> False
  CC_BY_ND_2_0                          -> False
  CC_BY_ND_2_5                          -> False
  CC_BY_ND_3_0                          -> False
  CC_BY_ND_4_0                          -> False
  CC_BY_SA_1_0                          -> False
  CC_BY_SA_2_0                          -> False
  CC_BY_SA_2_5                          -> False
  CC_BY_SA_3_0                          -> False
  CC_BY_SA_4_0                          -> False
  CC0_1_0                               -> False
  CDDL_1_0                              -> True
  CDDL_1_1                              -> False
  CDLA_Permissive_1_0                   -> False
  CDLA_Sharing_1_0                      -> False
  CECILL_1_0                            -> False
  CECILL_1_1                            -> False
  CECILL_2_0                            -> False
  CECILL_2_1                            -> True
  CECILL_B                              -> False
  CECILL_C                              -> False
  ClArtistic                            -> False
  CNRI_Jython                           -> False
  CNRI_Python_GPL_Compatible            -> False
  CNRI_Python                           -> True
  Condor_1_1                            -> False
  CPAL_1_0                              -> True
  CPL_1_0                               -> True
  CPOL_1_02                             -> False
  Crossword                             -> False
  CrystalStacker                        -> False
  CUA_OPL_1_0                           -> True
  Cube                                  -> False
  Curl                                  -> False
  D_FSL_1_0                             -> False
  Diffmark                              -> False
  DOC                                   -> False
  Dotseqn                               -> False
  DSDP                                  -> False
  Dvipdfm                               -> False
  ECL_1_0                               -> True
  ECL_2_0                               -> True
  EFL_1_0                               -> True
  EFL_2_0                               -> True
  EGenix                                -> False
  Entessa                               -> True
  EPL_1_0                               -> True
  EPL_2_0                               -> True
  ErlPL_1_1                             -> False
  EUDatagrid                            -> True
  EUPL_1_0                              -> False
  EUPL_1_1                              -> True
  EUPL_1_2                              -> True
  Eurosym                               -> False
  Fair                                  -> True
  Frameworx_1_0                         -> True
  FreeImage                             -> False
  FSFAP                                 -> False
  FSFUL                                 -> False
  FSFULLR                               -> False
  FTL                                   -> False
  GFDL_1_1_only                         -> False
  GFDL_1_1_or_later                     -> False
  GFDL_1_2_only                         -> False
  GFDL_1_2_or_later                     -> False
  GFDL_1_3_only                         -> False
  GFDL_1_3_or_later                     -> False
  Giftware                              -> False
  GL2PS                                 -> False
  Glide                                 -> False
  Glulxe                                -> False
  Gnuplot                               -> False
  GPL_1_0_only                          -> False
  GPL_1_0_or_later                      -> False
  GPL_2_0_only                          -> True
  GPL_2_0_or_later                      -> True
  GPL_3_0_only                          -> True
  GPL_3_0_or_later                      -> True
  GSOAP_1_3b                            -> False
  HaskellReport                         -> False
  HPND                                  -> True
  IBM_pibs                              -> False
  ICU                                   -> False
  IJG                                   -> False
  ImageMagick                           -> False
  IMatix                                -> False
  Imlib2                                -> False
  Info_ZIP                              -> False
  Intel_ACPI                            -> False
  Intel                                 -> True
  Interbase_1_0                         -> False
  IPA                                   -> True
  IPL_1_0                               -> True
  ISC                                   -> True
  JasPer_2_0                            -> False
  JSON                                  -> False
  LAL_1_2                               -> False
  LAL_1_3                               -> False
  Latex2e                               -> False
  Leptonica                             -> False
  LGPL_2_0_only                         -> True
  LGPL_2_0_or_later                     -> True
  LGPL_2_1_only                         -> True
  LGPL_2_1_or_later                     -> True
  LGPL_3_0_only                         -> True
  LGPL_3_0_or_later                     -> True
  LGPLLR                                -> False
  Libpng                                -> False
  Libtiff                               -> False
  LiLiQ_P_1_1                           -> True
  LiLiQ_R_1_1                           -> True
  LiLiQ_Rplus_1_1                       -> True
  Linux_OpenIB                          -> False
  LPL_1_0                               -> True
  LPL_1_02                              -> True
  LPPL_1_0                              -> False
  LPPL_1_1                              -> False
  LPPL_1_2                              -> False
  LPPL_1_3a                             -> False
  LPPL_1_3c                             -> True
  MakeIndex                             -> False
  MirOS                                 -> True
  MIT_0                                 -> True
  MIT_advertising                       -> False
  MIT_CMU                               -> False
  MIT_enna                              -> False
  MIT_feh                               -> False
  MIT                                   -> True
  MITNFA                                -> False
  Motosoto                              -> True
  Mpich2                                -> False
  MPL_1_0                               -> True
  MPL_1_1                               -> True
  MPL_2_0_no_copyleft_exception         -> True
  MPL_2_0                               -> True
  MS_PL                                 -> True
  MS_RL                                 -> True
  MTLL                                  -> False
  Multics                               -> True
  Mup                                   -> False
  NASA_1_3                              -> True
  Naumen                                -> True
  NBPL_1_0                              -> False
  NCSA                                  -> True
  Net_SNMP                              -> False
  NetCDF                                -> False
  Newsletr                              -> False
  NGPL                                  -> True
  NLOD_1_0                              -> False
  NLPL                                  -> False
  Nokia                                 -> True
  NOSL                                  -> False
  Noweb                                 -> False
  NPL_1_0                               -> False
  NPL_1_1                               -> False
  NPOSL_3_0                             -> True
  NRL                                   -> False
  NTP                                   -> True
  OCCT_PL                               -> False
  OCLC_2_0                              -> True
  ODbL_1_0                              -> False
  ODC_By_1_0                            -> False
  OFL_1_0                               -> False
  OFL_1_1                               -> True
  OGTSL                                 -> True
  OLDAP_1_1                             -> False
  OLDAP_1_2                             -> False
  OLDAP_1_3                             -> False
  OLDAP_1_4                             -> False
  OLDAP_2_0_1                           -> False
  OLDAP_2_0                             -> False
  OLDAP_2_1                             -> False
  OLDAP_2_2_1                           -> False
  OLDAP_2_2_2                           -> False
  OLDAP_2_2                             -> False
  OLDAP_2_3                             -> False
  OLDAP_2_4                             -> False
  OLDAP_2_5                             -> False
  OLDAP_2_6                             -> False
  OLDAP_2_7                             -> False
  OLDAP_2_8                             -> False
  OML                                   -> False
  OpenSSL                               -> False
  OPL_1_0                               -> False
  OSET_PL_2_1                           -> True
  OSL_1_0                               -> True
  OSL_1_1                               -> False
  OSL_2_0                               -> True
  OSL_2_1                               -> True
  OSL_3_0                               -> True
  PDDL_1_0                              -> False
  PHP_3_0                               -> True
  PHP_3_01                              -> False
  Plexus                                -> False
  PostgreSQL                            -> True
  Psfrag                                -> False
  Psutils                               -> False
  Python_2_0                            -> True
  Qhull                                 -> False
  QPL_1_0                               -> True
  Rdisc                                 -> False
  RHeCos_1_1                            -> False
  RPL_1_1                               -> True
  RPL_1_5                               -> True
  RPSL_1_0                              -> True
  RSA_MD                                -> False
  RSCPL                                 -> True
  Ruby                                  -> False
  SAX_PD                                -> False
  Saxpath                               -> False
  SCEA                                  -> False
  Sendmail                              -> False
  SGI_B_1_0                             -> False
  SGI_B_1_1                             -> False
  SGI_B_2_0                             -> False
  SimPL_2_0                             -> True
  SISSL_1_2                             -> False
  SISSL                                 -> True
  Sleepycat                             -> True
  SMLNJ                                 -> False
  SMPPL                                 -> False
  SNIA                                  -> False
  Spencer_86                            -> False
  Spencer_94                            -> False
  Spencer_99                            -> False
  SPL_1_0                               -> True
  SugarCRM_1_1_3                        -> False
  SWL                                   -> False
  TCL                                   -> False
  TCP_wrappers                          -> False
  TMate                                 -> False
  TORQUE_1_1                            -> False
  TOSL                                  -> False
  TU_Berlin_1_0                         -> False
  TU_Berlin_2_0                         -> False
  Unicode_DFS_2015                      -> False
  Unicode_DFS_2016                      -> False
  Unicode_TOU                           -> False
  Unlicense                             -> False
  UPL_1_0                               -> True
  Vim                                   -> False
  VOSTROM                               -> False
  VSL_1_0                               -> True
  W3C_19980720                          -> False
  W3C_20150513                          -> False
  W3C                                   -> True
  Watcom_1_0                            -> True
  Wsuipa                                -> False
  WTFPL                                 -> False
  X11                                   -> False
  Xerox                                 -> False
  XFree86_1_1                           -> False
  Xinetd                                -> False
  Xnet                                  -> True
  Xpp                                   -> False
  XSkat                                 -> False
  YPL_1_0                               -> False
  YPL_1_1                               -> False
  Zed                                   -> False
  Zend_2_0                              -> False
  Zimbra_1_3                            -> False
  Zimbra_1_4                            -> False
  Zlib_acknowledgement                  -> False
  Zlib                                  -> True
  ZPL_1_1                               -> False
  ZPL_2_0                               -> True
  ZPL_2_1                               -> False

--------------------------------------------------

{- | Whether the license is compatible with "Free, Libre, and Open" principles.

See <TODO>.

-}

licenseIsFlossCompatible :: SpdxLicenseIdentifier -> Bool

licenseIsFlossCompatible = \case

  GPL_3_0_only      -> True
  GPL_3_0_or_later  -> True

  AGPL_3_0_only     -> True
  AGPL_3_0_or_later -> True

  LGPL_3_0_only     -> True
  LGPL_3_0_or_later -> True

  _                 -> False --TODO

--------------------------------------------------
--------------------------------------------------