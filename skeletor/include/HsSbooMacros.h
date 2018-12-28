/* Macros for different platforms, compiler extensions, and introduced features.
 * 
 * 
 */

/************************************************/

#if !defined(HS_BOO_MACROS_H)

#define HS_SBOO_MACROS_H

/******************************************************************************/

#ifndef __GLASGOW_HASKELL__
#define __GLASGOW_HASKELL__ 000
/* i.e. « v0.0.0 » a.k.a. « False » */
#endif

/************************************************/

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(x,y,z1,z2) 0
/* i.e. « const False » */
#endif

/************************************************/

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
/* i.e. « const False » */
#endif

/* NOTE `ghc-7.10` introduced `MIN_VERSION_GLASGOW_HASKELL`. */

/************************************************/

#define IS_COMPILER_ghc   __GLASGOW_HASKELL__
#define IS_COMPILER_ghcjs ghcjs_HOST_OS

/************************************************/

#define IS_OS_LINUX   defined(linux_HOST_OS)
#define IS_OS_WINDOWS defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS) 
#define IS_OS_APPLE   defined(darwin_HOST_OS)

#define IS_OS_POSIX   defined(linux_HOST_OS) || defined(darwin_HOST_OS) || defined(aix_HOST_OS) || defined(hpux_HOST_OS) || defined(irix_HOST_OS) || defined(solaris_HOST_OS) || defined(freebsd_HOST_OS) || defined(opennbsd_HOST_OS) || defined(netbsd_HOST_OS) || defined(ios_HOST_OS) || defined(android_HOST_OS) || defined(hurd_HOST_OS) || defined(halvm_HOST_OS)

// See « https://en.wikipedia.org/wiki/POSIX#POSIX-oriented_operating_systems »:
//
// POSIX-certified: AIX, HPUX, IRIX, Solaris.
// POSIX-compliant (mostly): Linux, Darwin (OSX), FreeBSD, OpenBSD, NetBSD, IOS, Android, Hurd, HaLVM.

// « Cabal-2.4.1.0:Distribution.System.OS »
//
// See « https://www.haskell.org/cabal/release/latest/doc/API/Cabal/Distribution-System.html#t:OS »

/************************************************/

#define IS_ARCH_64_BIT_INTEL i386_HOST_ARCH
#define IS_ARCH_32_BIT_INTEL x86_64_HOST_ARCH

/******************************************************************************/

#define HAS_BASE_Contravariant                    MIN_VERSION_base(4,12,0)
/* module Data.Functor.Contravariant */

/******************************************************************************/

#define HAS_EXTENSION_DerivingStrategies      MIN_VERSION_GLASGOW_HASKELL(8,4,1,0)

#define HAS_EXTENSION_HexFloatLiterals        MIN_VERSION_GLASGOW_HASKELL(8,4,1,0) 

/************************************************/

#define HAS_EXTENSION_DerivingVia             MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)

#define HAS_EXTENSION_BlockArguments          MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 
#define HAS_EXTENSION_NumericUnderscores      MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 
#define HAS_EXTENSION_StarIsType              MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 

/******************************************************************************/

#define HAS_PRAGMA_COMPLETE                   MIN_VERSION_GLASGOW_HASKELL(8,2,1,0) 
/* e.g. « {-# COMPLETE LeftChoice, RightChoice #-} » */

/******************************************************************************/
#endif