/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash_fct.c                                                      *
 * Descr.: hash function                                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


/*
 * These hash functions are mainly based on MurmurHash3.
 *
 * MurmurHash3 was written by Austin Appleby, and is placed in the public
 * domain. The author hereby disclaims copyright to this source code.
 *
 * The original code can be found here (see function MurmurHash3_x86_32)
 * http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp
 *
 * However, the original code does not address (efficiently) several issues:
 *   data alignment
 *   endianess
 *   need for homegeneous hash (independently of the plateform 32/64)
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>		/* some systems need this */

#if 1
#include "engine_pl.h"
#else  /* to test appart from gprolog (only need HAVE_xxx macros defined by autoconf) */
#include "gp_config.h"
#endif

#include "hash_fct.h"

#ifdef HAVE_ENDIAN_H
#include <endian.h>
#endif

#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif

#ifdef HAVE_BYTESWAP_H
#include <byteswap.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define HASH_FCT_INITIAL_SEED      1688943522 /* any number is OK */


/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#if defined(_MSC_VER)

#define FORCE_INLINE    __forceinline

#define ROTL32(x, y)     _rotl(x, y)
#define ROTL64(x, y)     _rotl64(x, y)

#else /* !defined(_MSC_VER) */

#if defined(__GNUC__)
#define FORCE_INLINE __inline __attribute__ ((always_inline))
#else
#define FORCE_INLINE
#endif

static FORCE_INLINE uint32_t
ROTL32(uint32_t x, int8_t r)
{
  return (x << r) | (x >> (32 - r));
}

static FORCE_INLINE uint64_t
ROTL64(uint64_t x, int8_t r)
{
  return (x << r) | (x >> (64 - r));
}

#endif /* !defined(_MSC_VER) */


/*
 * Hash_Initialize (mainly set initial seed for MurmurHash3)
 */
static FORCE_INLINE uint32_t
Hash_Initialize(void)
{
  return HASH_FCT_INITIAL_SEED;
}


/*
 * Hash_Block (hash a 32 bits block)
 * this is the body the the main MurmurHash3 loop
 */

static FORCE_INLINE uint32_t
Hash_Block(uint32_t k1, uint32_t h1) /* h1 is the current hash */
{
  const uint32_t c1 = 0xcc9e2d51;
  const uint32_t c2 = 0x1b873593;

  k1 *= c1;
  k1 = ROTL32(k1, 15);
  k1 *= c2;

  h1 ^= k1;
  h1 = ROTL32(h1, 13);
  h1 = h1 * 5 + 0xe6546b64;

  return h1;
}


/*
 * Hash_Finalize
 * (corresponds to fmix of MurmurHash3 + the h ^= len done outside)
 *
 * fmix - force all bits of a hash block to avalanche
 */

static FORCE_INLINE uint32_t
Hash_Finalize(uint32_t h, int len)
{
  h ^= len;

  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;

  return h;
}


/*
 * Hash_Buffer (this is MurmurHash3_x86_32)
 * Hash_Buffer_Aligned
 * Hash_Buffer_Unaligned
 */

#define HASH_BUFFER_FCT  Hash_Buffer_Aligned
#include "hash_fct1.c"

#undef HASH_BUFFER_FCT
#define HASH_BUFFER_FCT  Hash_Buffer_Unaligned
#define USE_32BITS_ALIGNMENT
#include "hash_fct1.c"

uint32_t
Hash_Buffer(const void *key, int len, uint32_t seed)
{
  if (((uintptr_t) key & 3) == 0)	/* is it aligned on blocks (uint32_t) ? */
    return Hash_Buffer_Aligned(key, len, seed);

  return Hash_Buffer_Unaligned(key, len, seed);    
}




/*-------------------------------------------------------------------------*
 * The above functions (based on MurmurHash3) are not directly used by the *
 * Prolog engine. Instead they provide (none have to handle endianess):    *
 *                                                                         *
 *   Hash_Initialize                                                       *
 *   Hash_Finalize                                                         *
 *   Hash_Buffer (which in turn needs Hash_Buffer_Aligned / _Unaligned)    *
 *   Hash_Block                                                            *
 *                                                                         *
 * This decomposition is provided for incremental hashing.                 *
 *                                                                         *
 * To change the hashing method (e.g. xxhash, FNV1A_Yorikke,...) it is     *
 * only necessary to provide an implementation for the above functions.    *
 *                                                                         *
 * These functions are used to implement the API used by the Prolog engine *
 * composed of the next functions. They could be in another file (but here *
 * inlining can occur).                                                    *
 * They handle buffers (strings) and numbers separately (for endianness).  *
 * They provide incremental hashing.                                       *
 *-------------------------------------------------------------------------*/

#if !HAVE_DECL_BSWAP_32
uint32_t 
bswap_32(uint32_t x)
{
  x = ((x & 0xff) << 24) | ((x & 0xff00) << 8) | ((x >> 8) & 0xff00) | (x >> 24);
  return x;
}

#endif	/* !HAVE_DECL_BSWAP_32 */


#if !HAVE_DECL_HTOLE32
#ifdef htole32			/* should be useless but in case of... */
#undef htole32
#endif
static uint32_t 
htole32(uint32_t x)
{
#ifdef WORDS_BIGENDIAN

  return bswap_32(x);

#else

  return x;

#endif
}

#endif	/* !HAVE_DECL_HTOLE32 */




/*-------------------------------------------------------------------------*
 * PL_HASH_BUFFER                                                          *
 *                                                                         *
 * This is the same as:                                                    *
 *                                                                         *
 *   Pl_Hash_Incr_Init(hi);                                                *
 *   Pl_Hash_Incr_Init(hi, data, len);                                     *
 *   Pl_Hash_Incr_Term(hi);                                                *
 *-------------------------------------------------------------------------*/
uint32_t
Pl_Hash_Buffer(const void *data, int len)
{
  uint32_t hash = Hash_Initialize();
  hash = Hash_Buffer(data, len, hash);
  hash = Hash_Finalize(hash, len);
  return hash;
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_INIT                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Hash_Incr_Init(HashIncrInfo *hi)
{
  hi->len = 0;
  hi->hash = Hash_Initialize();
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_BUFFER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Hash_Incr_Buffer(HashIncrInfo *hi, const void *data, int len)
{
  hi->len += len;
  hi->hash = Hash_Buffer(data, len, hi->hash);
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_INT32                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Hash_Incr_Int32(HashIncrInfo *hi, uint32_t x)
{
  hi->len += 4;
  hi->hash = Hash_Block(htole32(x), hi->hash);
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_INT64                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Hash_Incr_Int64(HashIncrInfo *hi, uint64_t x)
{
  Pl_Hash_Incr_Int32(hi, (uint32_t) x);
  Pl_Hash_Incr_Int32(hi, x >> 32);
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_DOUBLE                                                     *
 *                                                                         *
 * NB: hashing float numbers is definitively not a good idea.              *
 *-------------------------------------------------------------------------*/
void
Pl_Hash_Incr_Double(HashIncrInfo *hi, double x)

#ifndef DBL_MAX_EXP
#define DBL_MAX_EXP 1024
#endif

#ifndef DBL_MANT_DIG
#define DBL_MANT_DIG 53
#endif

#ifndef UINT64_MAX
#define UINT64_MAX ((uint64_t) -1)
#endif

/*
 * IEEE 754: x is represented as: man * 2^exp  with 0.5 <= |man| < 1
 * A double is encoded on 64 bits as follows:
 * man is on 53 bits (52 encoded since 1 bit is implicitely set to 1)
 * exp is on 11 bits (signed)  (can encode in radix 10 -307..308)
 * +1 bit for the overall sign
 * A double can represent 1.0, 
 * next value > 1 is 1.0000000000000002, next is 1.0000000000000004
 */


/* multiplier to use maximum bits of mantissa (according to target type)
 * To be exact should be (UINT64_MAX + 1.0) (something like 1<<64
 * Does not for for MSVC: max is 1 << 63 because it exceeds signed int 64 bits */

#if 0
#define DEBUG
#endif

#define MULT ((uint64_t) 1 << 63) 
/*#define MULT ((uint64_t) 1 << 32)*/	/* test with less decimals */
{
  int exp;
  uint64_t man64;
  uint64_t rest;

#ifdef DEBUG
  printf("dbl: %.15g\n", x);
#endif
  x = frexp(x, &exp);  		/* x is now the mantissa */
#ifdef DEBUG
  printf("man: %.15f\n", x);
  printf("exp: %d\n", exp);
#endif

  /* Mantissa between 0 and 1 (other possibility: x = (2 * fabs(x) - 1) slower) */
  if (x < 0.0)
    x = -(x + 0.5);

  /* Mantissa on maximum bits for the target type (here 64 bits) */
  x *= MULT; 

#ifdef DEBUG
  printf("man scaled : %.2f\n", x);
#endif

  /* NB: signed cast (int64_t) is much faster than unsigned (uint64_t)
   * Anyway: here we only want bits 
   * Other possibility: use lrint() math function but is missing under MSVC */
  man64 = (int64_t) x; 

#ifdef DEBUG
  printf("man integer: %"  FMT64_d "\n", man64);
#endif

#if 0				/* hash both mantissa and exponent */

  Pl_Hash_Incr_Int64(hi, man64);
  Pl_Hash_Incr_Int32(hi, exp);

#else  /* This one is more suited to take less decimals into account (decrease MULT) */

  /* missing */
#if 0 /* only usefull if mantissa is converted to a smaller type (e.g.int32), else = 0 */
  rest = (x - (double) man64) * MULT;
#else
  rest = 0;
#endif

#ifdef DEBUG
  printf("rest: %" FMT64_x "\n",  rest);
  printf("div: %" FMT64_d " = 0x%" FMT64_x "\n",  (MULT / DBL_MAX_EXP), (MULT / DBL_MAX_EXP));
#endif

  /* take into account exp */
  man64 = man64 + rest + (MULT / DBL_MAX_EXP) * exp;

#ifdef DEBUG
  printf("final value: %"  FMT64_d "\n", man64);
#endif

  Pl_Hash_Incr_Int64(hi, man64);
#endif
}




/*-------------------------------------------------------------------------*
 * PL_HASH_INCR_TERM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
uint32_t
Pl_Hash_Incr_Term(HashIncrInfo *hi)
{
  hi->hash = Hash_Finalize(hi->hash, hi->len);
  return hi->hash;
}
