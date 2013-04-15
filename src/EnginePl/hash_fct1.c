/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash_fct1.c                                                     *
 * Descr.: hash function (part)                                            *
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
 * This one is mainly MurmurHash3_x86_32 without the finalization part.
 *
 * NB: the memory alignment of the key should not affect the hash result.
 * So it is NOT possible to "consume" 1,2 or 3 bytes at start to ensure
 * next block reads are aligned.
 *
 * This file is included twice to generate 2 versions (aligned and unaligned)
 */
static uint32_t
HASH_BUFFER_FCT(const void *key, int len, uint32_t seed)
{
  uint8_t *data = (uint8_t *) key;
  const uint8_t *limit_block = data + len - 4;	/* -4 for 32-bit block processing */

  const uint32_t c1 = 0xcc9e2d51;
  const uint32_t c2 = 0x1b873593;

  uint32_t h1 = seed;
  uint32_t k1;

  /* body */

  while(data <= limit_block)
    {
#ifdef USE_32BITS_ALIGNMENT
      memcpy(&k1, data, 4);
#else
      k1 = *(uint32_t *) data;
#endif
      data += 4;
      h1 = Hash_Block(k1, h1);
    }
    
  /* tail */
  
  k1 = 0;

  switch (len & 3)
    {
    case 3:
      k1 ^= data[2] << 16;
    case 2:
      k1 ^= data[1] << 8;
    case 1:
      k1 ^= data[0];
      k1 *= c1;
      k1 = ROTL32(k1, 15);
      k1 *= c2;
      h1 ^= k1;
    }

  return h1;
}
