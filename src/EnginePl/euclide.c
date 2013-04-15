/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : development tool                                                *
 * File  : euclide.c                                                       *
 * Descr.: compute inverse                                                 *
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


#include <stdio.h>
#include <string.h>


/*-------------------------------------------------------------------------*
 * This program takes 2 numbers N1 and N2 (relatively prime) and computes  *
 * N3 s.t. N1*N3 % N2 = 1.                                                 *
 * It uses the Euclide algorithm, by successive divisions of n1 by n2 until*
 * n2==0 maintaining u1 v1 and u2 v2 s.t., at each step, we have           *
 *                                                                         *
 *    n1=u1*N1 + v1*N2                                                     *
 *    n2=u2*N1 + v2*N2                                                     *
 *                                                                         *
 * At the end, n2=0, n1 is the gdb of N1 and N2 and u1 is the wanted nb.   *
 *                                                                         *
 * This program is used to compute the constant INV_RADIX_MOD_MAX_ATOM  in *
 * atom.c. Indeed, it is the result of euclide RADIX MAX_ATOM, ie. actually*
 * RADIX=67 and MAX_ATOM=65536 thus INV_RADIX_MOD_MAX_ATOM=19563.          *
 * If MAX_ATOM is changed, INV_RADIX_MOD_MAX_ATOM must be changed too.     *
 *-------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------*
 * EUCLIDE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
unsigned
Euclide(unsigned N1, unsigned N2)
{
  unsigned q, r;
  int u2, v2;
  int u1, v1;
  int t;
  unsigned n1 = N1;
  unsigned n2 = N2;

  /* v1 and v2 are not used actually */
  u1 = 1;
  v1 = 0;
  u2 = 0;
  v2 = 1;


  while (n2)
    {
      q = n1 / n2;		/* gcd part */
      r = n1 % n2;
      n1 = n2;
      n2 = r;

      t = u1 - q * u2;		/* maintaining u1 and u2 */
      u1 = u2;
      u2 = t;

      t = v1 - q * v2;		/* maintaining v1 and v2 */
      v1 = v2;
      v2 = t;
    }

  if (u1 < 0)
    u1 += N2;

  return u1;
}




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  unsigned n1;
  unsigned n2;
  unsigned inv_n1_mod_n2;

  if (argc != 3)
    {
      printf("Usage: %s nb_to_invert modulo\n", argv[0]);
      printf("       nb_to_invert and modulo must be relatively prime\n");
      return 1;
    }

  n1 = atoi(argv[1]);
  n2 = atoi(argv[2]);

  inv_n1_mod_n2 = Euclide(n1, n2);

  printf("Res: %-10u i.e. (1/%u) %% %u = %u\n",
	 inv_n1_mod_n2, n1, n2, inv_n1_mod_n2);
  printf("     %-10s i.e. (%u*%u) %% %u = 1\n", "", n1, inv_n1_mod_n2, n2);

  if ((unsigned) ((n1 * inv_n1_mod_n2) % n2) != 1)
    printf("\n*** CHECK ERROR %u instead of 1\n",
	   (n1 * inv_n1_mod_n2) % n2);

  return 0;
}
