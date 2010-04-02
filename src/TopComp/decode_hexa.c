/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : decode_hexa.c                                                   *
 * Descr.: hexadecimal decoding                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

/* This file is included by top_comp.c and hexfilter.c */




#define AUX_STR                    "_$aux"

#define Hex_Num(c)  (isdigit(c) ? (c)-'0' : isupper(c) ? (c)-'A'+10 : (c)-'a'+10)




/*-------------------------------------------------------------------------*
 * DECODE_HEXA                                                             *
 *                                                                         *
 * str       : the line to decode                                          *
 * format    : printf format to emit decoded strings (or NULL)             *
 * strict    : 1: only predicate indicators, 0: also predicate names       *
 * decode_aux: 0: no, 1: as its father, 2: as father + auxiliary number    *
 *-------------------------------------------------------------------------*/
char *
Decode_Hexa(char *str, char *format, int strict, int decode_aux)
{
  char *src, *dst;
  char *q, *s;
  int arity;
  int n;
  static char buff[2048];
  static char result[4096];

  if (format == NULL)
    format = "%s";

  dst = result;
  for (src = str; *src; src++)
    {
      if ((*src == '_' && src[1] == 'X') || *src == 'X')
	{
	  if (src > str && isalnum(src[-1]))
	    goto normal_char;

	  s = buff;
	  for (q = (*src == '_') ? src + 2 : src + 1; isxdigit(*q); q += 2)
	    {
	      if (!isxdigit(q[1]))
		goto normal_char;

	      *s++ = Hex_Num(*q) * 16 + Hex_Num(q[1]);
	    }

	  *s = '\0';
	  if (*q == '_')
	    {
	      if (!isdigit(*++q))
		goto normal_char;

	      arity = strtol(q, &q, 10);

	      if (isalnum(*q))
		goto normal_char;

	      sprintf(s, "/%d", arity);
	    }
	  else if (strict)
	    goto normal_char;

	  if (decode_aux == 0 || *buff != '$'
	      || (src = strstr(buff, AUX_STR)) == NULL)
	    goto normal_pred;

	  n = strtol(src + sizeof(AUX_STR) - 1, &s, 10);
	  if (strict && *s != '/')
	    goto normal_pred;

	  s = buff + 1;		/* skip leading $ */
	  if (decode_aux == 2)
	    sprintf(src, "-aux-%d", n);
	  else
	    *src = '\0';
	  goto disp_pred;

	normal_pred:
	  s = buff;

	disp_pred:
	  sprintf(dst, format, s);
	  while (*dst)
	    dst++;

	  src = q;
	  if (*src == '\0')
	    break;
	}

    normal_char:
      *dst++ = *src;
    }

  *dst = '\0';
  return result;
}
