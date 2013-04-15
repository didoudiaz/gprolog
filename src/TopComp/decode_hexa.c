/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : decode_hexa.c                                                   *
 * Descr.: hexadecimal decoding                                            *
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


#include <string.h>
#include <ctype.h>


/* This file is included by top_comp.c, hexfilter.c and wam2ma.c */


/* Name Mangling (decoration). 
 *
 * A [MODULE:]PRED/N will be encoded as
 *
 * XK_[E(MODULE)__]E(PRED)__aN
 *
 * K: an ASCII digit '0'-'5' storing coding information about MODULE and PRED
 *    associated integer (value on 3 bits):
 *     0 (00 0) : no module present        PRED is not encoded
 *     1 (00 1) : no module present        PRED is encoded
 *     2 (01 0) : MODULE is not encoded    PRED is not encoded
 *     3 (01 1) : MODULE is not encoded    PRED is encoded
 *     4 (10 0) : MODULE is encoded        PRED is not encoded
 *     5 (10 1) : MODULE is encoded        PRED is encoded
 *
 * Where E(STR) =
 *  - STR (not encoded) if STR only contains letters, digits or _
 *    but does not contain the substring __ and does not begin/end with _
 *    regexp: [a-zA-Z0-9] ([-]?[a-zA-Z0-9])*
 *  - an hexa representation (encoded) of each character of the string
 *
 * NB: if this mangling schema is modified also modify macro 
 *     Prolog_Prototype in engine.h
 */


static char pl_escape_symbol[] = "abfnrtv";
static char pl_escape_char[] = "\a\b\f\n\r\t\v";

#define AUX_STR                    "_$aux"



/* for encoding: we do not use isalpha to avoid problems with localization
 * an accent-letter is not a valid C/asm identifier
 */

#define Letter(c)        (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z'))

#define Letter_Digit(c)  (Letter(c) || isdigit(c))
#define Hexa_Digit(c)    (isdigit(c) || ((c) >= 'A' && (c) <= 'F'))

#define Hex_Num(c)       (isdigit(c) ? (c) - '0' : (c) - 'A' + 10)


static int String_Needs_Encoding(char *str);
static char *Encode_String(char *str, char *buff);


static char *Copy_Not_Encoded_String(char *str, char *buff);
static char *Decode_String(char *str, char *buff);
static void Quote_If_Needed(char *str);
static char *Parse_Atom(char *src, char *dst);


/*-------------------------------------------------------------------------*
 * ENCODE_HEXA                                                             *
 *                                                                         *
 * module    : the module to encode (NULL or '\0' if no module qualif)     *
 * pred      : the predicate functor                                       *
 * arity     : the arity (or -1 if no arity present)                       *
 * str       : the resulting encoded string                                *
 *                                                                         *
 * Returns the next position in str                                        *
 *-------------------------------------------------------------------------*/
char *
Encode_Hexa(char *module, char *pred, int arity, char *str)
{
  int module_encode = (module == NULL || *module == '\0') ? 0 : String_Needs_Encoding(module) + 1;
  int pred_encode = String_Needs_Encoding(pred);


  *str++ = 'X';
  *str++ = '0' + ((module_encode << 1) | pred_encode);
  *str++ = '_';
  if (module_encode == 1)
    str += sprintf(str, "%s__", module);
  else if (module_encode == 2)
    {
      str = Encode_String(module, str);
      *str++ = '_';
      *str++ = '_';
    }

  if (pred_encode == 0)
    str += sprintf(str, "%s", pred);
  else
    str = Encode_String(pred, str);

  if (arity >= 0)
    str += sprintf(str, "__a%d", arity);

  return str;
}



/*-------------------------------------------------------------------------*
 * ENCODE_HEXA_LINE                                                        *
 *                                                                         *
 * str       : the line to encode                                          *
 * format    : printf format to emit encoded strings (or NULL)             *
 * strict    : 1: only predicate indicators, 0: also predicate names       *
 *                                                                         *
 * Returns an encoded line (static buffer)                                 *
 *-------------------------------------------------------------------------*/
char *
Encode_Hexa_Line(char *str, char *format, int strict)
{
  static char result[4096];
  static char buff[4096];
  char *module, *pred;
  char *src, *dst;
  char *p, *q, *free_in_buff;
  int arity;

  if (format == NULL)
    format = "%s";

  src = str;
  dst = result;
  for(;;)
    {
      while(isspace(*src))
	*dst++ = *src++;

      if (*src == '\0')
	break;


      p = Parse_Atom(src, buff);
      if (p == NULL)
	{
	  *dst++ = *src++;
	  continue;
	}

      if (*p == ':') 	/* module qualif found */
	{
	  module = buff;
	  pred = buff + strlen(module) + 1;
	  q = p + 1;
	  p = Parse_Atom(q, pred);
	  if (p == NULL)
	    {
	      while(src < q)
		*dst++ = *src++;
	      continue;
	    }
	}
      else
	{
	  module = NULL;
	  pred = buff;
	}

      arity = strtol(p + 1, &q, 10); /* we suppose *p = '/' */

      if (*pred == '\0' || (*p != '/' && strict) ||
	  (*p == '/' && (arity < 0 || arity > 1024 || isalnum(*q) || *q == '_')))
	{
	  while(src < q)
	    *dst++ = *src++;
	  continue;
	}

      if (*p != '/')
	{
	  arity = -1;
	  src = p;
	}
      else
	src = q;

      free_in_buff = pred + strlen(pred) + 1;

      Encode_Hexa(module, pred, arity, free_in_buff);

      dst += sprintf(dst, format, free_in_buff);
    }

  *dst++ = '\0';
  return result;
}




/*-------------------------------------------------------------------------*
 * STRING_NEEDS_ENCODING                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
String_Needs_Encoding(char *str)
{
  if (*str == '\0' || !Letter_Digit(*str))
    return 1;

  while(*++str != '\0')
    {
      if (*str == '_')
	{
	  if (str[-1] == '_' || str[1] == '\0')
	    return 1;
	}
      else
	{
	  if (!Letter_Digit(*str))
	    return 1;
	}
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * ENCODE_STRING                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Encode_String(char *str, char *buff)
{
  while(*str)
    {
      sprintf(buff, "%02X", (unsigned) (unsigned char) *str);
      str++;
      buff += 2;
    }

  return buff;
}








/*-------------------------------------------------------------------------*
 * DECODE_HEXA                                                             *
 *                                                                         *
 * str       : the string to decode                                        *
 * strict    : 1: only predicate indicators, 0: also predicate names       *
 * quote     : 0: no quotes, 1: surround module|pred with quotes if needed *
 * decode_aux: 0: no, 1: as its father, 2: as father + auxiliary number    *
 * module    : the resulting decoded module ('\0' if no module qualif)     *
 * pred      : the resulting predicate functor                             *
 * arity     : the resulting arity (or -1 if no arity present and !strict) *
 * aux_no    : the resulting aux no (if decode_aux != 0) or -1             *
 *                                                                         *
 * Returns NULL if str is not well encoded or the next position in str     *
 *-------------------------------------------------------------------------*/
char *
Decode_Hexa(char *str, int strict, int quote, int decode_aux,
	    char *module, char *pred, int *arity, int *aux_no)
{
  int n;
  int module_encode;
  int pred_encode;
  char *p, *q;

  if (*str++ != 'X' || *str < '0' || *str >= '5')
    return NULL;

  n = *str++ - '0';
  module_encode = n >> 1;
  pred_encode = n & 1;

  if (*str++ != '_')
    return NULL;

  if (module_encode == 0) 	/* no module qualif */
      *module = '\0';
  else
    {
      str = (module_encode == 1) ?
	Copy_Not_Encoded_String(str, module) :
	Decode_String(str, module);

      if (str == NULL || *str++ != '_' || *str++ != '_')
	return NULL;

      if (quote)
	Quote_If_Needed(module);
    }

  str = (pred_encode == 0) ?
    Copy_Not_Encoded_String(str, pred) :
    Decode_String(str, pred);

  if (str == NULL || *pred == '\0')
    return NULL;

  *arity = -1;
  if (*str == '_' && str[1] == '_' && str[2] == 'a') /*  the arity */
    {
      *arity = strtoul(str + 3, &p, 10); /* +3 to skip '__a' */
      if (p == str + 3)
	*arity = -1;
      str = p;
    }

  if (*arity < 0 || *arity > 1024 || isalnum(*p) || *p == '_') /* no valid arity found */
    {
      if (strict)
	return NULL;

      *arity = -1;
    }

  *aux_no = -1;
  if (decode_aux && *pred == '$' && (p = strstr(pred, AUX_STR)) != NULL)
    {
      n = strtol(p + sizeof(AUX_STR) - 1, &q, 10);
      if (*q == '\0')
	{
	  while(--p > pred && isdigit(*p)) /* search for arity of the father pred */
	    ;

	  if (p >= pred && *p == '/')	/* father arity found */
	    {
	      *aux_no = n;
	      *p = '\0';
	      *arity = strtol(p + 1, &p, 10);
	      p = pred;
	      do
		*p = p[1];
	      while(*p++);    	/* skip leading $ */
	    }
	}
    }

  if (quote)
    Quote_If_Needed(pred);

  return str;
}





/*-------------------------------------------------------------------------*
 * DECODE_HEXA_LINE                                                        *
 *                                                                         *
 * str       : the line to decode                                          *
 * format    : printf format to emit decoded strings (or NULL)             *
 * strict    : 1: only predicate indicators, 0: also predicate names       *
 * quote     : 0: no quotes, 1: surround module|pred with quotes if needed *
 * decode_aux: 0: no, 1: as its father, 2: as father + auxiliary number    *
 *                                                                         *
 * Returns a decoded line (static buffer)                                  *
 *-------------------------------------------------------------------------*/
char *
Decode_Hexa_Line(char *str, char *format, int strict, int quote, int decode_aux)
{
  static char result[4096];
  static char module[2048];
  static char pred[1024];
  int arity;
  int aux_no;
  char *src, *dst, *p;
  int n;

  if (format == NULL)
    format = "%s";

  src = str;
  dst = result;
  while(*src)
    {
      p = src;
      if ((*src == 'X' || (*src == '_' && *(p = src + 1) == 'X')) &&
	  (src == str || !isalnum(src[-1])) &&
	  ((p = Decode_Hexa(p, strict, quote, decode_aux, module, pred, &arity, &aux_no)) != NULL))
	{
	  src = p;

	  n = strlen(module);
	  if (n)
	    module[n++] = ':';

	  n += sprintf(module + n, "%s", pred);

	  if (arity >= 0)
	    n += sprintf(module + n, "/%d", arity);

	  if (decode_aux == 2 && aux_no >= 0)
	    n += sprintf(module + n, "(aux %d)", aux_no);

	  dst += sprintf(dst, format, module);
	}
      else
	*dst++ = *src++;
    }

  *dst = '\0';
  return result;
}



/*-------------------------------------------------------------------------*
 * COPY_NOT_ENCODED_STRING                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Copy_Not_Encoded_String(char *str, char *buff)
{
  if (!Letter_Digit(*str))
    return NULL;

  *buff++ = *str++;

  while(*str != '\0')
    {
      if (*str == '_')
	{
	  if (str[-1] == '_' || str[1] == '\0')
	    {
	      str--;
	      buff--;
	      break;
	    }
	}
      else
	{
	  if (!Letter_Digit(*str))
	    break;
	}
      *buff++ = *str++;
    }


  *buff = '\0';
  return str;
}




/*-------------------------------------------------------------------------*
 * DECODE_STRING                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Decode_String(char *str, char *buff)
{
  while(Hexa_Digit(*str) && Hexa_Digit(str[1]))
    {
      *buff++ = Hex_Num(*str) * 16 + Hex_Num(str[1]);
      str += 2;
    }

  *buff = '\0';
  return str;
}




/*-------------------------------------------------------------------------*
 * QUOTE_IF_NEEDED                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Quote_If_Needed(char *str)
{
  static char buff[2048];
  char *p;
  char *q, *r;


  if (islower(*str))
    {
      for(p = str; isalnum(*p) || *p == '_'; p++)
	;

      if (*p == '\0')
	return;
    }


  p = str;
  q = buff;

  *q++ = '\'';
  for(p = str; *p; p++)
    {
      if ((r = (char *) strchr(pl_escape_char, *p)))
	{
	  *q++ = '\\';
	  *q++ = pl_escape_symbol[r - pl_escape_char];
	}
      else if (*p == '\'' || *p == '\\')  /* display twice */
	{
	  *q++ = *p;
	  *q++ = *p;
	}
      else if (!isprint(*p))
	{
	  q += sprintf(q, "\\x%x\\", (unsigned) (unsigned char) *p);
	}
      else
	*q++ = *p;
    }
  *q++ = '\'';
  *q = '\0';
  strcpy(str, buff);
}




/*-------------------------------------------------------------------------*
 * PARSE_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Parse_Atom(char *src, char *dst)
{
  char *p;
  int i;
  char delim;

  if (*src == '\'' || *src == '"')      /* quoted atom (we also accept double quotes") */
    {
      delim = *src++;
      while (*src && (*src != delim || src[1] == delim))
        {
          if (*src == delim)
            {
              *dst++ = delim;
              src += 2;
              continue;
            }

          if (*src == '\\')
            {
	      src++;
	      if (strchr("\\'\"`", *src))      /* \\ or \' or \" or \` */
		{
		  *dst++ = *src++;
		  continue;
		}

	      if ((p = (char *) strchr(pl_escape_symbol, *src)))    /* \a \b \f \n \r \t \v */
		{
		  *dst++ = pl_escape_char[p - pl_escape_symbol];
		  continue;
		}

	      if (*src == 'x')
		{
		  src++;
		  i = 16;
		}
	      else
		i = 8;
	      i = strtol(src, &p, i);       /* stop on the closing \ */
	      if (p == src || !isxdigit(*src) || *p != '\\')  /* isxdigit test is for sign */
		return NULL;
	      *dst++ = (char) i;
	      src = p + 1;
            }
	  else
	    *dst++ = *src++;	/* normal char */
        }
      if (*src != delim)
	return NULL;
      src++;
    }
  else
    {
      if (!isalpha(*src) && *src != '_' && *src != '$')
	return NULL;

      while (isalnum(*src) || *src == '_' || *src == '$')
	{
	  *dst++ = *src++;
	}
    }


  *dst = '\0';
  return src;
}
