/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : scan_supp.c                                                     *
 * Descr.: scanner support                                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>

#define SCAN_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int c_orig, c;		/* for read */
static int c_type;

static char *err_msg;



						       /* parser variables */
/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Read_Next_Char(StmInf *pstm, Bool convert);

static void Scan_Number(StmInf *pstm, Bool integer_only);

static void Scan_Quoted(StmInf *pstm);

static int Scan_Quoted_Char(StmInf *pstm, Bool convert, int c0, 
			    Bool no_escape);



#define   Unget_Last_Char       Pl_Stream_Ungetc(c_orig, pstm)




/*-------------------------------------------------------------------------*
 * PL_SCAN_PEEK_CHAR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Scan_Peek_Char(StmInf *pstm, Bool convert)
{
  int c_look;

  c_look = Pl_Stream_Peekc(pstm);

  if (convert)
    c_look = Char_Conversion(c_look);

  return c_look;
}




/*-------------------------------------------------------------------------*
 * READ_NEXT_CHAR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Read_Next_Char(StmInf *pstm, Bool convert)
{
  c_orig = c = Pl_Stream_Getc(pstm);

  if (c == EOF)
    c_type = 0;
  else
    {
      if (convert)
	c = Char_Conversion(c);

      c_type = pl_char_type[c];
    }

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_SCAN_TOKEN                                                           *
 *                                                                         *
 * Scan the next pl_token. The flag comma_is_punct specifies if ',' must be*
 * considered as a punctuation (e.g. separator of args of compound term or *
 * of a list) or as an atom.                                               *
 * The scanner only consumes the needed characters of the pl_token, calling*
 * Unget_Last_Char if necessary (see Scan_Number). Thus after a pl_token   *
 * has been read Pl_Stream_Peekc() will return the character directly      *
 * following this pl_token.                                                *
 *-------------------------------------------------------------------------*/
char *
Pl_Scan_Token(StmInf *pstm, Bool comma_is_punct)
{
  int c0;
  char *s;
  Bool layout_before = FALSE;


  err_msg = NULL;

start_scan:

  for (;;)
    {
      Read_Next_Char(pstm, TRUE);
      if (c_type != LA)		/* layout character */
	break;
      layout_before = TRUE;
    }


  pl_token.line = pstm->line_count + 1;
  pl_token.col = pstm->line_pos;


  if (c == EOF)
    {
      pl_token.type = TOKEN_END_OF_FILE;
      return err_msg;
    }


  switch (c_type)
    {
    case SL:			/* small letter */
    case UL:			/* underline */
    case CL:			/* capital letter */
      pl_token.type = (c_type == SL) ? TOKEN_NAME : TOKEN_VARIABLE;
      s = pl_token.name;
      do
	{
	  *s++ = c;
	  Read_Next_Char(pstm, TRUE);
	}
      while (c_type & (UL | CL | SL | DI));
      *s = '\0';
      Unget_Last_Char;
      break;

    case DI:			/* digit */
      Scan_Number(pstm, FALSE);
      break;

    case QT:			/* quote */
    case DQ:			/* double quote */
    case BQ:			/* back quote */
      Scan_Quoted(pstm);
      break;

    case GR:			/* graphic */
      c0 = c;
      Read_Next_Char(pstm, TRUE);
      if (c0 == '.' && (c == EOF || (c_type & (LA | CM))))
	{
	  if (c_type == CM)
	    Unget_Last_Char;

	  pl_token.type = TOKEN_FULL_STOP;
	  break;
	}

      if (c0 == '/' && c == '*')	/* comment */
	{
	  Read_Next_Char(pstm, TRUE);
	  if (c != EOF)
	    do
	      {
		c0 = c;
		Read_Next_Char(pstm, TRUE);
	      }
	    while (c != EOF && (c0 != '*' || c != '/'));

	  if (c == EOF)
	    {
	      pl_token.type = TOKEN_END_OF_FILE;
	      pl_token.line = pstm->line_count + 1;
	      pl_token.col = pstm->line_pos;
	      err_msg = "*/ expected here for /*...*/ comment";
	      break;
	    }

	  layout_before = TRUE;
	  goto start_scan;
	}


      pl_token.type = TOKEN_NAME;
      s = pl_token.name;
      *s++ = c0;
      while (c_type == GR)
	{
	  *s++ = c;
	  Read_Next_Char(pstm, TRUE);
	}
      *s = '\0';
      Unget_Last_Char;
      break;

    case CM:			/* comment character */
      do
	Read_Next_Char(pstm, TRUE);
      while (c != '\n' && c != EOF);
#if 0  // what says standard ? EOF allowad at end of %... comment ?
      if (c == EOF)
	{
	  pl_token.type = TOKEN_END_OF_FILE;
	  pl_token.line = pstm->line_count + 1;
	  pl_token.col = pstm->line_pos;
	  err_msg = "new-line expected here for %%... comment";
	  break;
	}
#endif
      layout_before = TRUE;
      goto start_scan;

    case PC:			/* punctuation character */
      if (c == '(' && !layout_before)
	{
	  pl_token.type = TOKEN_IMMEDIAT_OPEN;
	  break;
	}

      pl_token.type = TOKEN_PUNCTUATION;
      pl_token.punct = c;
      break;

    case SC:			/* solo character */
      if (c == ',' && comma_is_punct)
	{
	  pl_token.type = TOKEN_PUNCTUATION;
	  pl_token.punct = c;
	  break;
	}

      pl_token.type = TOKEN_NAME;
      pl_token.name[0] = c;
      pl_token.name[1] = '\0';
      break;

    case EX:			/* extended character */
      pl_token.type = TOKEN_EXTENDED;
      pl_token.name[0] = c;
      pl_token.name[1] = '\0';
      break;
    }

  return err_msg;
}




/*-------------------------------------------------------------------------*
 * SCAN_NUMBER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Scan_Number(StmInf *pstm, Bool integer_only)
{
  int lg;
  int radix;
  char *p, *f;
  int c_orig0;

  /* at entry: c is a digit */

  p = pl_token.name;
  do
    {
      *p++ = c;
      Read_Next_Char(pstm, TRUE);
    }
  while (c_type == DI);
  lg = p - pl_token.name;

  if (!integer_only &&		/* float if . and digit */
      c == '.' && isdigit(Pl_Scan_Peek_Char(pstm, TRUE)))
    goto is_a_float;

				/* integer number */
  pl_token.type = TOKEN_INTEGER;
  *p = '\0';


  /* if case of an underflow/overflow strtol() returns LONG_MIN/LONG_MAX and
   * sets errno to ERANGE. We dont test it because LONG_MIN is < INT_LOWEST_VALUE
   * and LONG_MAX is > INT_GREATEST_VALUE. We will detect it at return from
   * this function.
   */
  pl_token.int_num = strtol(pl_token.name, &p, 10);

  if (lg != 1 || pl_token.int_num != 0 || strchr("'bBoOxX", c) == NULL)
    goto push_back;

  if (c == '\'')		/* 0'<character> */
    {
      c = Scan_Quoted_Char(pstm, TRUE, '\'', FALSE);
      if (c == -1)		/* <character> is ' */
	{
	  pl_token.line = pstm->line_count + 1;
	  pl_token.col = pstm->line_pos + 1;
	  err_msg = "quote character expected here";
	}

      if (c == -2 || c == -3)
	{
	  Unget_Last_Char;

	  pl_token.line = pstm->line_count + 1;
	  pl_token.col = pstm->line_pos + 1;
	  err_msg = "character expected here";
	}

      pl_token.int_num = c;
      return;
    }

  radix = (c == 'b' || c == 'B') ? (f = "01", 2) :
    (c == 'o' || c == 'O') ? (f = "01234567", 8)
    : (f = "0123456789abcdefABCDEF", 16);
  p = pl_token.name;
  Read_Next_Char(pstm, TRUE);
  while (strchr(f, c) != NULL)
    {
      *p++ = c;
      Read_Next_Char(pstm, TRUE);
    }
  *p = '\0';

  pl_token.int_num = strtol(pl_token.name, &p, radix);
  goto push_back;


is_a_float:			/* float number */

  pl_token.type = TOKEN_FLOAT;
  *p++ = '.';
  Read_Next_Char(pstm, TRUE);
  while (c_type == DI)
    {
      *p++ = c;
      Read_Next_Char(pstm, TRUE);
    }

  if (c == 'e' || c == 'E')
    {
      c_orig0 = c_orig;
      Read_Next_Char(pstm, TRUE);
      if (!(c_type == DI || ((c == '+' || c == '-') &&
			     isdigit(Pl_Scan_Peek_Char(pstm, TRUE)))))
	{
	  Unget_Last_Char;
	  c_orig = c_orig0;
	  goto end_float;
	}

      *p++ = 'e';
      *p++ = c;

      Read_Next_Char(pstm, TRUE);
      while (c_type == DI)
	{
	  *p++ = c;
	  Read_Next_Char(pstm, TRUE);
	}
    }


end_float:
  *p = '\0';
  sscanf(pl_token.name, "%lf", &pl_token.float_num);

push_back:
  Unget_Last_Char;
}




/*-------------------------------------------------------------------------*
 * SCAN_QUOTED                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Scan_Quoted(StmInf *pstm)
{
  int c0;
  char *s;
  Bool convert = (c_orig != '\'');
  Bool no_escape;
  int i = 0;

  if (c_type == QT)
    {
      pl_token.type = TOKEN_NAME;
      i = 0;
    }
  else if (c_type == DQ)
    {
      pl_token.type = TOKEN_STRING;
      i = Flag_Value(FLAG_DOUBLE_QUOTES);
    }
  else
    {
      pl_token.type = TOKEN_BACK_QUOTED;
      i = Flag_Value(FLAG_BACK_QUOTES);
    }

  s = pl_token.name;
  c0 = c;
  no_escape = i >> FLAG_NO_ESCAPE_BIT;

  for (;;)
    {
      c = Scan_Quoted_Char(pstm, convert, c0, no_escape);
      if (c == -1)
	{
	  *s = '\0';
	  return;
	}

      if (c == -2)		/* EOF or \n */
	break;

      if (c == -3)		/* \ followed by \n */
	continue;

      *s++ = c;
    }
  /* error */
  *s = '\0';

  if (err_msg)
    return;

  Unget_Last_Char;

  pl_token.line = pstm->line_count + 1;
  pl_token.col = pstm->line_pos + 1;

  switch (pl_token.type)
    {
    case TOKEN_NAME:
      err_msg = "quote character expected here";
      break;

    case TOKEN_BACK_QUOTED:
      err_msg = "back quote character expected here";
      break;

    case TOKEN_STRING:
      err_msg = "double quote character expected here";
      break;

    default:			/* to avoid compiler warning */
      ;
    }
}




/*-------------------------------------------------------------------------*
 * SCAN_QUOTED_CHAR                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Scan_Quoted_Char(StmInf *pstm, Bool convert, int c0, Bool no_escape)
{
  int radix;
  char *p, *f;
  int x, i;

  Read_Next_Char(pstm, convert);
  if (c == c0)
    {
      if (Pl_Scan_Peek_Char(pstm, convert) != c0)	/* '' or "" or `` */
	return -1;

      Read_Next_Char(pstm, convert);
      return c;
    }

  if (c == EOF || c == '\n')
    return -2;

  if (c != '\\' || no_escape)
    return c;
  /* escape sequence */
  Read_Next_Char(pstm, convert);
  if (c == '\n')		/* \ followed by \n */
    return -3;

  if (strchr("\\'\"`", c))	/* \\ or \' or \" or \` */
    return c;

  if ((p = (char *) strchr(pl_escape_symbol, c)))	/* \a \b \f \n \r \t \v */
    return pl_escape_char[p - pl_escape_symbol];

  if (c == 'x' || ('0' <= c && c <= '7'))	/* \xnn\ \nn\ */
    {
      if (c == 'x')
	{
	  radix = 16;
	  f = "0123456789abcdefABCDEF";
	  x = 0;
	}
      else
	{
	  radix = 8;
	  f = "01234567";
	  x = c - '0';
	}

      Read_Next_Char(pstm, convert);
      while ((p = strchr(f, c)) != NULL)
	{
	  i = p - f;
	  if (i >= 16)
	    i -= 6;
	  x = x * radix + i;
	  Read_Next_Char(pstm, convert);
	}

      if (!Is_Valid_Code(x) && err_msg == NULL)
	{
	  pl_token.line = pstm->line_count + 1;
	  pl_token.col = pstm->line_pos;
	  err_msg = "invalid character code in \\constant\\ sequence";
	}
      if (c != '\\')
	{
	  if (err_msg == NULL)
	    {
	      pl_token.line = pstm->line_count + 1;
	      pl_token.col = pstm->line_pos;
	      err_msg = "\\ expected in \\constant\\ sequence";
	    }

	  Unget_Last_Char;
	}

      return (int) (unsigned char) x;
    }

  if (err_msg == NULL)
    {
      pl_token.line = pstm->line_count + 1;
      pl_token.col = pstm->line_pos;
      err_msg = "unknown escape sequence";
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * PL_RECOVER_AFTER_ERROR                                                  *
 *                                                                         *
 * Finds the next full stop (to restart after an error)                    *
 *-------------------------------------------------------------------------*/
void
Pl_Recover_After_Error(StmInf *pstm)
#define Next_Char   Read_Next_Char(pstm, convert); if (c == EOF) return
{
  int c0;
  Bool convert;

  if (pstm->eof_reached)
    return;

  for (;;)
    {
    loop:
      convert = FALSE;
      Next_Char;

      if (c == '.')
	{
	  Next_Char;
	  if (c_type & (LA | CM))
	    return;		/* full stop found */
	}

      if ((c_type & (QT | DQ | BQ)) == 0)
	continue;

      /* quoted pl_token */
      c0 = c;
      convert = (c_orig != '\'');

      for (;;)
	{
	  Next_Char;
	  if (c == c0)		/* detect end of pl_token - also for  '' or "" or `` */
	    break;

	  if (c != '\\')
	    continue;
	  /* escape sequence */
	  Next_Char;
	  if (c == '\n')	/* \ followed by \n */
	    continue;

	  if (strchr("\\'\"`", c))	/* \\ or \' or \" or \`  " */
	    continue;

	  if (strchr(pl_escape_symbol, c))	/* \a \b \f \n \r \t \v */
	    continue;

	  if (c != 'x' && (c < '0' || c > '7'))
	    continue;

	  for (;;)		/* \xnn\ \nn\ */
	    {			/* simply find terminal \ */
	      Next_Char;
	      if (c == c0)
		goto loop;

	      if (c == '\\' || !isxdigit(c))
		break;
	    }
	}
    }
}




	  /* Other Scanner facilities */




/*-------------------------------------------------------------------------*
 * PL_SCAN_NEXT_ATOM                                                       *
 *                                                                         *
 * Scan the next atom.                                                     *
 *-------------------------------------------------------------------------*/
char *
Pl_Scan_Next_Atom(StmInf *pstm)
{
  char *s;

  do
    Read_Next_Char(pstm, TRUE);
  while (c_type == LA);		/* layout character */

  pl_token.line = pstm->line_count + 1;
  pl_token.col = pstm->line_pos;

  switch (c_type)
    {
    case SL:			/* small letter */
      s = pl_token.name;
      do
	{
	  *s++ = c;
	  Read_Next_Char(pstm, TRUE);
	}
      while (c_type & (UL | CL | SL | DI));
      *s = '\0';
      Unget_Last_Char;
      break;

    case DQ:			/* double quote */
      if ((Flag_Value(FLAG_DOUBLE_QUOTES) & FLAG_AS_PART_MASK) != FLAG_AS_ATOM)
	goto error;
      goto do_scan_quoted;

    case BQ:			/* back quote */
      if ((Flag_Value(FLAG_BACK_QUOTES) & FLAG_AS_PART_MASK) != FLAG_AS_ATOM)
	goto error;
    case QT:			/* quote */
    do_scan_quoted:
      err_msg = NULL;
      Scan_Quoted(pstm);
      if (err_msg)
	return err_msg;
      break;

    case GR:			/* graphic */
      s = pl_token.name;
      while (c_type == GR)
	{
	  *s++ = c;
	  Read_Next_Char(pstm, TRUE);
	}
      *s = '\0';
      Unget_Last_Char;
      break;

    case SC:			/* solo character */
      pl_token.name[0] = c;
      pl_token.name[1] = '\0';
      break;

    default:
    error:
      Unget_Last_Char;
      return "cannot start an atom (use quotes ?)";
    }

  pl_token.type = TOKEN_NAME;
  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_SCAN_NEXT_NUMBER                                                     *
 *                                                                         *
 * Scan the next number (integer if integer_only is TRUE).                 *
 *-------------------------------------------------------------------------*/
char *
Pl_Scan_Next_Number(StmInf *pstm, Bool integer_only)
{
  Bool minus_op = FALSE;

  for (;;)
    {
      Read_Next_Char(pstm, TRUE);
      if (c_type != LA)		/* layout character */
	break;
    }


  pl_token.line = pstm->line_count + 1;
  pl_token.col = pstm->line_pos;


  if (c == '-' && isdigit(Pl_Scan_Peek_Char(pstm, FALSE)))	/* negative number */
    {
      Read_Next_Char(pstm, TRUE);
      minus_op = TRUE;
    }

  if (c_type != DI)
    {
      Unget_Last_Char;
      return "cannot start a number";
    }


  Scan_Number(pstm, integer_only);
  if (err_msg != NULL)
    return err_msg;

  if (minus_op)
    {
      if (pl_token.type == TOKEN_INTEGER)
	{
	  if (pl_token.int_num > -INT_LOWEST_VALUE)
	    return "integer underflow (exceeds min_integer)";
	  pl_token.int_num = -pl_token.int_num;
	}
      else
	pl_token.float_num = -pl_token.float_num;
    }
  else
    if (pl_token.type == TOKEN_INTEGER && pl_token.int_num > INT_GREATEST_VALUE)
      return "integer overflow (exceeds max_integer)";

  return NULL;
}
