/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : format_c.c                                                      *
 * Descr.: formatted output management - C part                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

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

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Format(StmInf *pstm, char *format, WamWord *lst_adr);

static WamWord Read_Arg(WamWord **lst_adr);

static char *Arg_Atom(WamWord **lst_adr);

static long Arg_Integer(WamWord **lst_adr);

static double Arg_Float(WamWord **lst_adr);




/*-------------------------------------------------------------------------*
 * FORMAT_3                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Format_3(WamWord sora_word, WamWord format_word, WamWord args_word)
{
  WamWord word, tag_mask;
  int stm;
  StmInf *pstm;
  char *str;
  char buff[2048];


  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = stm_tbl + stm;

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  DEREF(format_word, word, tag_mask);

  if (tag_mask == TAG_ATM_MASK && word != NIL_WORD)
    str = atom_tbl[UnTag_ATM(word)].name;
  else
    {
      strcpy(buff, Rd_Codes_Check(format_word));
      str = buff;
    }

  Format(stm_tbl + stm, str, &args_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Format_2(WamWord format_word, WamWord args_word)
{
  Format_3(NOT_A_WAM_WORD, format_word, args_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Format(StmInf *pstm, char *format, WamWord *lst_adr)

#define IMPOSS -12345678

{
  WamWord word;
  Bool has_n;
  long generic;
  long n, n1;
  char *p;
  long x;
  double d;
  int lg, stop;
  int i, k;
  char *format_stack[256];
  char **top_stack = format_stack;
  char buff[2048];


  *top_stack++ = format;
  do
    {
      format = *--top_stack;

      while (*format)
	{
	  if (*format == '%')	/* C printf format */
	    {
	      if (format[1] == '%')
		{
		  Stream_Putc('%', pstm);
		  format += 2;
		  continue;
		}

	      p = buff;
	      n = n1 = IMPOSS;

	      do
		if ((*p++ = *format++) == '*')
		  {
		    if (n == IMPOSS)
		      n = Arg_Integer(&lst_adr);
		    else
		      n1 = Arg_Integer(&lst_adr);
		  }
	      while ((char *) strchr("diouxXpnceEfgGs", p[-1]) == NULL);

	      *p = '\0';
	      if (strchr("eEfgG", p[-1]) == NULL)
		{
		  generic = (p[-1] == 's') ? (long) Arg_Atom(&lst_adr)
		    : Arg_Integer(&lst_adr);
		  if (n != IMPOSS)
		    {
		      if (n1 != IMPOSS)
			Stream_Printf(pstm, buff, n, n1, generic);
		      else
			Stream_Printf(pstm, buff, n, generic);
		    }
		  else
		    Stream_Printf(pstm, buff, generic);
		}
	      else
		{
		  d = Arg_Float(&lst_adr);
		  if (n != IMPOSS)
		    {
		      if (n1 != IMPOSS)
			Stream_Printf(pstm, buff, n, n1, d);
		      else
			Stream_Printf(pstm, buff, n, d);
		    }
		  else
		    Stream_Printf(pstm, buff, d);
		}
	      continue;
	    }

	  if (*format != '~')
	    {
	      Stream_Putc(*format, pstm);
	      format++;
	      continue;
	    }

	  if (*++format == '*')
	    {
	      n = Arg_Integer(&lst_adr);
	      format++;
	      has_n = TRUE;
	    }
	  else
	    {
	      p = format;
	      n = strtol(format, &format, 10);
	      has_n = (format != p);
	    }



	  switch (*format)
	    {
	    case 'a':
	      p = Arg_Atom(&lst_adr);
	      if (has_n)
		Stream_Printf(pstm, "%*s", -n, p);
	      else
		Stream_Puts(p, pstm);
	      break;

	    case 'c':
	      x = Arg_Integer(&lst_adr);
	      if (!Is_Valid_Code(x))
		Pl_Err_Representation(representation_character_code);

	      do
		Stream_Putc(x, pstm);
	      while (--n > 0);
	      break;

	    case 'e':
	    case 'E':
	    case 'f':
	    case 'g':
	    case 'G':
	      x = *format;
	      d = Arg_Float(&lst_adr);

	      if (has_n)
		sprintf(buff, "%%.%ld%c", n, (char) x);
	      else
		sprintf(buff, "%%%c", (char) x);

	      Stream_Printf(pstm, buff, d);
	      break;

	    case 'd':
	    case 'D':
	      x = Arg_Integer(&lst_adr);

	      if (n == 0 && *format == 'd')
		{
		  Stream_Printf(pstm, "%ld", x);
		  break;
		}

	      if (x < 0)
		{
		  Stream_Putc('-', pstm);
		  x = -x;
		}

	      sprintf(buff, "%ld", x);
	      lg = strlen(buff) - n;
	      if (lg <= 0)
		{
		  Stream_Puts("0.", pstm);
		  for (i = 0; i < -lg; i++)
		    Stream_Putc('0', pstm);
		  Stream_Printf(pstm, "%ld", x);
		  break;
		}


	      stop = (*format == 'D') ? lg % 3 : -1;

	      if (stop == 0)
		stop = 3;

	      for (p = buff, i = 0; *p; p++, i++)
		{
		  if (i == lg)
		    Stream_Putc('.', pstm), stop = -1;

		  if (i == stop)
		    Stream_Putc(',', pstm), stop += 3;
		  Stream_Putc(*p, pstm);
		}
	      break;

	    case 'r':
	    case 'R':
	      x = Arg_Integer(&lst_adr);

	      if (!has_n || n < 2 || n > 36)
		n = 8;

	      k = ((*format == 'r') ? 'a' : 'A') - 10;

	      if (x < 0)
		{
		  Stream_Putc('-', pstm);
		  x = -x;
		}

	      p = buff + sizeof(buff) - 1;
	      *p = '\0';
	      do
		{
		  i = x % n;
		  x = x / n;
		  --p;
		  *p = (i < 10) ? i + '0' : i + k;
		}
	      while (x);
	      Stream_Puts(p, pstm);
	      break;

	    case 's':
	    case 'S':
	      word = Read_Arg(&lst_adr);
	      if (*format == 's')
		p = Rd_Codes_Check(word);
	      else
		p = Rd_Chars_Check(word);

	      if (has_n)
		Stream_Printf(pstm, "%-*.*s", n, n, p);
	      else
		Stream_Printf(pstm, "%s", p);
	      break;

	    case 'i':
	      do
		Read_Arg(&lst_adr);
	      while (--n > 0);
	      break;

	    case 'k':
	      word = Read_Arg(&lst_adr);
	      Write_Term(pstm, -1, MAX_PREC,
			 WRITE_IGNORE_OP | WRITE_QUOTED, word);
	      break;

	    case 'q':
	      word = Read_Arg(&lst_adr);
	      Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
			 WRITE_NAME_VARS | WRITE_QUOTED, word);
	      break;

	    case 'p':		/* only work if print.pl is linked */
	      word = Read_Arg(&lst_adr);
	      Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
			 WRITE_NAME_VARS | WRITE_PORTRAYED, word);
	      break;

	    case 'w':
	      word = Read_Arg(&lst_adr);
	      Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
			 WRITE_NAME_VARS, word);
	      break;

	    case '~':
	      Stream_Putc('~', pstm);
	      break;

	    case 'N':
	      if (pstm->line_pos == 0)
		break;
	    case 'n':
	      do
		Stream_Putc('\n', pstm);
	      while (--n > 0);
	      break;

	    case '?':
	      if (format[1])
		*top_stack++ = format + 1;

	      format = Arg_Atom(&lst_adr);
	      continue;

	    default:
	      Pl_Err_Domain(domain_format_control_sequence,
			    Tag_ATM(ATOM_CHAR(*format)));
	    }
	  format++;
	}
    }
  while (top_stack > format_stack);
}




/*-------------------------------------------------------------------------*
 * READ_ARG                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Read_Arg(WamWord **lst_adr)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord car_word;


  DEREF(**lst_adr, word, tag_mask);

  if (tag_mask != TAG_LST_MASK)
    {
      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	Pl_Err_Domain(domain_non_empty_list, word);

      Pl_Err_Type(type_list, word);
    }
  
  adr = UnTag_LST(word);
  car_word = Car(adr);
  *lst_adr = &Cdr(adr);

  DEREF(car_word, word, tag_mask);
  return word;
}




/*-------------------------------------------------------------------------*
 * ARG_ATOM                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Arg_Atom(WamWord **lst_adr)
{
  WamWord word;

  word = Read_Arg(lst_adr);

  return atom_tbl[Rd_Atom_Check(word)].name;
}




/*-------------------------------------------------------------------------*
 * ARG_INTEGER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static long
Arg_Integer(WamWord **lst_adr)
{
  WamWord word;

  word = Read_Arg(lst_adr);

  Math_Load_Value(word, &word);
  return Rd_Integer_Check(word);
}




/*-------------------------------------------------------------------------*
 * ARG_FLOAT                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static double
Arg_Float(WamWord **lst_adr)
{
  WamWord word;

  word = Read_Arg(lst_adr);

  Math_Load_Value(word, &word);
  return Rd_Number_Check(word);
}
