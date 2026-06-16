/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : format_c.c                                                      *
 * Descr.: formatted output management - C part                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h"

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

#define MAX_PAD 1024		/* max consecutive ~t */

typedef struct {
  char *start_ptr;		/* ptr to buff of the stm_pad str stream */
  char fill_char;		/* char used to fill padding */
}
PadInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Format(StmInf *pstm, char *format, WamWord args_word);

static void Flush_Padding(StmInf *pstm_orig, int stm_pad, int nb_pad,
			  PadInf pad[], PlLong free_space);

static WamWord Read_Arg(WamWord **lst_adr);

static char *Arg_Atom(WamWord **lst_adr);

static char *Arg_Any_String(WamWord **lst_adr);

static PlLong Arg_Integer(WamWord **lst_adr);

static double Arg_Float(WamWord **lst_adr);




#define GET_WRITE_OPTIONS            X1_246765745F77726974655F6F7074696F6E73

Prolog_Prototype(GET_WRITE_OPTIONS, 1);




/*-------------------------------------------------------------------------*
 * PL_FORMAT_3                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Format_3(WamWord sora_word, WamWord format_word, WamWord args_word)
{
  WamWord word, tag_mask;
  int stm;
  StmInf *pstm;
  char buff[2048];		/* should be enough most of the case */
  char *p;
  size_t l;


  stm = (sora_word == NOT_A_WAM_WORD) ? pl_stm_current_output :
    Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = pl_stm_tbl[stm];

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  DEREF(format_word, word, tag_mask);

  /* copy from pl_glob_buff (in case of ~s occurs later) */
  p = Pl_Rd_Atom_Or_Chars_Or_Codes_Check(format_word);
  l = strlen(p);
  if (l < sizeof(buff))
    {
      strcpy(buff, p);
      p = buff;
    }
  else
    {
      p = Strdup(p);
    }

  Format(pstm, p, args_word);

  if (p != buff)
    Free(p);			/* missed in case of Pl_Err_... */
}




/*-------------------------------------------------------------------------*
 * PL_FORMAT_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Format_2(WamWord format_word, WamWord args_word)
{
  Pl_Format_3(NOT_A_WAM_WORD, format_word, args_word);
}




/*-------------------------------------------------------------------------*
 * FORMAT                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Format(StmInf *pstm, char *format, WamWord args_word)

#define IMPOSS -12345678
#define DEFAULT_N(def_n) if (!has_n) n = (def_n)

{
  WamWord *lst_adr = &args_word;
  WamWord word;
  Bool has_n;
  char fmt_char;
  PlLong n, n1;
  char *p;
  PlLong x;
  double d;
  int lg, stop;
  int i, k, res;
  char buff[2048];
  
#define MAX_PAD 1024
  StmInf *pstm_orig = pstm;	/* to switch between original stream and the str stream */
  PlLong last_line_count = pstm->line_count; /* to detect if a newline occured */
  PadInf pad[MAX_PAD];		/* table of active tab-paddings */
  int nb_pad = 0;		/* nb of active tab-paddings */
  int stm_pad = -1;		/* str stream to use fo active tab-paddings (-1 if not yet created) */
  PlLong tab_stop = 0;		/* tab stop (i.e. column boundary): numbered from 0 */
  PlLong curr_pos, free_space;
#define CLEAN if (stm_pad >= 0) { Pl_Delete_Str_Stream(stm_pad); stm_pad = -1; }
 

  while (*format)
    {
      if (*format != '~')
	{
	  Pl_Stream_Putc(*format, pstm);
	  format++;
	  continue;
	}

      if (*++format == '*')
	{
	  n = Arg_Integer(&lst_adr);
	  if (n < 0)
	    {
	      CLEAN;
	      Pl_Err_Domain(pl_domain_not_less_than_zero, Tag_INT(n));
	    }
	  format++;
	  has_n = TRUE;
	}
      else
	{
	  p = format;
	  if (*p == '`' && p[1] != '\0') /* form `<char> n=ascii code of <char> */
	    {
	      n = p[1];
	      format += 2;
	    }
	  else
	    {
	      n = (PlLong) Str_To_PlLong(format, &format, 10); /* 0 is returned if no conversion is possible */

	      /* In case of overflow strtol() returns LONG_MIN or LONG_MAX and
	       * sets errno to ERANGE (NB: not on windows). We dont test it because 
	       * LONG_MIN is < INT_LOWEST_VALUE and LONG_MAX is > INT_GREATEST_VALUE. 
	       * We will detect it at return from this function.
	       */
	      if (TEST_INT_OVERFLOW(n))
		{
		  CLEAN;
		  Pl_Err_Evaluation(pl_evaluation_int_overflow);
		}
	    }
	  has_n = (format != p);
	}

      fmt_char = *format;
      switch (fmt_char)
	{
	case 'a':
	  p = Arg_Atom(&lst_adr);
	  if (has_n)
	    Pl_Stream_Printf(pstm, "%*s", -n, p);
	  else
	    Pl_Stream_Puts(p, pstm);
	  break;

	case 'c':
	  x = Arg_Integer(&lst_adr);
	  if (!Is_Valid_Code(x))
	    {
	      CLEAN;
	      Pl_Err_Representation(pl_representation_character_code);
	    }

	  DEFAULT_N(1);
	  while(n-- > 0)
	    Pl_Stream_Putc((int) x, pstm);
	  break;

	case 'e':
	case 'E':
	case 'f':
	case 'F':
	case 'g':
	case 'G':
	  d = Arg_Float(&lst_adr);

	  if (has_n)
	    sprintf(buff, "%%.%" PL_FMT_d "%c", n, fmt_char);
	  else
	    sprintf(buff, "%%%c", fmt_char);

	  Pl_Stream_Printf(pstm, buff, d);
	  break;

	case 'd':
	case 'D':
	  x = Arg_Integer(&lst_adr);

	  DEFAULT_N(0);
	  if (n == 0 && fmt_char == 'd')
	    {
	      Pl_Stream_Printf(pstm, "%" PL_FMT_d, x);
	      break;
	    }

	  if (x < 0)
	    {
	      Pl_Stream_Putc('-', pstm);
	      x = -x;
	    }

	  sprintf(buff, "%" PL_FMT_d, x);
	  lg = (int) (strlen(buff) - n);
	  if (lg <= 0)
	    {
	      Pl_Stream_Puts("0.", pstm);
	      for (i = 0; i < -lg; i++)
		Pl_Stream_Putc('0', pstm);
	      Pl_Stream_Printf(pstm, "%" PL_FMT_d, x);
	      break;
	    }

	  stop = (fmt_char == 'D') ? lg % 3 : -1;

	  if (stop == 0)
	    stop = 3;

	  for (p = buff, i = 0; *p; p++, i++)
	    {
	      if (i == lg)
		Pl_Stream_Putc('.', pstm), stop = -1;

	      if (i == stop)
		Pl_Stream_Putc(',', pstm), stop += 3;
	      Pl_Stream_Putc(*p, pstm);
	    }
	  break;

	case 'r':
	case 'R':
	  x = Arg_Integer(&lst_adr);

	  DEFAULT_N(8);
	  if (n < 2 || n > 36)
	    {
	      CLEAN;
	      Pl_Err_Domain(pl_domain_radix, Tag_INT(n));
	    }

	  k = ((fmt_char == 'r') ? 'a' : 'A') - 10;

	  if (x < 0)
	    {
	      Pl_Stream_Putc('-', pstm);
	      x = -x;
	    }

	  p = buff + sizeof(buff) - 1;
	  *p = '\0';
	  do
	    {
	      i = (int) (x % n);
	      x = x / n;
	      --p;
	      *p = (i < 10) ? i + '0' : i + k;
	    }
	  while (x);
	  Pl_Stream_Puts(p, pstm);
	  break;

	case 's':
	  p = Arg_Any_String(&lst_adr);
 
	  if (has_n)
	    Pl_Stream_Printf(pstm, "%-*.*s", n, n, p);
	  else
	    Pl_Stream_Printf(pstm, "%s", p);
	  break;

	case 'i':
	  Read_Arg(&lst_adr);
	  break;

	case 'k':
	  word = Read_Arg(&lst_adr);
	  Pl_Write_Term(pstm, 0, MAX_PREC,
			WRITE_IGNORE_OP | WRITE_QUOTED, NULL, word);
	  break;

	case 'q':
	  word = Read_Arg(&lst_adr);
	  Pl_Write_Term(pstm, 0, MAX_PREC, WRITE_NUMBER_VARS |
			WRITE_NAME_VARS | WRITE_QUOTED, NULL, word);
	  break;

	case 'p':           /* only work if print.pl is linked */
	  word = Read_Arg(&lst_adr);
	  Pl_Write_Term(pstm, 0, MAX_PREC, WRITE_NUMBER_VARS |
			WRITE_NAME_VARS | WRITE_PORTRAYED, NULL, word);
	  break;

	case 'w':
	  word = Read_Arg(&lst_adr);
	  Pl_Write_Term(pstm, 0, MAX_PREC, WRITE_NUMBER_VARS |
			WRITE_NAME_VARS, NULL, word);
	  break;

	case 'W':
	  word = Read_Arg(&lst_adr);
	  A(0) = Read_Arg(&lst_adr);

	  Pl_Query_Begin(TRUE);
	  res = Pl_Query_Call_Native(Prolog_Predicate(GET_WRITE_OPTIONS, 1));
	  Pl_Query_End(PL_RECOVER);
  
	  if (res == PL_EXCEPTION)
	    Pl_Throw(Pl_Get_Exception());

	  Pl_Write_Term_Options_In_Sys_Var(pstm, word);
	  break;

	case '~':
	  Pl_Stream_Putc('~', pstm);
	  break;

	case 'N': /* NB: the 'n' is ignored since after the the first \n we are at the begin of line */
	  if (pstm->line_pos > 0)
	    Pl_Stream_Putc('\n', pstm);
	  break;

	case 'n':
	  DEFAULT_N(1);
	  while (n-- > 0)
	    Pl_Stream_Putc('\n', pstm);
	  break;

	case '%':		/* interface to C printf(3) */
	  if (format[1] == '%')
	    {
	      Pl_Stream_Putc('%', pstm);
	      format += 2;
	      continue;
	    }

	  p = buff;
	  n = n1 = IMPOSS;

	  /* quick/dirty parsing of C printf format (stop at first letter) */
	  do
	    if ((*p++ = fmt_char = *format++) == '*')
	      {
		if (n == IMPOSS)
		  n = Arg_Integer(&lst_adr);
		else if (n1 == IMPOSS)
		  n1 = Arg_Integer(&lst_adr);
	      }
	  while (fmt_char >= ' ' && fmt_char <= '9');
	  *p = '\0';
	  format--;		/* sync for break */
	  
	  if (strchr("diouxXpcsaAeEfFgG", fmt_char) == NULL) /* do not include 'n' (dangerous) */
	    {
	    c_printf_error:
	      CLEAN;
	      Pl_Err_Domain(pl_domain_format_control_sequence,
			    Tag_ATM(Pl_Create_Atom(buff)));
	    }


#define INVOC_PRINTF(arg)				\
  if (n != IMPOSS)					\
    {							\
      if (n1 != IMPOSS)					\
	k = Pl_Stream_Printf(pstm, buff, n, n1, arg);	\
      else						\
	k = Pl_Stream_Printf(pstm, buff, n, arg);	\
    }							\
  else							\
    k = Pl_Stream_Printf(pstm, buff, arg);		\
  if (k < 0)						\
    goto c_printf_error;


	  if (fmt_char == 's')			/* a string: passed as a C char * */
	    {
	      char *q = Arg_Any_String(&lst_adr);
	      INVOC_PRINTF(q);
	    }
	  else if (strchr("aAeEfFgG", fmt_char))/* a float: passed as a C double */
	    {
	      d = Arg_Float(&lst_adr);
	      INVOC_PRINTF(d);
	    }
	  else if (fmt_char == 'c') 		/* a character: passed as a C int */
	    {
	      word = Read_Arg(&lst_adr);
	      Pl_Math_Load_Value(word, &word);
	      k = Pl_Rd_C_Int_Check(word);
	      INVOC_PRINTF(k);
	    }
	  else 					/* otherwise passed as a C PlLong */
	    {
	      x = Arg_Integer(&lst_adr);
	      if (fmt_char != 'p') 		/* is 'p' (pointer) really useful ? */
		{
		  strcpy(p - 1, PL_FMT_d); 	/* add correct length modifier (e.g. lld) */
		  p += strlen(p);
		  p[-1] = fmt_char; 		/* fix conversion specifier (e.g. llx) */
		}
	      INVOC_PRINTF(x);
	    }
	  break;

	  /* Tabs and tab-padding manangement (~|, ~+, and ~t). 
	   * When ~t is encountered, output is redirected from the original stream 
	   * (initially pstm_orig) to a string stream (str stream) stm_pad which is created 
	   * at the first ~t and remains alive until the end of the function.
	   * At each ~t a new entry is added entry to the pad[] array with 2 data: 
	   * - the start_ptr in the str stream (where chars, after this ~t, are written)
	   * - the padding character.
	   *
	   * When ~| or ~+ is encountered, the pad[] array is flushed by emitting the padding,
	   * followed by the substring associated with pad[i] (from its start ptr to the start 
	   * ptr of pad[i+1]).
	   *
	   * This flush also occurs in two other situations:
	   * - at the end (when the format string is fully treated) in case some ~t are pending.
	   * - when a newline occurs inside ~|, ~+, ~t. In this case, the pads are flushed and
	   *   tab-related information is discarded (newline is treated as ~|~n~|).
	   * Detecting a newline could be done by directly handling ~n, but this would miss \n 
	   * produced by other means. Instead, newline detection is performed by comparing the 
	   * line_count associated with the streams (the original stream and the str stream).
	   */
	case '+':		/* ~N+ set relative tab stop (at +8 or +N) */
	  DEFAULT_N(8);
	  // fall through	      
	case '|':		/* ~N| set absolute tab stop (here or at pos N) */
	  if (last_line_count != pstm->line_count)
	    {		/* newline occured juste before this ~| or ~+ - flush (no free space) */
	      Flush_Padding(pstm_orig, stm_pad, nb_pad, pad, 0); 
	      nb_pad = 0;
	      tab_stop = 0;
	    }
		
	  /* curr_pos = nb chars written: on original pstm + on the str stream stm_pad */
	  curr_pos = pstm_orig->line_pos;
	  if (nb_pad > 0)
	    curr_pos += pstm->line_pos;

	  if (fmt_char == '+')
	    tab_stop += n;
	  else
	    {
	      DEFAULT_N(curr_pos);
	      tab_stop = n;
	    }
	  free_space = tab_stop - curr_pos;
	  if (nb_pad == 0)
	    {
	      for(i = 0; i < free_space; i++)
		Pl_Stream_Putc(' ', pstm);
	    }
	  else
	    {
	      Flush_Padding(pstm_orig, stm_pad, nb_pad, pad, free_space);
	    }
	  nb_pad = 0;
	  pstm = pstm_orig;
	  last_line_count = pstm->line_count;
	  break;

	case 't':		/* insert tab-padding */
	  DEFAULT_N(' ');

	  if (last_line_count != pstm->line_count)
	    {		/* newline occured between previous ~t and this ~t - flush (no free space) */
	      Flush_Padding(pstm_orig, stm_pad, nb_pad, pad, 0);
	      nb_pad = 0;
	      tab_stop = 0;
	      last_line_count = pstm->line_count;
	    }

	  if (nb_pad == 0)
	    {
	      if (stm_pad < 0)
		stm_pad = Pl_Add_Str_Stream(NULL, 0); /* prop_other ? TERM_STREAM_ATOM ? */
	      pstm = pl_stm_tbl[stm_pad]; /* will only be restored to pstm_orig at ~| or ~+ */
	      last_line_count = pstm->line_count;
	    }

	  if (nb_pad >= MAX_PAD)
	    {
	      CLEAN;
	      Pl_Err_Resource(Pl_Create_Atom("too many consecutive ~t in format"));
	    }
	      
	  pad[nb_pad].start_ptr = Pl_Get_Ptr_Str_Stream(stm_pad);
	  pad[nb_pad].fill_char = (char) n;
	  nb_pad++;
	  break;
	      
	default:
	  CLEAN;
	  Pl_Err_Domain(pl_domain_format_control_sequence, Tag_ATM(ATOM_CHAR(fmt_char)));
	}
      format++;
    }

  /* end of format occured while remaining ~t - flush (no free space) */
  Flush_Padding(pstm_orig, stm_pad, nb_pad, pad, 0);

  CLEAN;  
  Pl_Check_Empty_List(*lst_adr, args_word);
}




/*-------------------------------------------------------------------------*
 * FLUSH_PADDING                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Flush_Padding(StmInf *pstm, int stm_pad, int nb_pad, PadInf pad[],
	      PlLong free_space)
{
  PlLong spc, rem, n;
  char *p, *end;
  int i;

  if (nb_pad == 0)
    return;

  if (free_space < 0)		/* should not occur */
    free_space = 0;

  spc = free_space / nb_pad;
  rem = free_space % nb_pad;
  
  p = Pl_Get_Buffer_Str_Stream(stm_pad);
  for(i = 0; i < nb_pad; i++)
    {
#if 0				/* insert additional spaces both left and right (balanced) */
      int rl = rem / 2;		/* space to distribute left */
      int rr = rem - rl;	/* space to distribute right */
      n = spc + (i < rl || i >= nb_pad - rr);
#else  				/* insert additional spaces at the end (as by PIP-0110) */
      n = spc + (i >= nb_pad - rem);
#endif
      while(--n >= 0)
	Pl_Stream_Putc(pad[i].fill_char, pstm);

      end = (i < nb_pad - 1) ? pad[i + 1].start_ptr : Pl_Get_Ptr_Str_Stream(stm_pad);
      while(p < end)
	Pl_Stream_Putc(*p++, pstm);
    }
  Pl_Rewind_Str_Stream(stm_pad); /* in case it is reused later */
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
        Pl_Err_Domain(pl_domain_non_empty_list, word);

      Pl_Err_Type(pl_type_list, word);
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

  return pl_atom_tbl[Pl_Rd_Atom_Check(word)].name;
}




/*-------------------------------------------------------------------------*
 * ARG_ANY_STRING                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Arg_Any_String(WamWord **lst_adr)
{
  WamWord word;

  word = Read_Arg(lst_adr);

  return Pl_Rd_Atom_Or_Chars_Or_Codes_Check(word); /* can return pl_glob_buff */
}




/*-------------------------------------------------------------------------*
 * ARG_INTEGER                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong
Arg_Integer(WamWord **lst_adr)
{
  WamWord word;

  word = Read_Arg(lst_adr);

  Pl_Math_Load_Value(word, &word);
  return Pl_Rd_Integer_Check(word);
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

  Pl_Math_Load_Value(word, &word);
  return Pl_Rd_Number_Check(word);
}
