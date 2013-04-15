/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : format_c.c                                                      *
 * Descr.: formatted output management - C part                            *
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

static PlLong Arg_Integer(WamWord **lst_adr);

static double Arg_Float(WamWord **lst_adr);




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
  char *str;
  char buff[2048];


  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = pl_stm_tbl[stm];

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  DEREF(format_word, word, tag_mask);

  if (tag_mask == TAG_ATM_MASK && word != NIL_WORD)
    str = pl_atom_tbl[UnTag_ATM(word)].name;
  else
    {
      strcpy(buff, Pl_Rd_Codes_Check(format_word));
      str = buff;
    }
  Format(pstm, str, &args_word);
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
Format(StmInf *pstm, char *format, WamWord *lst_adr)

#define IMPOSS -12345678

{
  WamWord word;
  Bool has_n;
  PlLong generic;
  PlLong n, n1;
  char *p;
  PlLong x;
  double d;
  int lg, stop;
  int i, k;
  char *format_stack[256];
  char **top_stack;
  char buff[2048];

  //  printf("d: %p\n", &d);
  top_stack = format_stack;

  *top_stack++ = format;
  do
    {
      format = *--top_stack;

      while (*format)
        {
          if (*format == '%')   /* C printf format */
            {
              if (format[1] == '%')
                {
                  Pl_Stream_Putc('%', pstm);
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
                  generic = (p[-1] == 's') ? (PlLong) Arg_Atom(&lst_adr)
                    : Arg_Integer(&lst_adr);
                  if (n != IMPOSS)
                    {
                      if (n1 != IMPOSS)
                        Pl_Stream_Printf(pstm, buff, n, n1, generic);
                      else
                        Pl_Stream_Printf(pstm, buff, n, generic);
                    }
                  else
                    Pl_Stream_Printf(pstm, buff, generic);
                }
              else
                {
                  d = Arg_Float(&lst_adr);
                  if (n != IMPOSS)
                    {
                      if (n1 != IMPOSS)
                        Pl_Stream_Printf(pstm, buff, n, n1, d);
                      else
                        Pl_Stream_Printf(pstm, buff, n, d);
                    }
                  else
                    Pl_Stream_Printf(pstm, buff, d);
                }
              continue;
            }

          if (*format != '~')
            {
              Pl_Stream_Putc(*format, pstm);
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
                Pl_Stream_Printf(pstm, "%*s", -n, p);
              else
                Pl_Stream_Puts(p, pstm);
              break;

            case 'c':
              x = Arg_Integer(&lst_adr);
              if (!Is_Valid_Code(x))
                Pl_Err_Representation(pl_representation_character_code);

              do
                Pl_Stream_Putc(x, pstm);
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
                sprintf(buff, "%%.%" PL_FMT_d "%c", n, (char) x);
              else
                sprintf(buff, "%%%c", (char) x);

              Pl_Stream_Printf(pstm, buff, d);
              break;

            case 'd':
            case 'D':
              x = Arg_Integer(&lst_adr);

              if (n == 0 && *format == 'd')
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
              lg = strlen(buff) - n;
              if (lg <= 0)
                {
                  Pl_Stream_Puts("0.", pstm);
                  for (i = 0; i < -lg; i++)
                    Pl_Stream_Putc('0', pstm);
                  Pl_Stream_Printf(pstm, "%" PL_FMT_d, x);
                  break;
                }


              stop = (*format == 'D') ? lg % 3 : -1;

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

              if (!has_n || n < 2 || n > 36)
                n = 8;

              k = ((*format == 'r') ? 'a' : 'A') - 10;

              if (x < 0)
                {
                  Pl_Stream_Putc('-', pstm);
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
              Pl_Stream_Puts(p, pstm);
              break;

            case 's':
            case 'S':
              word = Read_Arg(&lst_adr);
              if (*format == 's')
                p = Pl_Rd_Codes_Check(word);
              else
                p = Pl_Rd_Chars_Check(word);

              if (has_n)
                Pl_Stream_Printf(pstm, "%-*.*s", n, n, p);
              else
                Pl_Stream_Printf(pstm, "%s", p);
              break;

            case 'i':
              do
                Read_Arg(&lst_adr);
              while (--n > 0);
              break;

            case 'k':
              word = Read_Arg(&lst_adr);
              Pl_Write_Term(pstm, -1, MAX_PREC,
                            WRITE_IGNORE_OP | WRITE_QUOTED, NULL, word);
              break;

            case 'q':
              word = Read_Arg(&lst_adr);
              Pl_Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
			    WRITE_NAME_VARS | WRITE_QUOTED, NULL, word);
              break;

            case 'p':           /* only work if print.pl is linked */
              word = Read_Arg(&lst_adr);
              Pl_Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
                            WRITE_NAME_VARS | WRITE_PORTRAYED, NULL, word);
              break;

            case 'w':
              word = Read_Arg(&lst_adr);
              Pl_Write_Term(pstm, -1, MAX_PREC, WRITE_NUMBER_VARS |
                            WRITE_NAME_VARS, NULL, word);
              break;

            case '~':
              Pl_Stream_Putc('~', pstm);
              break;

            case 'N':
              if (pstm->line_pos == 0)
                break;
            case 'n':
              do
                Pl_Stream_Putc('\n', pstm);
              while (--n > 0);
              break;

            case '?':
              if (format[1])
                *top_stack++ = format + 1;

              format = Arg_Atom(&lst_adr);
              continue;

            default:
              Pl_Err_Domain(pl_domain_format_control_sequence,
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
