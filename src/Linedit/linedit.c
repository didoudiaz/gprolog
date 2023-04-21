/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : linedit.c                                                       *
 * Descr.: line editor                                                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "../EnginePl/gp_config.h"

#define LE_DEFINE_HOOK_MACROS

#include "terminal.h"
#include "ctrl_c.h"
#include "linedit.h"

#if defined(__unix__) || defined(__CYGWIN__)

#include <unistd.h>
#include <sys/time.h>

#elif defined(_WIN32)

#include <time.h>

#endif


#if 1
#define TREAT_BUFFERED_CHARS    /* treat buffered chars at start (X paste) */
#endif

#if 1
#define NO_DUP_IN_HISTORY       /* do not put in history line == the last */
#endif

#if 1
#define IGNORE_QUOTED_PART      /* ingore quoted item in bracket matching */
#endif

#if 0
#define NO_USE_SELECT           /* no use select(2) for temporisation */
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define LINEDIT_VERSION            "2.6"

#define MAX_HISTORY_LINES          64
#define MAX_SEPARATORS             256

#define NB_TAB_BEFORE_LIST         1

#define DEFAULT_SEPARATORS         " ,;:-'\"!@$#^&()-+*/\\[]|<=>`~{}"

#define NB_MATCH_LINES_BEFORE_ASK  20

#define OPEN_BRACKET               "([{"
#define CLOSE_BRACKET              ")]}"

#ifdef NO_USE_SELECT
#define BRACKET_TIMMING            300000       /* in microseconds */
#else
#define BRACKET_TIMMING            900000       /* in microseconds */
#endif




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int buff_length;
  int line_length;
  char *line;
}
HistCell;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char separators[MAX_SEPARATORS] = DEFAULT_SEPARATORS;
static int ins_mode = 1;

static char *global_str;
static char *global_pos;
static char *global_end;

static int prompt_length;

static PlLong ctrl_c_ret_val;

static char clipboard[4096] = "";

static HistCell hist_tbl[MAX_HISTORY_LINES];
static int hist_start = 0;
static int hist_end = 0;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static bool New_Char(int c, char *str, int size, char **p_pos, char **p_end);

static char *Skip(char *from, char *limit, int res_sep_cmp, int direction);

static bool Is_A_Separator(char c);

static int Search_Bracket(char *brackets, char c);

static int Tab_To_Spaces(int p);



static void History_Add_Line(char *line, int length);

static void History_Update_Line(char *line, int length, int hist_no);

static int History_Get_Line(char *str, int hist_no);



static void Display_All_Completions(ComplMatch *compl_match);

static void Display_Help(void);



#define NEW_LN()  { PUT_CHAR('\n'); }


#define Hist_Inc(n)   { if (++(n) >= MAX_HISTORY_LINES) (n) = 0; }

#define Hist_Dec(n)   { if (--(n) < 0) (n) = MAX_HISTORY_LINES - 1; }

#define Hist_First(n) { (n) = Hist_Start_Entry(); }

#define Hist_Last(n)  { (n) = Hist_End_Entry(); }

#define Hist_Start_Entry()    (hist_start)

#define Hist_End_Entry()      (hist_end)

#define Hist_Is_Empty()       (hist_start == hist_end)


#define RE_DISPLAY_LINE          \
do {                             \
  if (prompt && display_prompt)  \
    DISPL_STR(prompt);           \
                                 \
  DISPL(end - str, str);         \
  BACKD(end - pos);              \
} while(0)




/*-------------------------------------------------------------------------*
 * PL_LE_GETS                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_LE_Gets(char *str)
{
  int l;
  int big_size = ((unsigned) -1) >> 1;


  if ((str = Pl_LE_FGets(str, big_size, NULL, 0)) != NULL)
    {
      l = (int) (strlen(str) - 1); /* for gets remove last \n */
      if (l >= 0 && str[l] == '\n')
        str[l] = '\0';
    }

  return str;
}




/*-------------------------------------------------------------------------*
 * PL_LE_FGETS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_LE_FGets(char *str, int size, char *prompt, int display_prompt)
{
  char *pos = str;
  char *end = str;
  char *mark = NULL;
  char *p, *q, *start, *stop;
  char w;
  int c, n, n1;
  bool last_was_eof = false;
  int h_no = Hist_End_Entry();
  int tab_count = 0;
  int count_bracket[3];
  ComplMatch compl_match;

  Pl_LE_Initialize();

  size--;                       /* -1 for '\0' */

  prompt_length = (prompt && display_prompt) ? (int) strlen(prompt) : 0;

  Pl_LE_Open_Terminal();
  global_str = str;
  global_pos = pos;
  global_end = end;

#ifdef TREAT_BUFFERED_CHARS     /* treat buffered lines (for paste) */
  while (KBD_IS_NOT_EMPTY)
    {
      if (end - str >= size || ((c = Pl_LE_Get_Char()) == '\n') || c == '\r')
        {
          RE_DISPLAY_LINE;
          goto return_is_read;
        }
      if (c == '\t')            /* '\t' on output would cause trouble */
        for(n = Tab_To_Spaces((int) (end - str)); n; n--)
          *end++ = ' ';
      else
        *end++ = c;
    }

  if (end != str)
    {
      pos = end;
      goto re_display_line;
    }
#endif

  if (prompt && display_prompt)
    DISPL_STR(prompt);

  for (;;)
    {
      global_pos = pos;
      global_end = end;

      Debug_Check_Positions(pos - str);

      c = Pl_LE_Get_Char();
    one_char:
      *end = ' ';               /* to allow for separator test */

      if (Pl_LE_Is_Interrupt_Key(c))
        {                       /* save global vars for reentrancy */
          int save_prompt_length = prompt_length;

          FORWD(end - pos, pos); /* go to EOL to avoid multi-line */
          /* truncation on the output */
          Pl_LE_Close_Terminal();
          c = *end;
          *end = '\0';          /* to allow the handler to use/test str */
          if ((ctrl_c_ret_val = Pl_Emit_Ctrl_C()) != 0)
            return (char *) -2;

          Pl_LE_Open_Terminal();
          *end = c;

          global_str = str;     /* restore global vars for reentrancy */
          prompt_length = save_prompt_length;

        re_display_line:        /* display prompt + full line */
          RE_DISPLAY_LINE;
          continue;
        }

      if (KEY_IS_EOF(c))        /* to avoid EOF when typing too much ^D */
        {
          if (end == str)
            {
              if (c == KEY_CTRL('D') && last_was_eof)
                goto error;
              else
                {
                  str = NULL;
                  goto finish;
                }
            }
          last_was_eof = (c == KEY_CTRL('D'));
        }
      else
        last_was_eof = false;

      if (c != '\t')
        tab_count = 0;

      switch (c)
        {
        case KEY_CTRL('A'):     /* go to begin of line */
        case KEY_EXT_HOME:
          BACKD(pos - str);
          pos = str;
          continue;


        case KEY_CTRL('E'):     /* go to end of line */
        case KEY_EXT_END:
          FORWD(end - pos, pos);
          pos = end;
          continue;


        case KEY_CTRL('B'):     /* go to 1 char backward */
        case KEY_EXT_LEFT:
          if (pos == str)
            goto error;
          BACKD(1);
          pos--;
          continue;


        case KEY_CTRL('F'):     /* go to 1 char forward */
        case KEY_EXT_RIGHT:
          if (pos == end)
            goto error;
          FORWD(1, pos);
          pos++;
          continue;


        case KEY_BACKSPACE:     /* erase previous char */
        case KEY_DELETE:
          if (pos == str)
            goto error;
        del_last:
          for (p = pos; p < end; p++)
            p[-1] = *p;
          BACKD(1);
          pos--;
          end--;
          DISPL(end - pos, pos);
          ERASE(1);
          BACKD(end - pos);
          continue;


        case KEY_CTRL('D'):     /* erase current char */
        case KEY_EXT_DELETE:
          if (pos == end)
            goto error;
          /* simply equivalent to ^F + BACKSPACE */
          FORWD(1, pos);
          pos++;
          goto del_last;


        case KEY_CTRL('U'):     /* erase begin of line */
        case KEY_ID2(KEY_MODIF_CTRL, KEY_EXT_HOME):
          q = clipboard;
          p = str;
          while (p < pos)       /* add deleted part to clipboard */
            *q++ = *p++;
          *q = '\0';
          n = (int) (pos - str);
          for (p = pos; p < end; p++)
            p[-n] = *p;

          pos = str;
          end -= n;
          BACKD(n);
          DISPL(end - pos, pos);
          ERASE(n);
          BACKD(end - pos);
          continue;


        case KEY_CTRL('K'):     /* erase end of line */
        case KEY_ID2(KEY_MODIF_CTRL, KEY_EXT_END):
          q = clipboard;
          p = pos;
          while (p < end)       /* add deleted part to clipboard */
            *q++ = *p++;
          *q = '\0';
          ERASE(end - pos);
          end = pos;
          continue;


        case KEY_CTRL('Y'):     /* paste from clipboard */
          for (p = clipboard; *p; p++)
            if (!New_Char(*p, str, size, &pos, &end))
              goto error;
          continue;


        case KEY_CTRL(' '):     /* mark begin selection */
          mark = pos;
          continue;


        case KEY_ESC('W'):      /* copy (from mark) to clipboard */
        case KEY_CTRL('W'):     /* cut (from mark) to clipboard */
          if (mark == NULL)
            goto error;

          if (mark < pos)
            {
              start = mark;
              stop = pos;
            }
          else
            {
              start = pos;
              stop = mark;
            }
          q = clipboard;
          p = start;
          while (p < stop)
            *q++ = *p++;
          *q = '\0';
          if (c == KEY_ESC('W'))
            continue;

          n = (int) (stop - start);
          for (p = stop; p < end; p++)
            p[-n] = *p;

          if (mark < pos)
            BACKD(n);
          pos = start;
          end -= n;
          DISPL(end - pos, pos);
          ERASE(n);
          BACKD(end - pos);
          continue;


        case KEY_ESC('B'):      /* go to previous word */
        case KEY_ID2(KEY_MODIF_CTRL, KEY_EXT_LEFT):
          p = (pos == str) ? pos : pos - 1; /* to avoid start of a word */
          p = Skip(p, str, 1, -1);      /* skip separators */
          p = Skip(p, str, 0, -1);      /* skip non separators */
          p = Skip(p, end, 1, +1);      /* skip separators */
          BACKD(pos - p);
          pos = p;
          continue;


        case KEY_ESC('F'):      /* go to next word */
        case KEY_ID2(KEY_MODIF_CTRL, KEY_EXT_RIGHT):
          p = pos;
          p = Skip(p, end, 0, +1);      /* skip non separators */
          p = Skip(p, end, 1, +1);      /* skip separators */
          FORWD(p - pos, pos);
          pos = p;
          continue;


        case KEY_ESC('C'):      /* capitalize word */
          p = pos;
          p = Skip(p, end, 1, +1);      /* skip separators */
          if (islower(*p))
            *p = *p - 'a' + 'A';
          p = Skip(p, end, 0, +1);      /* skip non separators */
          DISPL(p - pos, pos);
          pos = p;
          continue;


        case KEY_ESC('L'):      /* convert to lower case */
          p = pos;
          p = Skip(p, end, 1, +1);      /* skip separators */
          for (; p < end && !Is_A_Separator(*p); p++)
            *p = tolower(*p);
          DISPL(p - pos, pos);
          pos = p;
          continue;


        case KEY_ESC('U'):      /* convert to upper case */
          p = pos;
          p = Skip(p, end, 1, +1);      /* skip separators */
          for (; p < end && !Is_A_Separator(*p); p++)
            *p = toupper(*p);
          DISPL(p - pos, pos);
          pos = p;
          continue;


        case '\t':              /* TAB: completion */
          if (tab_count != 0)   /* already a TAB */
            {
              if (++tab_count > NB_TAB_BEFORE_LIST)
                {
                  NEW_LN();
                  Display_All_Completions(&compl_match);
                  goto re_display_line;
                }
              goto error;
            }
          p = (pos == str) ? pos : pos - 1; /* to avoid start of a word */
          p = Skip(p, str, 0, -1);      /* skip non separators */
          p = Skip(p, end, 1, +1);      /* skip separators */
	  n = (int) (pos - p);
	  w = *pos;			/* prefix from p to pos */
          *pos = '\0';
 //	  printf("p: %p  pos: %p p[1]: %d  *p:%d\n", p, pos, p[1], *p);
	  Pl_LE_Compl_Match_First(&compl_match, p, n);
	  *pos = w;
	  if (compl_match.nb_match == 0)
            goto error;
 //	  printf("\nmatch prefix: <%s> len: %d   cur_word: <%s>  max_prefix_len: %d (%.*s)\n", p, n, compl_match.cur_word, compl_match.max_prefix_length, compl_match.max_prefix_length, compl_match.cur_word);
	  p = compl_match.cur_word + n;
	  n = compl_match.max_prefix_length - n; /* display rest to complete longest prefix */
          while (n--)
            if (!New_Char(*p++, str, size, &pos, &end))
              goto error;

          if (compl_match.nb_match > 1)
            {
              tab_count = 1;
              goto error;       /* for the beep */
            }
          tab_count = 0;
          continue;


        case KEY_ESC('\t'):     /* transform a tab to spaces */
          for (n = Tab_To_Spaces((int) (pos - str)); n; n--)
            if (!New_Char(' ', str, size, &pos, &end))
              goto error;
          continue;


        case KEY_CTRL('V'):     /* switch insert mode (on/off) */
        case KEY_EXT_INSERT:
          ins_mode = 1 - ins_mode;
          INS_MODE(ins_mode);
          continue;


        case KEY_CTRL('T'):     /* swap last and current char */
          if (pos == str || pos == end)
            goto error;
          w = pos[0];
          pos[0] = pos[-1];
          pos[-1] = w;
          BACKD(1);
          DISPL(2, pos - 1);
          pos++;
          continue;

        case '\n':
        case '\r':
        return_is_read:
          FORWD(end - pos, pos); /* go to EOL to avoid multi-line */
	  /* truncation on the output */
	  *end = '\0';
	  History_Add_Line(str, (int) (end - str));
	  if (end - str < size)   /* '\n' can be added */
	    *end++ = '\n';
	  *end = '\0';
	  goto finish;


        case KEY_CTRL('P'):     /* history: recall previous line */
        case KEY_EXT_UP:
          if (Hist_Is_Empty() || h_no == Hist_Start_Entry())
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
          Hist_Dec(h_no);
        write_hist_line:
          p = end;
          end = str + History_Get_Line(str, h_no);
          BACKD(pos - str);
          DISPL(end - str, str);
          if (end < p)
            ERASE(p - end);
          pos = end;
          continue;


        case KEY_CTRL('N'):     /* history: recall next line */
        case KEY_EXT_DOWN:
          if (Hist_Is_Empty() || h_no == Hist_End_Entry())
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
          Hist_Inc(h_no);
          goto write_hist_line;


        case KEY_ESC('P'):      /* history: recall previous matching line */
         if (Hist_Is_Empty() || pos == str)
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
        try_previous:
          if (h_no == Hist_Start_Entry())
            goto error;
          Hist_Dec(h_no);
          if (hist_tbl[h_no].line == NULL ||
              strncmp(str, hist_tbl[h_no].line, pos - str) != 0)
            goto try_previous;
        write_hist_match_line:
          p = end;
          end = str + History_Get_Line(str, h_no);
          DISPL(end - pos, pos);
          if (end < p)
            ERASE(p - end);
          BACKD(end - pos);
          continue;


        case KEY_ESC('N'):      /* history: recall next matching line */
          if (Hist_Is_Empty() || pos == str)
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
        try_next:
          if (Hist_Is_Empty() || h_no == Hist_End_Entry())
            goto error;
          Hist_Inc(h_no);
          if (hist_tbl[h_no].line == NULL ||
              strncmp(str, hist_tbl[h_no].line, pos - str) != 0)
            goto try_next;
          goto write_hist_match_line;


        case KEY_ESC('<'):      /* history: recall first line */
        case KEY_EXT_PAGE_UP:
          if (Hist_Is_Empty() || h_no == Hist_Start_Entry())
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
          Hist_First(h_no);
          goto write_hist_line;


        case KEY_ESC('>'):      /* history: recall last line */
        case KEY_EXT_PAGE_DOWN:
          if (Hist_Is_Empty() || h_no == Hist_End_Entry())
            goto error;
          *end = '\0';
          History_Update_Line(str, (int) (end - str), h_no);
          Hist_Last(h_no);
          goto write_hist_line;


        case KEY_ESC('?'):      /* display help */
        display_help:
          NEW_LN();
          Display_Help();
          goto re_display_line;


        default:
          if ((unsigned) c > 255 || !isprint(c))
            {
              n = c;
              EMIT_BEEP;
              c = Pl_LE_Get_Char();
              if (c != n)
                goto one_char;
              goto display_help;
            }

          if (!New_Char(c, str, size, &pos, &end))
            goto error;
          /* brackets ([{ }]): matching */
          if (KBD_IS_NOT_EMPTY || (n = Search_Bracket(CLOSE_BRACKET, c)) < 0)
            continue;

          count_bracket[0] = count_bracket[1] = count_bracket[2] = 0;
          count_bracket[n]--;

          p = pos - 1;
          for (; count_bracket[n] != 0;)
            {
              if (--p < str)
                goto bracket_exit;

              c = *p;
              if ((n1 = Search_Bracket(CLOSE_BRACKET, c)) >= 0)
                {
                  count_bracket[n1]--;
                  continue;
                }

              if ((n1 = Search_Bracket(OPEN_BRACKET, c)) >= 0)
                if (++count_bracket[n1] > 0)
                  goto bracket_exit;

#ifdef IGNORE_QUOTED_PART
              if (p > str && (c == '\'' || c == '"') && p[-1] != '\\')
                {               /* ignore quoted part */
                  while (--p > str && (*p != c || p[-1] == '\\'))
                    ;
                }
#endif
            }

          if (KBD_IS_NOT_EMPTY)
            continue;

          n = (int) (pos - p);
          q = pos;
          BACKD(n);
#if defined(_WIN32) && !defined(__CYGWIN__)

          {
            PlLong t0 = clock(), t1;

            do
              t1 = clock();
            while (!KBD_IS_NOT_EMPTY &&
                   (t1 - t0) * 1000000 / CLOCKS_PER_SEC < BRACKET_TIMMING);
          }

#elif !defined(NO_USE_SELECT)
          {
            fd_set set;
            struct timeval t;

            t.tv_sec = 0;
            t.tv_usec = BRACKET_TIMMING;

            FD_ZERO(&set);
            FD_SET(0, &set);
            select(1, &set, NULL, NULL, &t);
          }
#else
          usleep(BRACKET_TIMMING);
#endif
          pos = p;
          FORWD(n, pos);
          pos = q;
        bracket_exit:
          continue;
        }
    error:
      EMIT_BEEP;
    }

 finish:
  NEW_LN();

  Pl_LE_Close_Terminal();

  return str;
}




/*-------------------------------------------------------------------------*
 * NEW_CHAR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static bool
New_Char(int c, char *str, int size, char **p_pos, char **p_end)
{
  char *pos = *p_pos;
  char *end = *p_end;
  char *p;

  if ((ins_mode || pos == end) && end - str >= size)
    return false;

  if (!ins_mode)
    {
      *pos = (char) c;
      if (++pos > end)
        end = pos;
      PUT_CHAR(c);
    }
  else
    {
      for (p = end; p > pos; p--)
        *p = p[-1];

      *pos = (char) c;
      end++;
      DISPL(end - pos, pos);
      pos++;
      BACKD(end - pos);
    }

  *p_pos = pos;
  *p_end = end;

  return true;
}



/*-------------------------------------------------------------------------*
 * PL_LE_GET_PROMPT_LENGTH                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Get_Prompt_Length(void)
{
  return prompt_length;
}




/*-------------------------------------------------------------------------*
 * PL_LE_GET_CURRENT_POSITION                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Get_Current_Position(void)
{
  return (int) (global_pos - global_str);
}




/*-------------------------------------------------------------------------*
 * PL_LE_GET_CURRENT_WORD                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Get_Current_Word(char *word)
{
  char *str = global_str;
  char *pos = global_pos;
  char *end = global_end;
  char *p, *q;

  p = Skip(pos, str, 0, -1);    /* skip non separators */
  if (Is_A_Separator(*p))
    p++;

  q = Skip(pos, end, 0, +1);    /* skip non separators */

  while (p < q)
    *word++ = *p++;

  *word = '\0';
}




/*-------------------------------------------------------------------------*
 * PL_LE_GET_SEPARATORS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_LE_Get_Separators(void)
{
  return separators;
}




/*-------------------------------------------------------------------------*
 * PL_LE_SET_SEPARATORS                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_LE_Set_Separators(char *sep_str)
{
  return strcpy(separators, sep_str);
}




/*-------------------------------------------------------------------------*
 * PL_LE_ADJUST_WORD_FOR_COMPLETION                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_LE_Adjust_For_Completion(char *word)
{	 /* to check word (done in atom.c) */
  char *s;
  while(Is_A_Separator(*word))	/* skip leading separators */
    word++;

  for(s = word; *s; s++)
    if (Is_A_Separator(*s))
      return NULL;

  return word;
}




/*-------------------------------------------------------------------------*
 * PL_LE_GET_CTRL_C_RETURN_VALUE                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_LE_Get_Ctrl_C_Return_Value(void)
{
  return ctrl_c_ret_val;
}




/*-------------------------------------------------------------------------*
 * SKIP                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Skip(char *from, char *limit, int res_sep_cmp, int direction)
{
  while (from != limit)
    {
      if (Is_A_Separator(*from) != res_sep_cmp)
        break;                  /* exit since *from does not satisfy res_sep_cmp */

      from = from + direction;
    }

  return from;
}




/*-------------------------------------------------------------------------*
 * IS_A_SEPARATOR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static bool
Is_A_Separator(char c)
{
  char *p;

  /* like strchr(separators, c) but does not take into account '\0' */
  for (p = separators; *p; p++)
    if (*p == c)
      return true;

  return false;
}




/*-------------------------------------------------------------------------*
 * SEARCH_BRACKET                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Search_Bracket(char *brackets, char c)
{
  int n;

  for(n = 0; brackets[n] != '\0'; n++)
    if (brackets[n] == c)
      return n;

  return -1;
}




/*-------------------------------------------------------------------------*
 * TAB_TO_SPACES                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Tab_To_Spaces(int p)
{
  p += prompt_length;
  p = 8 - (p % 8);
  return p;
}




/*-------------------------------------------------------------------------*
 * HISTORY_ADD_LINE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
History_Add_Line(char *line, int length)
{
  char *p = line;

  while (*p == ' ')
    p++;
  if (*p == '\0')               /* do not add an empty line */
    return;

#ifdef NO_DUP_IN_HISTORY
  if (hist_end > 0 && strcmp(line, hist_tbl[hist_end - 1].line) == 0)
    return;
#endif

  History_Update_Line(line, length, hist_end);

  Hist_Inc(hist_end);
  if (hist_end == hist_start)
    Hist_Inc(hist_start);
}




/*-------------------------------------------------------------------------*
 * HISTORY_UPDATE_LINE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
History_Update_Line(char *line, int length, int hist_no)
{
  HistCell *h;

  h = hist_tbl + hist_no;

  if (h->line != NULL && h->buff_length < length)
    {
      free(h->line);
      h->line = NULL;           /* to ensure future malloc */
    }

  if (h->line == NULL)          /* not yet allocated */
    {
      if ((h->line = (char *) malloc(length + 1)) == NULL)
        exit(1);
      h->buff_length = length;
    }

  strcpy(h->line, line);
  h->line_length = length;
}




/*-------------------------------------------------------------------------*
 * HISTORY_GET_LINE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
History_Get_Line(char *str, int hist_no)
{
  HistCell *h = hist_tbl + hist_no;

  strcpy(str, h->line);

  return h->line_length;
}




/*-------------------------------------------------------------------------*
 * DISPLAY_ALL_COMPLETIONS                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_All_Completions(ComplMatch *compl_match)
{
  int nb_match = compl_match->nb_match;
  int max_word_length = compl_match->max_word_length;
  int row, col;
  int nb_cols, nb_lines;
  int space_between;
  char buff[64];
  int l, c;


  SCREEN_SIZE(&row, &col);

#define MIN_CHARS_BETWEEN_WORDS 1

  /* col - 1 to avoid to fully fill a line (keep at least 1 char after last word) */
  nb_cols = (col - 1 + MIN_CHARS_BETWEEN_WORDS) / (max_word_length + MIN_CHARS_BETWEEN_WORDS); 
  if (nb_cols < 1)
    nb_cols = 1;
  
  nb_lines = (nb_match + nb_cols - 1) / nb_cols;

#if 0
  printf("rows: %d  cols: %d   maxlg:%d  nb cols: %d  width:%d  nb lines: %d\n",
	 row, col, max_word_length, nb_cols,
	 nb_cols * max_word_length + (nb_cols - 1) * MIN_CHARS_BETWEEN_WORDS,
	 nb_lines);
#endif

#if 1	      /* fixed nb of chars between columns */
  space_between = MIN_CHARS_BETWEEN_WORDS;
#else	     /* spread the display on all the width of the screen (cols chars) */
  space_between = ((col - 1) - (nb_cols * max_word_length)) / (nb_cols - 1);
#endif

  if (nb_lines > NB_MATCH_LINES_BEFORE_ASK)     /* too many matchings ? */
    {
      sprintf(buff, "Show all %d possibilities (y/n) ? ", nb_match);
      DISPL_STR(buff);
      c = Pl_LE_Get_Char();
      NEW_LN();
      if (c != 'y' && c != 'Y')
        return;
    }

  for(l = 0; l < nb_lines; l++)
    {
      /* no space after the last word, so display <space of previous word><curr word> */
      int space_prev = 0;
      for(c = 0; c < nb_cols; c++)
	{
	  int no = c * nb_lines + l;
	  if (Pl_LE_Compl_Match_Goto(compl_match, no))
	    {
	      while(space_prev--)
		PUT_CHAR(' ');
	      DISPL_STR(compl_match->cur_word);
	      space_prev = max_word_length - compl_match->cur_word_length + space_between;
	    }
	}
      NEW_LN();
    }
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_Help(void)
#define L(msg)     DISPL_STR(msg); NEW_LN()
{
  char buff[80];

  L("");
  sprintf(buff,
          "   linedit %-25s Copyright (C) 1999-2023 Daniel Diaz",
          LINEDIT_VERSION);
  L(buff);
  L("");
  L("                              Moving");
  L("   Ctl-B   previous char             Ctl-F   next char");
  L("   Esc-B   previous word             Esc-F   next word");
  L("   Ctl-A   begin of line             Ctl-E   end of line");
  L("");
  L("                             Deleting");
  L("   Ctl-U   delete begin of line      Ctl-K   delete end of line");
  L("   Ctl-H   delete previous char      Ctl-D   delete current char");
  L("");
  L("                             Changing");
  L("   Esc-L   downcase word             Esc-U   upcase word");
  L("   Esc-C   capitalize word           Ctl-T   reverse last two chars");
  L("");
  L("                             History");
  L("   Esc-<   first line                Esc->   last line");
  L("   Ctl-P   previous line             Ctl-N   next line");
  L("   Esc-P   previous matching line    Esc-N   next matching line");
  L("");
  L("                             Selection");
  L("   Ctl-spc mark selection            Ctl-W   cut  selection");
  L("   Esc-W   copy selection            Ctl-Y   past selection");
  L("");
  L("                           Miscellaneous");
  L("   Ctl-V   insert mode switch        Ctl-I   completion (twice = all)");
  L("   Esc-?   display this help       Esc-Ctl-I insert spaces for tab");
  L("");
}

#undef L




/*-------------------------------------------------------------------------*
 * PL_LE_GET_KEY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Get_Key(int echo, int catch_ctrl_c)
{
  int c;

  Pl_LE_Initialize();
  prompt_length = 0;

start:
  Pl_LE_Open_Terminal();

  c = Pl_LE_Get_Char();
  if (catch_ctrl_c && Pl_LE_Is_Interrupt_Key(c))
    {
      Pl_LE_Close_Terminal();
      if ((ctrl_c_ret_val = Pl_Emit_Ctrl_C()) != 0)
        return -2;
      goto start;
    }

  if (KEY_IS_EOF(c))
    c = EOF;

  if (echo && (unsigned) c <= 255 && isprint(c))
    PUT_CHAR(c);

  Pl_LE_Close_Terminal();

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_LE_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Printf(char *format, ...)
{
  va_list arg_ptr;
  static char buff[65535];
  int ret;

  Pl_LE_Initialize();

  va_start(arg_ptr, format);
  ret = vsprintf(buff, format, arg_ptr);
  DISPL_STR(buff);
  va_end(arg_ptr);

  return ret;
}
