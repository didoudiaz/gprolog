/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : terminal.c                                                      *
 * Descr.: basic terminal operations                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>

#include "../EnginePl/gp_config.h"

#if defined(__unix__) || defined(__CYGWIN__)

#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/uio.h>


#if defined(HAVE_SYS_IOCTL_COMPAT_H)
#include <sys/ioctl_compat.h>
#endif

#if defined(HAVE_TERMIOS_H)
#include <termios.h>
typedef struct termios TermIO;

#define Gtty(fd, s) tcgetattr(fd, s)
#define Stty(fd, s) tcsetattr(fd, TCSANOW, s)
#else
#include <termio.h>
typedef struct termio TermIO;

#define Gtty(fd, s) ioctl(fd, TCGETA, s)
#define Stty(fd, s) ioctl(fd, TCSETA, s)
#endif /* !HAVE_TERMIOS_H */

#elif defined(_WIN32)

#include <windows.h>
#include <io.h>
#include <conio.h>

#endif


#define LE_DEFINE_HOOK_MACROS

#define TERMINAL_FILE

#include "terminal.h"
#include "linedit.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int use_gui = 1;
static int use_ansi = 1;
static int fd_in = 0;		/* not changed */
static int fd_out = -1;

#if defined(__unix__) || defined(__CYGWIN__)

static int is_tty_in;
static int is_tty_out;
static TermIO old_stty_in;
static TermIO new_stty_in;
static TermIO old_stty_out;
static TermIO new_stty_out;

static int nb_rows, nb_cols;
static int pos;

#elif defined(_WIN32)

static HANDLE h_stdin;
static HANDLE h_stdout;
static DWORD im;

#endif

static int interrupt_key;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Parse_Env_Var(void);

#if defined(__unix__) || defined(__CYGWIN__)

static void Choose_Fd_Out(void);

static void Set_TTY_Mode(TermIO *old, TermIO *new);

#endif

static int LE_Get_Char0(void);

static void Backd(int n);

static void Forwd(int n, char *str);

static void Displ(int n, char *str);

static void Displ_Str(char *s);

static void Erase(int n);




/*-------------------------------------------------------------------------*
 * LE_INITIALIZE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
LE_Initialize(void)
{
  static int initialized = 0;
  static int le_hook_present = 0; /* ie. gui is present */

  if (initialized)
    return le_hook_present;

  initialized = 1;

  Parse_Env_Var();

#if defined(__unix__) || defined(__CYGWIN__)
  Choose_Fd_Out();
#endif

  if (le_hook_start && use_gui == 1)
    (*le_hook_start) ();

  if (le_hook_put_char != NULL && le_hook_get_char0 != NULL
      && le_hook_kbd_is_not_empty != NULL && le_hook_screen_size != NULL)
    le_hook_present = 1;
  else
    {
      le_hook_put_char = NULL;
      le_hook_get_char0 = NULL;
      le_hook_kbd_is_not_empty = NULL;
      le_hook_screen_size = NULL;
    }

#define INIT_FCT(hook, def) if (hook == NULL) hook = def

  /* inside terminal.c */
  INIT_FCT(le_hook_screen_size, LE_Screen_Size);
  INIT_FCT(le_hook_kbd_is_not_empty, LE_Kbd_Is_Not_Empty);
  INIT_FCT(le_hook_put_char, LE_Put_Char);
  INIT_FCT(le_hook_get_char0, LE_Get_Char0);
  INIT_FCT(le_hook_ins_mode, LE_Ins_Mode);
  INIT_FCT(le_hook_emit_beep, LE_Emit_Beep);

  /* inside linedit.c */
  INIT_FCT(le_hook_backd, Backd);
  INIT_FCT(le_hook_forwd, Forwd);
  INIT_FCT(le_hook_displ, Displ);
  INIT_FCT(le_hook_erase, Erase);
  INIT_FCT(le_hook_displ_str, Displ_Str);


#if defined(__unix__) || defined(__CYGWIN__)

#elif defined(_WIN32)

  if (le_hook_put_char == LE_Put_Char)	/* DOS console mode */
    {
      h_stdin = GetStdHandle(STD_INPUT_HANDLE);
      h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    }

  interrupt_key = KEY_CTRL('C');	/* WIN32: interrupt = CTRL+C */

#endif

  return le_hook_present;
}




/*-------------------------------------------------------------------------*
 * PARSE_ENV_VAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parse_Env_Var(void)
{
  char *p = getenv("LINEDIT");

  if (p == NULL)
    return;

  if (strstr(p, "gui=no") != NULL)
    use_gui = 0;
      
  if (strstr(p, "ansi=no") != NULL)
    use_ansi = 0;

  if ((p = strstr(p, "out=")) != NULL)
    {
      p += 4;

      if (isdigit(*p))
	fd_out = strtol(p, NULL, 10);
      else
	{
	  char buff[1024];
	  char *q = buff;

	  while(*p && isprint(*p) && !isspace(*p))
	    *q++ = *p++;

	  *q = '\0';
	  fd_out = open(buff, O_WRONLY); /* on error fd_out = -1 */
	}
    }
}




#if defined(__unix__) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * CHOOSE_FD_OUT                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Choose_Fd_Out(void)
{
  int fd[3] = { 1, 0, 2 };	/* order fd list to try to find a tty */
  int i, try;
  int mask;
  char *p;

  for(i = 0; i < 3 && fd_out < 0; i++)
    {
      try = fd[i];

      if (!isatty(try))
	continue;

      mask = fcntl(try, F_GETFL);
      if ((mask & O_WRONLY) == O_WRONLY || (mask & O_RDWR) == O_RDWR)
	{
	  fd_out = try;
	  break;
	}

      if ((p = ttyname(try)) != NULL)
	fd_out = open(p, O_WRONLY);
    }

  if (fd_out < 0)
    fd_out = 1;

}
#endif


/*-------------------------------------------------------------------------*
 * LE_OPEN_TERMINAL                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Open_Terminal(void)
{
  fflush(stdout);
  fflush(stderr);


#if defined(__unix__) || defined(__CYGWIN__) /* Mode cbreak (raw mode) */
  is_tty_in = !Gtty(fd_in, &old_stty_in);
  is_tty_out = !Gtty(fd_out, &old_stty_out);

  if (is_tty_in)
    {
      interrupt_key = old_stty_in.c_cc[VINTR];
      Set_TTY_Mode(&old_stty_in, &new_stty_in);
      Stty(fd_in, &new_stty_in);
    }
  else
    interrupt_key = KEY_CTRL('C');

  if (is_tty_out)
    {
      Set_TTY_Mode(&old_stty_out, &new_stty_out);
      Stty(fd_out, &new_stty_out);
    }

  LE_Screen_Size(&nb_rows, &nb_cols);

  pos = 0;

#elif defined(_WIN32)

  if (le_hook_put_char == LE_Put_Char)	/* DOS console mode */
    {
      h_stdin = GetStdHandle(STD_INPUT_HANDLE);
      h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
      GetConsoleMode(h_stdin, &im);
      SetConsoleMode(h_stdin, im & ~ENABLE_PROCESSED_INPUT);
    }

  interrupt_key = KEY_CTRL('C');	/* WIN32: interrupt = CTRL+C */

#endif
}




/*-------------------------------------------------------------------------*
 * LE_CLOSE_TERMINAL                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Close_Terminal(void)
{
#if defined(__unix__) || defined(__CYGWIN__) /* Initial mode (cooked mode) */

  if (is_tty_in)
    Stty(fd_in, &old_stty_in);

  if (is_tty_out)
    Stty(fd_out, &old_stty_out);

#elif defined(_WIN32)

  if (le_hook_put_char == LE_Put_Char)	/* DOS console mode */
    SetConsoleMode(h_stdin, im);

#endif
}




#if defined(__unix__) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * SET_TTY_MODE                                                            *
 *                                                                         *
 * Mode cbreak (raw mode).                                                 *
 *-------------------------------------------------------------------------*/
static void
Set_TTY_Mode(TermIO *old, TermIO *new)
{
  *new = *old;

  new->c_iflag &= ~(INLCR | IGNCR | ICRNL | IXON | IXOFF);
  new->c_oflag = OPOST | ONLCR;
  new->c_lflag &= ~(ICANON | ECHO | ECHONL);

  new->c_cc[VMIN] = 1;		/* MIN # of chars */
  new->c_cc[VTIME] = 1;		/* TIME */

  new->c_cc[VINTR] = -1;	/* deactivate SIGINT signal */
}

#endif




/*-------------------------------------------------------------------------*
 * LE_SCREEN_SIZE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Screen_Size(int *row, int *col)
{
#if defined(__unix__) || defined(__CYGWIN__)
  struct winsize ws;

  if (!is_tty_out)
    {
      row = col = 0;
      return;
    }

  ioctl(fd_out, TIOCGWINSZ, &ws);
  nb_rows = *row = ws.ws_row;
  nb_cols = *col = ws.ws_col;

#elif defined(_WIN32)

  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if (GetConsoleScreenBufferInfo(h_stdout, &csbi))
    {
      *row = csbi.dwSize.Y;
      *col = csbi.dwSize.X;
    }
  else
    {
      *row = 25;
      *col = 80;
    }

#endif
}




/*-------------------------------------------------------------------------*
 * LE_IS_INTERRUPT_KEY                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
LE_Is_Interrupt_Key(int c)
{
  return (c == interrupt_key);
}




/*-------------------------------------------------------------------------*
 * LE_KBD_IS_NOT_EMPTY                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
LE_Kbd_Is_Not_Empty(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

#ifdef FIONREAD
  int nb_not_read;

  ioctl(fd_in, FIONREAD, &nb_not_read);
  return nb_not_read != 0;
#else
  return 0;
#endif

#elif defined(_WIN32)

  return kbhit();

#endif
}




/*-------------------------------------------------------------------------*
 * LE_INS_MODE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Ins_Mode(int ins_mode)
{
#if defined(_WIN32) && !defined(__CYGWIN__)

  CONSOLE_CURSOR_INFO cci;

  if (!GetConsoleCursorInfo(h_stdout, &cci))
    return;

  cci.dwSize = (ins_mode) ? 5 : 50;

  SetConsoleCursorInfo(h_stdout, &cci);

#endif
}




/*-------------------------------------------------------------------------*
 * LE_EMIT_BEEP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Emit_Beep(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

  LE_Put_Char('\a');

#else

  Beep(800, 220);

#endif
}




/*
 * Character I/O
 */


/*-------------------------------------------------------------------------*
 * LE_PUT_CHAR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
LE_Put_Char(int c)
{
#if defined(__unix__) || defined(__CYGWIN__)
  char c0 = c;

  if (use_ansi)
    {
      char buf[20];

      switch(c)
	{
	case '\b':
	  if (pos == 0)
	    {
	      pos = nb_cols - 1;
	      sprintf(buf, "\033[A\033[%dC", pos);
	      write(fd_out, buf, strlen(buf));
	      return;
	    }
	  pos--;
	  break;

	case '\a':
	  break;

	case '\n':
	  pos = 0;
	  break;

	default:
	  if (++pos > nb_cols)
	    pos = 1;
	}
    }

  c0 = c;
  write(fd_out, &c0, 1);

#elif defined(_WIN32)

  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if (c != '\b')
    {
#ifdef WIN32_CONVERT_OEM_ASCII
      unsigned char buff[2];

      buff[0] = c;
      buff[1] = '\0';
      CharToOem(buff, buff);
      c = buff[0];
#endif
      putch(c);
      return;
    }

  GetConsoleScreenBufferInfo(h_stdout, &csbi);

  if (csbi.dwCursorPosition.X == 0)
    {
      csbi.dwCursorPosition.X = csbi.dwSize.X - 1;
      csbi.dwCursorPosition.Y--;
    }
  else
    csbi.dwCursorPosition.X--;

  SetConsoleCursorPosition(h_stdout, csbi.dwCursorPosition);

#endif
}




/*-------------------------------------------------------------------------*
 * LE_GET_CHAR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
LE_Get_Char(void)
{
  int c;

  c = GET_CHAR0;

  if (c == 0x1b)
    {
      int esc_c;

      esc_c = GET_CHAR0;
#if defined(__unix__) || defined(__CYGWIN__)
      if (esc_c == '[' || esc_c == 'O')	/* keyboard ANSI ESC sequence */
	{
	  if ((c = GET_CHAR0) == '[')
	    c = GET_CHAR0;
          if (isdigit(c))
	    {
	      esc_c = c;
	      c = 0;
	      while (esc_c != '~')
		{
		  c = c * 10 + esc_c - '0';
		  esc_c = GET_CHAR0;
		}
	    }
	  c = (1 << 8) | c;
	}
      else
#endif
	c = KEY_ESC(esc_c);
    }

  return c;
}




/*-------------------------------------------------------------------------*
 * LE_GET_CHAR0                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
LE_Get_Char0(void)
{
#if defined(__unix__) || defined(__CYGWIN__)
  unsigned char c;

  if (read(fd_in, &c, 1) != 1)
    return KEY_CTRL('D');
  return (int) c;

#elif defined(_WIN32)

  INPUT_RECORD ir;
  DWORD nb;
  int c;

read_char:
  if (!ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), &ir, 1, &nb))
    return -1;

  switch (ir.EventType)
    {
    case KEY_EVENT:
      if (!ir.Event.KeyEvent.bKeyDown)
	goto read_char;
      c = ir.Event.KeyEvent.uChar.AsciiChar & 0xff;
      if (c == 0 || c == 0xe0)
	{
	  c = ir.Event.KeyEvent.wVirtualKeyCode;
	  if (c < 0x15 || c > 0x87)	/* e.g. CTRL key alone */
	    goto read_char;
	  if (ir.Event.KeyEvent.dwControlKeyState &
	      (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	    c = (2 << 8) | c;
	  else
	    c = (1 << 8) | c;
	}
#ifdef WIN32_CONVERT_OEM_ASCII
      else
	{
	  unsigned char buff[2];

	  buff[0] = c;
	  buff[1] = '\0';
	  OemToChar(buff, buff);
	  c = buff[0];
	}
#endif
      break;

    case MOUSE_EVENT:
    case WINDOW_BUFFER_SIZE_EVENT:
    case MENU_EVENT:
    case FOCUS_EVENT:
      goto read_char;
      break;
    }

  return c;

#endif
}




/*-------------------------------------------------------------------------*
 * BACKD                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Backd(int n)
{
  while (n--)
    PUT_CHAR('\b');
}




/*-------------------------------------------------------------------------*
 * FORWD                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Forwd(int n, char *str)
{
  while (n--)
    PUT_CHAR(*str++);
}




/*-------------------------------------------------------------------------*
 * DISPL                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Displ(int n, char *str)
{
  while (n--)
    PUT_CHAR(*str++);
}




/*-------------------------------------------------------------------------*
 * ERASE                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Erase(int n)
{
  int n0 = n;

  while (n--)
    PUT_CHAR(' ');

  BACKD(n0);
}




/*-------------------------------------------------------------------------*
 * DISPL_STR                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Displ_Str(char *str)
{
  while (*str)
    PUT_CHAR(*str++);
}
