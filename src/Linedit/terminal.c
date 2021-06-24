/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : terminal.c                                                      *
 * Descr.: basic terminal operations                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <errno.h>

#include "../EnginePl/gp_config.h"

#if defined(__unix__) || defined(__CYGWIN__)

#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/stat.h>

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


/* Interesting infos: The Open Group Base Specifications Issue 7
 * Chapter 11. General Terminal Interface
 * https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap11.html#tag_11
 */


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int use_linedit;
static int use_gui;
static int use_ansi;
#if defined(__unix__) || defined(__CYGWIN__)
static int fd_in = 0;           /* not changed */
#endif
static int fd_out = -1;
static FILE *file_dbg = NULL;	/* activate DEBUG and set LINEDIT env var dbg=/dev/ttyxxx */

#if defined(__unix__) || defined(__CYGWIN__)

static int is_tty_in;
static int is_tty_out;
static int same_tty;
static TermIO old_stty_in;
static TermIO new_stty_in;
static TermIO old_stty_out;
static TermIO new_stty_out;

static int nb_rows, nb_cols;
static int term_pos;

#elif defined(_WIN32)

static HANDLE h_stdin;
static HANDLE h_stdout;
static DWORD im;

static int code_page = 0;
static int oem_put = 1;
static int oem_get = 1;

#endif

static int interrupt_key;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Parse_Env_Var(void);

#if defined(__unix__) || defined(__CYGWIN__)

static int Same_File(int fd1, int fd2);

static void Choose_Fd_Out(void);

static void Set_TTY_Mode(TermIO *old, TermIO *new);

static void Install_Resize_Handler();

static void Resize_Handler();

#endif

static int LE_Get_Char0(void);

static void Backd(int n);

static void Forwd(int n, char *str);

static void Displ(int n, char *str);

static void Displ_Str(char *s);

static void Erase(int n);




/*-------------------------------------------------------------------------*
 * PL_LE_INITIALIZE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Initialize(void)
{
  static int initialized = 0;
  static int le_mode;

  if (initialized)
    return le_mode;

  initialized = 1;
  
  Parse_Env_Var();

  if (!use_linedit)
    return (le_mode = LE_MODE_DEACTIVATED);

   le_mode = LE_MODE_TTY;	/* default */


#if defined(__unix__) || defined(__CYGWIN__)
  Choose_Fd_Out();
#endif

  if (pl_le_hook_start && use_gui)
    (*pl_le_hook_start) (use_gui == 2);

  if (pl_le_hook_put_char != NULL && pl_le_hook_get_char0 != NULL
      && pl_le_hook_kbd_is_not_empty != NULL && pl_le_hook_screen_size != NULL)
    le_mode = LE_MODE_HOOK;
  else
    {
      pl_le_hook_put_char = NULL;
      pl_le_hook_get_char0 = NULL;
      pl_le_hook_kbd_is_not_empty = NULL;
      pl_le_hook_screen_size = NULL;
    }

#define INIT_FCT(hook, def) if (hook == NULL) hook = def

  /* inside terminal.c */
  INIT_FCT(pl_le_hook_screen_size, Pl_LE_Screen_Size);
  INIT_FCT(pl_le_hook_kbd_is_not_empty, Pl_LE_Kbd_Is_Not_Empty);
  INIT_FCT(pl_le_hook_put_char, Pl_LE_Put_Char);
  INIT_FCT(pl_le_hook_get_char0, LE_Get_Char0);
  INIT_FCT(pl_le_hook_ins_mode, Pl_LE_Ins_Mode);
  INIT_FCT(pl_le_hook_emit_beep, Pl_LE_Emit_Beep);

  /* inside linedit.c */
  INIT_FCT(pl_le_hook_backd, Backd);
  INIT_FCT(pl_le_hook_forwd, Forwd);
  INIT_FCT(pl_le_hook_displ, Displ);
  INIT_FCT(pl_le_hook_erase, Erase);
  INIT_FCT(pl_le_hook_displ_str, Displ_Str);


#if defined(__unix__) || defined(__CYGWIN__)

#elif defined(_WIN32)

  if (pl_le_hook_put_char == Pl_LE_Put_Char)    /* DOS console mode */
    {
      h_stdin = GetStdHandle(STD_INPUT_HANDLE);
      h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    }

  interrupt_key = KEY_CTRL('C');        /* WIN32: interrupt = CTRL+C */

#endif

  return le_mode;
}




/*-------------------------------------------------------------------------*
 * PARSE_ENV_VAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Parse_Env_Var(void)
{
  char *p, *q, *r;
  char buff[1024];

  use_linedit = use_gui = use_ansi = 1; /* default */

  p = getenv("LINEDIT");
  if (p == NULL)
    return;

  if (strncmp(p, "no", 2) == 0)	/* deactivate linedit */
    {
      use_linedit = 0;
      return;
    }

  if (strstr(p, "gui=n") != NULL)
    use_gui = 0;

  if (strstr(p, "gui=s") != NULL) /* silent */
    use_gui = 2;

  if (strstr(p, "ansi=n") != NULL)
    use_ansi = 0;

#ifdef _WIN32
  if ((q = strstr(p, "cp=")) != NULL && isdigit(q[3]))
    code_page = strtol(q + 3, NULL, 10);

  if (strstr(p, "oem_put=n") != NULL)
    oem_put = 0;

  if (strstr(p, "oem_put=y") != NULL)
    oem_put = 1;

  if (strstr(p, "oem_get=n") != NULL)
    oem_get = 0;

  if (strstr(p, "oem_get=y") != NULL)
    oem_get = 1;
#endif

  if ((q = strstr(p, "out=")) != NULL)
    {
      q += 4;

      if (isdigit(*p))
        fd_out = strtol(p, NULL, 10);
      else
        {
	  r = buff;
          while(*q && isprint(*q) && !isspace(*q))
            *q++ = *r++;

          *r = '\0';
          fd_out = open(buff, O_WRONLY); /* on error fd_out = -1 */
        }
    }

  if ((q = strstr(p, "dbg=")) != NULL)
    {
      q += 4;

      r = buff;
      while(*q && isprint(*q) && !isspace(*q))
	*r++ = *q++;

      *r = '\0';
      file_dbg = fopen(buff, "wt");
    }

  return;
}




#ifdef DEBUG
/*-------------------------------------------------------------------------*
 * DEBUG_PRINTF                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Debug_Printf(char *fmt, ...)
{
  va_list arg_ptr;

  if (file_dbg == NULL)
    return;

  va_start(arg_ptr, fmt);

  vfprintf(file_dbg, fmt, arg_ptr);

  va_end(arg_ptr);
  fputc('\n', file_dbg);
  fflush(file_dbg);
}




/*-------------------------------------------------------------------------*
 * DEBUG_CHECK_POSITIONS                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void Debug_Check_Positions(int lin_pos)
{
#if defined(__unix__) || defined(__CYGWIN__)

  int l = Pl_LE_Get_Prompt_Length();
  if ((lin_pos + l) % nb_cols != term_pos)
    Debug_Printf("********* ERROR lin_pos %d  + prompt_len %d) %% nb_cols %d = %d != term_pos %d\n",
		 lin_pos, l, nb_cols, (lin_pos + l) % nb_cols, term_pos);

#endif
}
#endif




/*-------------------------------------------------------------------------*
 * PL_LE_OPEN_TERMINAL                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Open_Terminal(void)
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

  Pl_LE_Screen_Size(&nb_rows, &nb_cols);

  same_tty = Same_File(fd_in, fd_out);

  Debug_Printf("Initial size: %dx%d\n", nb_cols, nb_rows);
  Debug_Printf("Same TTY: %d\n", same_tty);

  Install_Resize_Handler();

  term_pos = 0;

#elif defined(_WIN32)

  if (pl_le_hook_put_char == Pl_LE_Put_Char)    /* DOS console mode */
    {
      h_stdin = GetStdHandle(STD_INPUT_HANDLE);
      h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
      GetConsoleMode(h_stdin, &im);
      SetConsoleMode(h_stdin, im & ~ENABLE_PROCESSED_INPUT);
      if (code_page && (!SetConsoleCP(code_page) || !SetConsoleOutputCP(code_page)))
	printf("warning: Setting console code page to %d failed (error: %d)\n", code_page, (int) GetLastError());
    }

  interrupt_key = KEY_CTRL('C');        /* WIN32: interrupt = CTRL+C */

#endif
}




/*-------------------------------------------------------------------------*
 * PL_LE_CLOSE_TERMINAL                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Close_Terminal(void)
{
#if defined(__unix__) || defined(__CYGWIN__) /* Initial mode (cooked mode) */

  if (is_tty_in)
    Stty(fd_in, &old_stty_in);

  if (is_tty_out)
    Stty(fd_out, &old_stty_out);

#elif defined(_WIN32)

  if (pl_le_hook_put_char == Pl_LE_Put_Char)    /* DOS console mode */
    SetConsoleMode(h_stdin, im);

#endif
}




#if defined(__unix__) || defined(__CYGWIN__)

/*-------------------------------------------------------------------------*
 * CHOOSE_FD_OUT                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Choose_Fd_Out(void)
{
  int fd[3] = { 1, 0, 2 };      /* order fd list to try to find a tty */
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
	{
	  fd_out = open(p, O_WRONLY); /* could be O_RDWR to read ansi seq answer from terminal, e.g. read cursor position */
	  Debug_Printf("found a tty %d name: %s  fd: %d\n", try, p, fd_out);
	  break;
	}
    }

  if (fd_out < 0)
    fd_out = 1;
}




/*-------------------------------------------------------------------------*
 * SAME_FILE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Same_File(int fd1, int fd2)
{
  struct stat stat1, stat2;

  return (fstat(fd1, &stat1) != -1 && fstat(fd2, &stat2) != -1 &&
    stat1.st_dev == stat2.st_dev && stat1.st_ino == stat2.st_ino);
}




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

  new->c_cc[VMIN] = 1;          /* MIN # of chars */
  new->c_cc[VTIME] = 1;         /* TIME */

  new->c_cc[VINTR] = -1;        /* deactivate SIGINT signal */

#if 0 /* TODO compare our settings with BSD raw mode */
  new->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
  new->c_oflag &= ~OPOST;
  new->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  new->c_cflag &= ~(CSIZE | PARENB);
  new->c_cflag |= CS8;
#endif
}




/*-------------------------------------------------------------------------*
 * INSTALL_RESIZE_HANDLER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Install_Resize_Handler()
{
#if defined(HAVE_WORKING_SIGACTION) || defined(M_solaris) || defined(M_sco)

  struct sigaction act;
  act.sa_sigaction = (void (*)()) Resize_Handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_RESTART;
  sigaction(SIGWINCH, &act, NULL);

#else

  signal(SIGWINCH, (void (*)()) Resize_Handler);

#endif
}




/*-------------------------------------------------------------------------*
 * RESIZE_HANDLER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Resize_Handler()
{
  int r, c;
  Pl_LE_Screen_Size(&r, &c);
  if (c > 0 && r > 0 && (r != nb_rows || c != nb_cols))
    {
      Debug_Printf("Resize: %dx%d\n", c, r);
      nb_rows = r;
      nb_cols = c;
      term_pos = (Pl_LE_Get_Current_Position() + Pl_LE_Get_Prompt_Length()) % nb_cols;
    }
}

#endif /* defined(__unix__) || defined(__CYGWIN__) */




/*-------------------------------------------------------------------------*
 * PL_LE_SCREEN_SIZE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Screen_Size(int *row, int *col)
{
#if defined(__unix__) || defined(__CYGWIN__)
  struct winsize ws;

  if (!is_tty_out)
    {
      *row = *col = 0;
      return;
    }
  
  ws.ws_row = ws.ws_col = 0;	/* default (error) values */
  if (ioctl(fd_out, TIOCGWINSZ, &ws) == -1 || ws.ws_row == 0 || ws.ws_col == 0)
    {				/* under lldb ioctl fails (workaround use: process launch -tty) but here we ask /dev/tty */
      int fd = open("/dev/tty", O_RDONLY);
      if (fd != -1)
	{
	  ioctl(fd, TIOCGWINSZ, &ws);
	  close (fd);
	}
    }
  *row = ws.ws_row;
  *col = ws.ws_col;

#elif defined(_WIN32)

  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if (GetConsoleScreenBufferInfo(h_stdout, &csbi))
    {
      *row = csbi.dwSize.Y;
      *col = csbi.dwSize.X;
      /* alternative: *col = csbi.srWindow.Bottom - csbi.srWindow.Top + 1; */
    }
  else
    {
      *row = 25;
      *col = 80;
    }

#endif
}




/*-------------------------------------------------------------------------*
 * PL_LE_IS_INTERRUPT_KEY                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Is_Interrupt_Key(int c)
{
  return (c == interrupt_key);
}




/*-------------------------------------------------------------------------*
 * PL_LE_KBD_IS_NOT_EMPTY                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Kbd_Is_Not_Empty(void)
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
 * PL_LE_INS_MODE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Ins_Mode(int ins_mode)
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
 * PL_LE_EMIT_BEEP                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Emit_Beep(void)
{
#if defined(__unix__) || defined(__CYGWIN__)

  Pl_LE_Put_Char('\a');

#else

  Beep(800, 220);

#endif
}




/*
 * Character I/O
 */



/*-------------------------------------------------------------------------*
 * PL_LE_PUT_CHAR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Put_Char(int c)
{
#if defined(__unix__) || defined(__CYGWIN__)
  static char buf[20];
  int n;

  /* recognize \a \n \b and normal characters
   * \b sends a \b to the terminal. But it cannot wrap around backward
   * we thus use a term_pos to know if we need to go up one line "by hand"
   * NB: term_pos is incremented on each char, so it counts the prompt length
   *
   * We also use it at the end of a line to force the cursor on the beginning
   * of the next line (see remark below).
   *
   * In any case, term_pos is not stricly necessary and can be computed from
   * the current pos in linedit. This is what is done in Resize_Handler.
   */

  
  buf[0] = c;
  buf[1] = '\0';

  if (use_ansi)
    {
      /* If stdin/stdout use different tty, SIGWINCH only catch the main tty resizes.
       * In that case, we here systematically check if size changed.
       */
      if (!same_tty)
	Resize_Handler();

      switch(c)
        {
        case '\b':
          if (term_pos == 0)
            {
	      Debug_Printf("\n++++++ UP\n");
              term_pos = nb_cols - 1;
              sprintf(buf, "\033[A\033[%dC", term_pos); /* cursor at end of previous line */
            }
	  else 
	    term_pos--;
          break;

        case '\a':
          break;

        case '\n':
          term_pos = 0;
          break;

        default:
	  /* On some terminals, when displaying the last character of a line, 
	   * the cursor remains on the last column (instead of advancing to 
	   * beginning of next line) and only does a nl on the next character.
	   * So, add a space + \b if on last column to force newline
	   * (we do not add \n since when resizing the screen the newlines stay there)
	   */
	  if (++term_pos >= nb_cols && nb_cols > 0)
	    { 
	      Debug_Printf("\n====== ADD SPACE TO FORCE NEWLINE after char:%c  pos:%d\n", c, term_pos);
	      buf[1] = ' ';
	      buf[2] = '\b';
	      buf[3] = '\0';
	      term_pos = 0;	/* like a \n */
	    }
        }
    }
 
  
  n = strlen(buf);
  if (write(fd_out, buf, strlen(buf)) != n)
    {
      /* loop if errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? */
      Debug_Printf("ERROR trying to write on fd: %d, errno: %d\n", fd_out, errno);
    }

#elif defined(_WIN32)

  CONSOLE_SCREEN_BUFFER_INFO csbi;

  if (c != '\b')
    {
      if (oem_put)
	{
	  char buff[2];

	  buff[0] = c;
	  buff[1] = '\0';
	  CharToOem(buff, buff);
	  c = buff[0];
	}

#if 0
      putch(c);
#else  /* replacement of putch() same but a bit faster */
      {
	DWORD nb;
	char c0 = (char) c;

	WriteConsole(h_stdout, &c0, 1, &nb, NULL);
      }
#endif
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
 * PL_LE_GET_CHAR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Get_Char(void)
{
  int c = GET_CHAR0;

  if (c == 0x1b)
    {
      int esc_c = GET_CHAR0;

#if defined(__unix__) || defined(__CYGWIN__)
      int modif = 0;
      int double_bracket = 0;
      int number[2] = {0, 0};
      int idx_number = 0;

      if (esc_c == 0x1b)		/* CYGWIN ESC CSI ... = ALT modif + CSI ... */
	{
	  modif = 2;		/* ALT */
	  esc_c = GET_CHAR0;
	}

      if (esc_c == '[' || esc_c == 'O') /* keyboard ANSI ESC sequence (CSI or SS3) */
        {
          if ((esc_c = GET_CHAR0) == '[') /* CYGWIN ESC [ [ A = F1 ... ESC [ [ E= F5 */
	    {
	      esc_c = GET_CHAR0;
	      double_bracket = 1;
	    }
          while(isdigit(esc_c) || esc_c == ';')
            {
	      if (esc_c == ';')
		idx_number = 1 - idx_number;
	      else
		number[idx_number] = number[idx_number] * 10 + esc_c - '0';

	      esc_c = GET_CHAR0;
            }

	  c = number[0];
	  if (number[1])
	    modif |= (number[1] - 1);

	  if (isupper(esc_c))
	    {
	      if (double_bracket)
		c = esc_c - 'A' + 11;	/* CYGWIN F1 ..F5 = CSI [ A .. CSI [ E map to 11-15 */
	      else if (esc_c >= 'P') 	/* ANSY F1 .. F4 SS3 P .. SS3 Q map to 11-15*/
		c = esc_c - 'P' + 11;
	      else
		c = esc_c;
	    }
	  else if (esc_c == '^')   	/* CYGWIN CTRL + F1 .. F12 = CSI 11 ^ .. CSI 24 ^ */
	    {
	      modif |= KEY_MODIF_CTRL;		/* CTRL */
	    }
	  else if (esc_c == '$') 	/* CYGWIN: shift+F11 = CSI 23 $  shift+F12 = CSI 24 $ */
	    {
	      modif |= KEY_MODIF_SHIFT;
	    }

	  if (c == 1) 	/* CYGWIN: Home = CSI 1 ~  End = CSI 4 ~ */
	    c = 'H';
	  else if (c == 4)
	    c = 'F';
	  else if (c >= 25 && c <= 36) /* CYGWIN: shift+F1 = F11, shift+F2=F12 shift+F3=25 */
	    {
	      c = c - ((c <= 26 || c == 29) ? 12 : 13);
	      modif |= KEY_MODIF_SHIFT;
	    }


          c = KEY_ID2(modif, c);
#if 0
	  Debug_Printf("\n++++ key id: %d (%x)  modif: %d  end char: %c n[0]=%d  n[1]=%d\n", c, c, modif, esc_c, number[0], number[1]);
#endif
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

#if 0
  {
    char s[32];
    if (isprint(c))
      s[0] = c, s[1] = '\0';
    else if (c == 27)
      strcpy(s, (c == 27) ? "ESC": "???");
      
    Debug_Printf("char0: %d %s\n", c, s);
  }
#endif
  return (int) c;

#elif defined(_WIN32)

  INPUT_RECORD ir;
  DWORD nb;
  int modif = 0;
  int c;

 read_char:
  if (!ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), &ir, 1, &nb) || nb != 1)
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
          if (c < 0x15 || c > 0x87)     /* e.g. CTRL key alone */
            goto read_char;

          if (ir.Event.KeyEvent.dwControlKeyState & (SHIFT_PRESSED))
            modif |= KEY_MODIF_SHIFT;

          if (ir.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED))
            modif |= KEY_MODIF_ALT;

          if (ir.Event.KeyEvent.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
            modif |= KEY_MODIF_CTRL;

	  c = KEY_ID2(modif, c);
        }
      else if (oem_get)
        {
	  char buff[2];
	  
          buff[0] = c;
          buff[1] = '\0';
          OemToChar(buff, buff);
          c = buff[0];
        }
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
