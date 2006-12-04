/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Utiliy                                                          *
 * File  : hexfilter.c                                                     *
 * Descr.: Prolog hexadecimal decoding filter                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2006 Daniel Diaz                                     *
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
#include <stdarg.h>

#include "../EnginePl/gp_config.h"

#include "decode_hexa.c"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_ARGS                   1024

#define HEXGPLC_VERSION            "1.0"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

int encode = 0;
int strict = 1;
int enclose = 1;
int decode_aux = 0;
int cmd_line = 0;
char *format = NULL;
FILE *fin = NULL;

char *arg[MAX_ARGS];
int nb_arg = 0;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void One_File(FILE *f);

void One_Line(char *str);

void Fatal_Error(char *format, ...);

void Parse_Arguments(int argc, char *argv[]);

void Display_Help(void);



#define Check_Arg(i, str)       (strncmp(argv[i], str, strlen(argv[i])) == 0)




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  FILE *f;
  int i;


  Parse_Arguments(argc, argv);
  if (nb_arg == 0)
    {
      if (cmd_line)
	Fatal_Error("command-line is empty");

      One_File(stdin);
      return 0;
    }

  for (i = 0; i < nb_arg; i++)
    {
      if (cmd_line)
	{
	  One_Line(arg[i]);
	  putchar('\n');
	  continue;
	}

      if ((f = fopen(arg[i], "rt")) == NULL)
	Fatal_Error("cannot open %s", arg[i]);

      One_File(f);
      fclose(f);
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * ONE_FILE                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
One_File(FILE *f)
{
  static char buff[4096];

  for (;;)
    {
      if (fgets(buff, sizeof(buff), f) == NULL)
	break;

      One_Line(buff);
    }
}




/*-------------------------------------------------------------------------*
 * ONE_LINE                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
One_Line(char *str)
{
  int n;
  char *p;

#define Is_Sep(c) (c == '\0' || isspace(c) || strchr("'\"", c))

  if (encode)
    {
      while (*str)
	{
	  if (Is_Sep(*str))
	    {
	      putchar(*str++);
	      continue;
	    }

	  putchar('X');
	  while (*str && !Is_Sep(*str))
	    {
	      if (*str == '/')
		{
		  n = strtol(str + 1, &p, 10);
		  if (n >= 0 && n < 1024 && Is_Sep(*p))
		    {
		      str = p;
		      printf("_%d", n);
		      break;
		    }
		}
	      printf("%2X", (unsigned) (unsigned char) *str);
	      str++;
	    }
	}
      return;
    }

  fputs(Decode_Hexa(str, format, strict, decode_aux), stdout);
}




/*-------------------------------------------------------------------------*
 * PARSE_ARGUMENTS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Parse_Arguments(int argc, char *argv[])
{
  int i;

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (Check_Arg(i, "--encode"))
	    {
	      encode = 1;
	      continue;
	    }

	  if (Check_Arg(i, "--relax"))
	    {
	      strict = 0;
	      continue;
	    }

	  if (Check_Arg(i, "--printf"))
	    {
	      if (++i >= argc)
		Fatal_Error("format missing after -printf option");

	      format = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--aux-father"))
	    {
	      decode_aux = 1;
	      continue;
	    }

	  if (Check_Arg(i, "--aux-father2"))
	    {
	      decode_aux = 2;
	      continue;
	    }

	  if (Check_Arg(i, "--cmd-line"))
	    {
	      cmd_line = 1;
	      continue;
	    }

	  if (Check_Arg(i, "-H"))
	    {
	      encode = 1;
	      cmd_line = 1;
	      strict = 0;
	      continue;
	    }

	  if (Check_Arg(i, "-P"))
	    {
	      encode = 0;
	      cmd_line = 1;
	      strict = 0;
	      decode_aux = 0;
	      continue;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      fprintf(stderr, "Prolog/Hexa Filter                    ");
	      fprintf(stderr, "                    Daniel Diaz - 1998\n");
	      fprintf(stderr, "%s version %s\n", HEXGPLC, HEXGPLC_VERSION);
	      exit(0);
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }

	  Fatal_Error("unknown option %s - try %s --help", argv[i],
		      HEXGPLC);
	}

      arg[nb_arg++] = argv[i];
    }
}




/*-------------------------------------------------------------------------*
 * FATAL_ERROR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fatal_Error(char *format, ...)
{
  va_list arg_ptr;

  va_start(arg_ptr, format);
  vfprintf(stderr, format, arg_ptr);
  va_end(arg_ptr);

  fprintf(stderr, "\n");
  exit(1);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Help(void)
#define L(msg)  fprintf(stderr, "%s\n", msg)
{
  fprintf(stderr, "Usage: %s [OPTION]... [FILE...]", HEXGPLC);
  L(" ");
  L("Options:");
  L("  --encode                    encoding mode (default is decoding)");
  L("  --relax                     decode also predicate names (not only predicate indicators)");
  L("  --printf FORMAT             pass encoded/decoded string to printf with FORMAT");
  L("  --aux-father                decode auxiliary predicate as its father");
  L("  --aux-father2               decode auxiliary predicate as its father + auxiliary number");
  L("  --cmd-line                  command-line mode: encode/decode each argument of the command-line");
  L("  -H                          shortcut for --cmd-line --encode");
  L("  -P                          shortcut for --cmd-line --relax");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
}
