/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : extract_asm.c                                                   *
 * Descr.: utility to write inline assembly code for mappers               *
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_FCT 512

#define MAX_ASM_INST_PER_FCT  1024
#define MAX_LABEL_PER_FCT     1024



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int label;
  char code_op[32];
  char args[256];
}AsmLine;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char buff[4096];
char buff1[4096];

int nb_fct;
char *fct[MAX_FCT];
int found[MAX_FCT];

int disassemble = 0;

AsmLine line[MAX_ASM_INST_PER_FCT];
int nb_line;

char *lab[MAX_LABEL_PER_FCT];
int nb_lab;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Needs_Quote(char *str);

char *Read_Line(char *buff, int size, FILE *f_in);

void Gen_Inline(FILE *f_in, FILE *f_out, int nb_fct, char *fct[]);

void Emit_Fct(int fct_no, char *fct_name, FILE *f_in, FILE *f_out);

char *Get_Label(char *str);

int Detect_End_Of_Fct(char *buff);




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])

{
  char *asm_file = NULL;
  char *c_file = NULL;
  char *out_file = NULL;
  char *flags = "-O3 -fomit-frame-pointer";
  FILE *f, *f_in, *f_out;
  int tmp = 0;
  int  i, r;
  int p_open = 1;


  *buff1 = '\0';

  for(i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-c") == 0)
	{
	  c_file = argv[++i];
	  continue;
	}

      if (strcmp(argv[i], "-C") == 0)
	{
	  flags = argv[++i];
	  continue;
	}

      if (strcmp(argv[i], "-a") == 0)
	{
	  asm_file = argv[++i];
	  continue;
	}

      if (strcmp(argv[i], "-o") == 0)
	{
	  out_file = argv[++i];
	  continue;
	}

      if (strcmp(argv[i], "-d") == 0)
	{
	  disassemble = 1;
	  continue;
	}

      if (strcmp(argv[i], "-c") == 0)
	{
	  disassemble = 0;
	  continue;
	}

      if (strcmp(argv[i], "-f") == 0)
	{
	  if ((f = fopen(argv[++i], "r")) == NULL)
	    {
	      perror(argv[i]);
	      goto error;
	    }
	  while(fscanf(f, "%s", buff) == 1)
	    fct[nb_fct++] = strdup(buff);
	  continue;
	}

      if (strcmp(argv[i], "-i") == 0)
	{
	  sprintf(buff1 + strlen(buff1), " -e 's!^.*%s.*$!IGN!'", argv[++i]);
	  continue;
	}

      if (strcmp(argv[i], "-e") == 0)
	{
	  sprintf(buff1 + strlen(buff1), " -e 's!^.*%s.*$!END!'", argv[++i]);
	  continue;
	}

      if (strcmp(argv[i], "-h") == 0)
	{
	  printf("Usage: extract_asm [OPTION | FCT_NAME...]\n");
	  printf("  -c FILE   specify C file to compile to assembly\n");
	  printf("  -C FLAGS  specify C compiler flags\n");
	  printf("  -a FILE   specify assembly file (input or output if -c)\n");
	  printf("  -o FILE   specify output file (else stdout)\n");
	  printf("  -f FILE   get the list of functions from FILE\n");
	  printf("  -d        simply disassemble\n");
	  printf("  -c        create C data for asm inlining\n");
	  printf("  -i RE     ignore lines containing RE\n");
	  printf("  -e RE     end a function when a line containing RE is read\n");
	  printf("  -h        print this help and exit\n");
	  return 0;
	}

      if (*argv[i] == '-')
	{
	  fprintf(stderr, "unrecognized option %s\n", argv[i]);
	  goto error;
	}

      fct[nb_fct++] = argv[i];
    }

  if (nb_fct == 0 && (c_file == NULL || asm_file == NULL))
    {
      fprintf(stderr, "nothing to do - try extract_asm -h for help\n");
      goto error;
    }

  if (asm_file == NULL && c_file == NULL)
    {
      fprintf(stderr, "either -c or -a option should be used\n");
      goto error;
    }

  if (c_file)
    {
      if (asm_file)
	printf("generate asm file %s from C file %s\n", asm_file, c_file);

      if (asm_file == NULL)
	{
	  if ((asm_file = tmpnam(NULL)) == NULL)
	    {
	      perror("cannot create tmp file name\n");
	      goto error;
	    }
	  else
	    tmp = 1;
	}

      sprintf(buff, "gplc -c -C '%s -S' -o %s %s", flags, asm_file, c_file);

      r = system(buff);
      if (r == -1 || r == 127)
	{
	  fprintf(stderr, "cannot execute %s\n", buff);
	  goto error;
	}

      if (r != 0)
	{
	  fprintf(stderr, "command: %s\n\treturned error code; %d\n", buff, r);
	  goto error;
	}
    }

  if (*buff1)
    {
      sprintf(buff, "sed %s %s", buff1, asm_file);
      f_in = popen(buff, "r"), p_open = 1;
    }
  else
    f_in = fopen(asm_file, "r");

  if (f_in == NULL)
    {
      perror(asm_file);
      goto error;
    }

  if (out_file == NULL)
    f_out = stdout;
  else
    if ((f_out = fopen(out_file, "w")) == NULL)
      {
	perror(out_file);
	goto error;
      }


  fprintf(f_out, "/* command:");
  for(i = 0; i < argc; i++)
    if (Needs_Quote(argv[i]))
      fprintf(f_out, " '%s'", argv[i]);
    else
      fprintf(f_out, " %s", argv[i]);
  fprintf(f_out, " */\n\n");


  Gen_Inline(f_in, f_out, nb_fct, fct);

  if (tmp)
    unlink(asm_file);
  if (p_open)
    pclose(f_in);
  else
    fclose(f_in);
  if (f_out != stdout)
    fclose(f_out);
  return 0;

 error:
  if (tmp)
    unlink(asm_file);
  return 1;
}




/*-------------------------------------------------------------------------*
 * NEEDS_QUOTE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Needs_Quote(char *str)

{
  while(*str)
    {
      if (!isalnum(*str) && strchr("-_./", *str) == NULL)
	return 1;
      str++;
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * READ_LINE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Read_Line(char *buff, int size, FILE *f_in)
{
  char *p;

  do
    if (fgets(buff, size, f_in) == NULL)
      return NULL;
  while(strncmp(buff, "IGN", 3) == 0);

  for(p = buff+ strlen(buff) - 1; isspace(*p); p--)
	    ;
  p[1] = '\0';

  return buff;
}




/*-------------------------------------------------------------------------*
 * GEN_INLINE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Gen_Inline(FILE *f_in, FILE *f_out, int nb_fct, char *fct[])

{
  char *p;
  static char fct_name[1024];
  int i, fct_no = 0;

  while ((Read_Line(buff, sizeof(buff), f_in)) != NULL)
    {
      p = Get_Label(buff);
      if (p == NULL)
	continue;

      strcpy(fct_name, p);
#if 0
      printf("start line: %s -- fct name (%s)\n", buff, fct_name);
#endif
      for(i = 0; i < nb_fct; i++)
	if (strcmp(fct_name, fct[i]) == 0 ||
	    (*fct_name == '_' && strcmp(fct_name + 1, fct[i]) == 0))
	    break;

      if (i == nb_fct || found[i])
	continue;

#if 0
      printf("corresponds to fct %d\n",i);
#endif

      Emit_Fct(fct_no++, fct[i], f_in, f_out);
      found[i] = 1;
      if (fct_no == nb_fct)
	return;
    }

  for(i = 0; i < nb_fct; i++)
    if (!found[i])
      fprintf(stderr, "cannot find entry code of %s\n", fct[i]);
}



/*-------------------------------------------------------------------------*
 * EMIT_FCT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Fct(int fct_no, char *fct_name, FILE *f_in, FILE *f_out)

{
  char *p;
  char *start, *end;
  int i, l, eof;
  int inline_level, inline_info;

  nb_line = 0;
  nb_lab = 0;

  while ((Read_Line(buff, sizeof(buff), f_in)) != NULL)
    {
      eof = Detect_End_Of_Fct(buff);
      if (eof == 1)
	break;

      if (nb_line >= MAX_ASM_INST_PER_FCT)
	{
	  fprintf(stderr, "function %s has more than %d asm lines\n", fct_name,
		  MAX_ASM_INST_PER_FCT);
	  exit(1);
	}

      p = Get_Label(buff);
      if (p)
	{
	  if (nb_lab >= MAX_LABEL_PER_FCT)
	    {
	      fprintf(stderr, "function %s has more than %d labes\n", fct_name,
		      MAX_LABEL_PER_FCT);
	      exit(1);
	    }

	  line[nb_line].label = 1;
	  sprintf(line[nb_line].code_op, "%d", nb_lab + 1);
	  strcpy(line[nb_line].args, p);
	  lab[nb_lab++] = line[nb_line].args;
	}
      else
	{
	  line[nb_line].label = 0;
	  for(p = buff; isspace(*p); p++)
	    ;

	  start = p;
	  while(*p && !isspace(*p))
	    p++;
	  end = p;

	  strncpy(line[nb_line].code_op, start, end - start);
	  line[nb_line].code_op[end - start] = '\0';

	  while(isspace(*p))
	    p++;

	  strcpy(line[nb_line].args, p);
	}

      nb_line++;

      if (eof == 2)
	break;
    }

				/* pass 2 : code emission */
  if (disassemble)
    fprintf(f_out, "%s:\n", fct_name);
  else
    {
      if (fct_no == 0)
	fprintf(f_out, "char *inline_asm_data[] = {\n");
      else
	fprintf(f_out, "\n");

      inline_level = 1;
      inline_info = 1;
      fprintf(f_out, "  \"%s\", INL_NEXT, INL_LEVEL(%d), INL_INFO(%d),\n",
	      fct_name, inline_level, inline_info);
    }

  if (disassemble)
    {
      for(i = 0; i < nb_line; i++)
	{
	  if (line[i].label)
	    fprintf(f_out, "%s:\n", line[i].args);
	  else
	    fprintf(f_out, "\t%s\t%s\n", line[i].code_op, line[i].args);
	}
    }
  else
    {
      for(i = 0; i < nb_line; i++)
	{
	  if (line[i].label)
	    fprintf(f_out, "  INL_LABEL(%s),\n", line[i].code_op);
	  else
	    {
	      for(l = 0; l < nb_lab; l++)
		if (strcmp(lab[l], line[i].args) == 0)
		  break;

	      fprintf(f_out, "      \"%s\", ", line[i].code_op);
	      if (l < nb_lab)
		fprintf(f_out, "INL_LABEL(%d),\n", l + 1);
	      else
		fprintf(f_out, "\"%s\",\n", line[i].args);
	    }
	}
      fprintf(f_out, "  INL_END_FUNC,\n");
    }

  if (fct_no == nb_fct - 1 && !disassemble)
    fprintf(f_out, "\n  NULL };\n");
}




/*-------------------------------------------------------------------------*
 * GET_LABEL                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Get_Label(char *str)
{
  if (isspace(*buff))
    return NULL;

  while(isalnum(*str) || strchr(".$_", *str))
    str++;

  if (*str != ':' || str[1] != '\0')
    return NULL;

  *str = '\0';
  return buff;
}




/*-------------------------------------------------------------------------*
 * DETECT_END_OF_FCT                                                       *
 *                                                                         *
 * returns 0 if not the end, 1 if the end, 2 if the last                   *
 *-------------------------------------------------------------------------*/
int
Detect_End_Of_Fct(char *buff)

{
  if (strncmp(buff, "END", 3) == 0)
    return 1;

#if defined(__i386__)

  if (strncmp(buff, ".Lfe", 4) == 0)
    return 1;

#elif defined(__alpha__)

  if (strncmp(buff, "\t.end", 5) == 0)
    return 1;

#elif defined(__sparc__)

  if (strncmp(buff, "\trestore", 8) == 0)
    return 2;

#else
  {
    static int i=0;
    if (i == 0)
      {
	fprintf(stderr, "warning, Detect_End_Of_Fct() not customized"
		"for this architecture\n)");
	i++;
      }
  }
#endif
  return 0;
}

