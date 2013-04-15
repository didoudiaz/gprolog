/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : configuration                                                   *
 * File  : pl_config.c                                                     *
 * Descr.: C Compiler options and WAM Configuration                        *
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
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "gp_config.h"
#include "machine.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define STR_LENGTH                 512

#define FILE_WAM_ARCHI_DEF         "wam_archi.def"
#define FILE_WAM_ARCHI_H           "wam_archi.h"

#define FILE_WAM_REGS_H            "wam_regs.h"

#define FILE_WAM_STACKS_H          "wam_stacks.h"

#define FILE_GPROLOG_CST_H         "gprolog_cst.h"



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  char *mach_reg_name;
  char *pl_reg_name;
  char *type;
}
UsedMachRegInf;


typedef struct
{
  char type[32];
  char name[32];
}
RegInf;



typedef enum
{
  SHORT_UNS,
  LONG_INT,
  ADDRESS
}
TypTag;


typedef struct
{
  char name[32];
  TypTag type;
  int value;
}
TagInf;




typedef struct
{
  char name[32];
  char desc[64];
  int def_size;
  char top_macro[128];
}
StackInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

UsedMachRegInf used_mach_reg[256];
int nb_of_used_mach_regs;

char save_str[STR_LENGTH];
FILE *fw_r;
FILE *fw_s;
FILE *fg_c;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Generate_Archi(void);

void Write_C_Compiler_Info(void);

void Write_GProlog_Cst(void);

char *Read_Identifier(char *s, int fail_if_error, char **end);

char *Read_String(char *s, char **end);

int Pl_Read_Integer(char *s, char **end);

void Generate_Regs(FILE *f, FILE *g);

void Generate_Tags(FILE *f, FILE *g);

void Generate_Stacks(FILE *f, FILE *g);

void Pl_Fatal_Error(char *format, ...);




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(void)
{
  int i;

  if (*M_CPU == '?')
    {
      printf("*** This architecture is not supported ***\n");
      return 1;
    }


  if (WORD_SIZE != (sizeof(PlLong) * 8))
    {
      printf("Bad Value of WORD_SIZE - should be %d\n",
             (int) (sizeof(PlLong) * 8));

      return 1;
    }

  if ((fw_r = fopen(FILE_WAM_REGS_H, "wb")) == NULL)
    Pl_Fatal_Error("cannot open %s", FILE_WAM_REGS_H);

  fprintf(fw_r, "/* this file is automatically generated by pl_config.c */\n");
  fprintf(fw_r, "\n#include \"gp_config.h\"\n\n");


  if ((fw_s = fopen(FILE_WAM_STACKS_H, "wb")) == NULL)
    Pl_Fatal_Error("cannot open %s", FILE_WAM_STACKS_H);

  fprintf(fw_s, "/* this file is automatically generated by pl_config.c */\n");

  if ((fg_c = fopen(FILE_GPROLOG_CST_H, "wb")) == NULL)
    Pl_Fatal_Error("cannot open %s", FILE_GPROLOG_CST_H);
#if 0
  fprintf(fg_c, "/* this file is automatically generated by pl_config.c */\n");
#endif

  printf("\n");
  printf("\t-------------------------------\n");
  printf("\t--- GNU PROLOG INSTALLATION ---\n");
  printf("\t-------------------------------\n\n");
  printf("GNU Prolog version: %s (%s)\n", PROLOG_VERSION, PROLOG_DATE);
  printf("Operating system  : %s\n", M_OS);
  printf("Processor         : %s\n", M_CPU);
  printf("Size of a WAM word: %d bits\n", (int) WORD_SIZE);
  printf("C compiler        : %s\n", CC);
  printf("C flags           : %s\n", CFLAGS);
  printf("C flags machine   : %s\n", CFLAGS_MACHINE);
  printf("Assembler         : %s\n", AS);
  printf("Assembler flags   : %s\n", ASFLAGS);
  printf("Loader flags      : %s\n", LDFLAGS);
  printf("Loader libraries  : %s\n", LDLIBS);
  printf("Use line editor   : %s\n",
#ifndef NO_USE_LINEDIT
         "Yes"
#else
         "No"
#endif
    );

  printf("Use piped consult : %s\n",
#ifndef NO_USE_PIPED_STDIN_FOR_CONSULT
         "Yes"
#else
         "No"
#endif
    );

#ifdef _WIN32
  printf("Use GUI console   : %s\n",
#ifdef W32_GUI_CONSOLE
         "Yes"
#else
         "No"
#endif
    );
#endif

#ifdef W32_GUI_CONSOLE
  printf("Use HtmlHelp      : %s\n",
#if defined(WITH_HTMLHELP) && WITH_HTMLHELP == 1
         "Yes (statically linked)"
#elif defined(WITH_HTMLHELP) && WITH_HTMLHELP == 2
         "Yes (dynamically loaded)"
#else
         "No"
#endif
    );
#endif

  printf("Use sockets       : %s\n",
#ifndef NO_USE_SOCKETS
         "Yes"
#else
         "No"
#endif
    );

  printf("Use FD solver     : %s\n",
#ifndef NO_USE_FD_SOLVER
         "Yes"
#else
         "No"
#endif
    );

#ifdef COULD_COMPILE_FOR_FC
  printf("Use fast call     : %s\n",
#ifndef NO_USE_FAST_CALL
         "Yes"
#else
         "No"
#endif
    );
#endif

  printf("Use machine regs. : %s\n",
#ifndef NO_USE_REGS
         "Yes"
#else
         "No"
#endif
    );

  Write_GProlog_Cst();

  Generate_Archi();

  Write_C_Compiler_Info();


#if 0
  fprintf(fg_c, "/* end of automatically generated part */\n");
#endif

  fclose(fw_r);
  fclose(fw_s);
  fclose(fg_c);

  printf("Used register(s)  : ");

  for (i = 0; i < nb_of_used_mach_regs; i++)
    printf("%s (%s)  ", used_mach_reg[i].mach_reg_name, used_mach_reg[i].pl_reg_name);
  printf("\n");


  printf("\n");
  printf("\t------------------------------\n\n");

  return 0;
}




/*-------------------------------------------------------------------------*
 * WRITE_GPROLOG_CST                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_GProlog_Cst(void)
{
  int major, minor, patch_level;

  sscanf(PROLOG_VERSION, "%d.%d.%d", &major, &minor, &patch_level);

  fputc('\n', fg_c);

  fprintf(fg_c, "#define __GNU_PROLOG__        \t%d\n", major);
  fprintf(fg_c, "#define __GPROLOG__           \t%d\n", major);
  fprintf(fg_c, "#define __GPROLOG_MINOR__     \t%d\n", minor);
  fprintf(fg_c, "#define __GPROLOG_PATCHLEVEL__\t%d\n", patch_level);
  fprintf(fg_c, "#define __GPROLOG_VERSION__   \t%d\n",
          major * 10000 + minor * 100 + patch_level);

  fputc('\n', fg_c);

  fprintf(fg_c, "#define PROLOG_DIALECT  \t\"" PROLOG_DIALECT "\"\n");
  fprintf(fg_c, "#define PROLOG_NAME     \t\"" PROLOG_NAME "\"\n");
  fprintf(fg_c, "#define PROLOG_VERSION  \t\"" PROLOG_VERSION "\"\n");
  fprintf(fg_c, "#define PROLOG_DATE     \t\"" PROLOG_DATE "\"\n");
  fprintf(fg_c, "#define PROLOG_COPYRIGHT\t\"" PROLOG_COPYRIGHT "\"\n");

  fputc('\n', fg_c);
}



/*-------------------------------------------------------------------------*
 * WRITE_C_COMPILER_INFO                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_C_Compiler_Info(void)
{
  int i;

  fputc('\n', fw_r);
  fprintf(fw_r, "#define CFLAGS_REGS\t\t\"");
  for (i = 0; i < nb_of_used_mach_regs; i++)
    {
      fprintf(fw_r, CFLAGS_PREFIX_REG, used_mach_reg[i].mach_reg_name);
      fputc(' ', fw_r);
    }

  fputs("\"\n", fw_r);
}




/*-------------------------------------------------------------------------*
 * GENERATE_ARCHI                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Generate_Archi(void)
{
  FILE *f, *g;
  static char str[STR_LENGTH];
  char *p1, *p2;

  if ((f = fopen(FILE_WAM_ARCHI_DEF, "rt")) == NULL)
    Pl_Fatal_Error("cannot open %s", FILE_WAM_ARCHI_DEF);

  if ((g = fopen(FILE_WAM_ARCHI_H, "wb")) == NULL)
    Pl_Fatal_Error("cannot open %s", FILE_WAM_ARCHI_H);

  while (!feof(f) && fgets(str, sizeof(str), f))
    {
      if (*str != '@')
        {
          fputs(str, g);
          continue;
        }

      strcpy(save_str, str);
      p1 = Read_Identifier(str + 1, 1, &p2);

      if (strcmp(p1, "begin") != 0)
        Pl_Fatal_Error("Syntax error: incorrect @ declaration in: %s",
                    save_str);

      p1 = Read_Identifier(p2 + 1, 1, &p2);

      if (strcmp(p1, "regs") == 0)
        {
          Generate_Regs(f, g);
          continue;
        }

      if (strcmp(p1, "tags") == 0)
        {
          Generate_Tags(f, g);
          continue;
        }

      if (strcmp(p1, "stacks") == 0)
        {
          Generate_Stacks(f, g);
          continue;
        }


      Pl_Fatal_Error("Syntax error: unknown section in: %s", save_str);
    }

  fclose(f);
  fclose(g);
}




/*-------------------------------------------------------------------------*
 * READ_IDENTIFIER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Read_Identifier(char *s, int fail_if_error, char **end)
{
  while (isspace(*s))
    s++;

  *end = s;
  if (!isalpha(**end))
    {
      if (fail_if_error)
        Pl_Fatal_Error("Syntax error: identifier expected in: %s", save_str);
      else
        return NULL;
    }

  do
    (*end)++;
  while (isalnum(**end) || **end == '_');

  if (!isspace(**end))
    Pl_Fatal_Error("Syntax error: space expected after identifier in: %s",
                save_str);

  **end = '\0';

  return s;
}




/*-------------------------------------------------------------------------*
 * READ_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Read_String(char *s, char **end)
{
  while (isspace(*s))
    s++;

  if (*s != '"')
    Pl_Fatal_Error("Syntax error: string expected in: %s", save_str);

  for(*end = s + 1; **end != '"'; (*end)++)
    if (**end == '\0')
      Pl_Fatal_Error("Syntax error: string expected in: %s", save_str);

  (*end)++;
  **end = '\0';

  return s;                     /* NB: returned string contains double-quotes */
}




/*-------------------------------------------------------------------------*
 * PL_READ_INTEGER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Read_Integer(char *s, char **end)
{
  int x = 0;

  while (isspace(*s))
    s++;

  *end = s;
  if (!isdigit(**end))
    Pl_Fatal_Error("Syntax error: integer expected in: %s", save_str);

  do
    {
      x = x * 10 + **end - '0';
      (*end)++;
    }
  while (isdigit(**end) || **end == '_');

  if (!isspace(**end))
    Pl_Fatal_Error("Syntax error: space expected after identifier in: %s",
                save_str);
  **end = '\0';
  return x;
}




/*-------------------------------------------------------------------------*
 * GENERATE_REGS                                                           *
 *                                                                         *
 * initial filler description                                              *
 *    @filler size                                                         *
 *                                                                         *
 * register description:                                                   *
 *    @reg priority type name                                              *
 *         priority: 1-9 (1:high, 9:low)                                   *
 *         type must be machine word castable (ex int unsigned pointer...) *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Generate_Regs(FILE *f, FILE *g)
{
  char *p1, *p2;
  static char str[STR_LENGTH];
  char str_base[32] = "";
  char *used_regs[] = M_USED_REGS;
  char **p = used_regs;
  RegInf reg[10][50];
  int nb_reg[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  RegInf *dp;
  int total_nb_reg = 0;
  int nb_not_alloc = 0;
  int regs_to_save_for_signal;
  int i, j, k;

#ifdef NO_USE_REGS
  p[0] = NULL;
#endif

  for (;;)
    {
      if (feof(f) || !fgets(str, sizeof(str), f))
        Pl_Fatal_Error("Syntax error: end expected for @begin reg");

      if (*str != '@')
        {
          if (*str != '\n')
            fputs(str, g);
          continue;
        }

      strcpy(save_str, str);
      p1 = Read_Identifier(str + 1, 1, &p2);

      if (strcmp(p1, "end") == 0)
        break;

      if (strcmp(p1, "filler") == 0)
        {
          p1 = Read_Identifier(p2 + 1, 0, &p2);
          if (!p1)
            {
              i = Pl_Read_Integer(p2, &p2);
              p1 = str;
              sprintf(p1, "%d", i);
            }

          sprintf(str_base + strlen(str_base), "%s+", p1);
          continue;
        }

      if (strcmp(p1, "reg") == 0)
        {
          i = Pl_Read_Integer(p2 + 1, &p2);
          strcpy(reg[i][nb_reg[i]].type, Read_Identifier(p2 + 1, 1, &p2));
          strcpy(reg[i][nb_reg[i]].name, Read_Identifier(p2 + 1, 1, &p2));
          nb_reg[i]++;
          continue;
        }

      Pl_Fatal_Error("Syntax error: incorrect @ declaration in: %s", save_str);
    }




  fprintf(g, "\n\n   /*--- Begin Register Generation ---*/\n\n");

#ifndef NO_MACHINE_REG_FOR_REG_BANK
  if (*p)
    {
      used_mach_reg[nb_of_used_mach_regs].mach_reg_name = *p;
      used_mach_reg[nb_of_used_mach_regs].pl_reg_name = "pl_reg_bank";
      used_mach_reg[nb_of_used_mach_regs].type = "WamWordP";
      nb_of_used_mach_regs++;
      fprintf(g, "register WamWord \t\t*pl_reg_bank asm (\"%s\");\n\n", *p);
      fprintf(fw_r, "#define MAP_REG_BANK\t\t\"%s\"\n\n", *p);
      p++;
    }
  else
    {
      fprintf(g, "#ifdef ENGINE_FILE\n\n");
      fprintf(g, "       WamWord \t\t\t*pl_reg_bank;\n");
      fprintf(g, "\n#else\n\n");
      fprintf(g, "extern WamWord \t\t\t*pl_reg_bank;\n");
      fprintf(g, "\n#endif\n\n");
    }
#endif

  for (i = 0; i < 10; i++)
    for (j = 0, total_nb_reg += nb_reg[i]; j < nb_reg[i]; j++)
      {
        dp = &reg[i][j];
        if (*p)
          {
            used_mach_reg[nb_of_used_mach_regs].mach_reg_name = *p;
            used_mach_reg[nb_of_used_mach_regs].pl_reg_name = dp->name;
            used_mach_reg[nb_of_used_mach_regs].type = dp->type;
            nb_of_used_mach_regs++;
            fprintf(g, "register %s\t\t%-3s asm (\"%s\");\n",
                    dp->type, dp->name, *p);
            fprintf(fw_r, "#define MAP_REG_%-10s\t\"%s\"\n", dp->name,
                    *p++);
            if (!*p)
              fprintf(g, "\n\n");
          }
        else
          {
            fprintf(g, "#define %s\t\t\t(((%-8s *) pl_reg_bank)[%s%d])\n",
                    dp->name, dp->type, str_base, nb_not_alloc);
            fprintf(fw_r, "#define MAP_OFFSET_%-6s\t((%s%d)*%d)\n",
                    dp->name, str_base, nb_not_alloc++, (int) sizeof(PlLong));
          }
      }


  fprintf(g, "\n\n");
  fprintf(g, "#define NB_OF_REGS          \t%d\n", total_nb_reg);
  fprintf(g, "#define NB_OF_ALLOC_REGS    \t%d\n", total_nb_reg - nb_not_alloc);
  fprintf(g, "#define NB_OF_NOT_ALLOC_REGS\t%d\n", nb_not_alloc);
  fprintf(g, "#define REG_BANK_SIZE       \t(%sNB_OF_NOT_ALLOC_REGS)\n",
          str_base);
  fprintf(g, "\n\n\n\n#define NB_OF_USED_MACHINE_REGS %d\n",
          nb_of_used_mach_regs);        /* same as NB_OF_ALLOC_REGS :-) ? */


#ifndef NO_MACHINE_REG_FOR_REG_BANK /* pl_reg_bank restored anyway */
  regs_to_save_for_signal = (nb_of_used_mach_regs > 1);
#else
  regs_to_save_for_signal = (nb_of_used_mach_regs >= 1);
#endif

  fprintf(g, "\n");
  fprintf(g, "#ifdef ENGINE_FILE\n\n");
#ifdef NO_MACHINE_REG_FOR_REG_BANK
  fprintf(g, "WamWord pl_reg_bank[REG_BANK_SIZE];\n");
#else
  fprintf(g, "WamWord *save_reg_bank;\n\n");
#endif

  if (regs_to_save_for_signal)
    fprintf(g, "WamWord pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS + 1];\n\n");

  fprintf(g, "char *pl_reg_tbl[] = { ");
  k = 0;
  for (i = 0; i < 10; i++)
    for (j = 0; j < nb_reg[i]; j++)
      {
        dp = &reg[i][j];
        fprintf(g, "\"%s\"%s", dp->name,
                k < total_nb_reg - 1 ? ", " : "};\n");
        k++;
      }

  fprintf(g, "\n#else\n\n");
#ifdef NO_MACHINE_REG_FOR_REG_BANK
  fprintf(g, "extern WamWord pl_reg_bank[];\n");
#else
  fprintf(g, "extern WamWord *save_reg_bank;\n\n");
#endif

  if (regs_to_save_for_signal)
    fprintf(g, "extern WamWord pl_buff_signal_reg[];\n\n");

  fprintf(g, "extern char *pl_reg_tbl[];\n");

  fprintf(g, "\n#endif\n\n");

#ifndef NO_MACHINE_REG_FOR_REG_BANK
  fprintf(g, "#define Init_Reg_Bank(x)  save_reg_bank = pl_reg_bank = x\n");
#else
  fprintf(g, "#define Init_Reg_Bank(x)\n");
#endif


  fprintf(g, "\n\n");
  fprintf(g, "#define Reg(i)\t\t\t(");
  k = 0;
  for (i = 0; i < 10; i++)
    for (j = 0; j < nb_reg[i]; j++)
      {
        dp = &reg[i][j];
        if (k < total_nb_reg - 1)
          fprintf(g, "((i)==%d) ? (WamWord) %-3s\t: \\\n\t\t\t\t ",
                  k++, dp->name);
        else
          fprintf(g, "           (WamWord) %s)\n", dp->name);
      }


  fprintf(g, "\n\n\n\n#define Save_All_Regs(buff_save) \\\n");
  fprintf(g, "  do { \\\n");

  k = 0;
  for (i = 0; i < 10; i++)
    for (j = 0; j < nb_reg[i]; j++)
      {
        dp = &reg[i][j];
        fprintf(g, "    buff_save[%d] = (WamWord) %s; \\\n", k,
                dp->name);
        k++;
      }

  fprintf(g, "  } while(0)\n");


  fprintf(g, "\n\n\n\n#define Restore_All_Regs(buff_save) \\\n");
  fprintf(g, "  do { \\\n");

  k = 0;
  for (i = 0; i < 10; i++)
    for (j = 0; j < nb_reg[i]; j++)
      {
        dp = &reg[i][j];
        fprintf(g, "    %-6s = (%-8s) buff_save[%d]; \\\n",
                dp->name, dp->type, k);
        k++;
      }

  fprintf(g, "  } while(0)\n");



  fprintf(g, "\n\n\n\n#define Save_Machine_Regs(buff_save) \\\n");
  fprintf(g, "  do { \\\n");

  for (i = 0; i < nb_of_used_mach_regs; i++)
    fprintf(g, "    buff_save[%d] = (WamWord) %s; \\\n", i, used_mach_reg[i].pl_reg_name);

  fprintf(g, "  } while(0)\n");



  fprintf(g, "\n\n#define Restore_Machine_Regs(buff_save) \\\n");
  fprintf(g, "  do { \\\n");

  for (i = 0; i < nb_of_used_mach_regs; i++)
    fprintf(g, "    %s = (%-8s) buff_save[%d]; \\\n", used_mach_reg[i].pl_reg_name, used_mach_reg[i].type, i);

  fprintf(g, "  } while(0)\n");



  if (regs_to_save_for_signal)
    {
      fprintf(g, "\n\n\n\n#define Start_Protect_Regs_For_Signal \\\n");
      fprintf(g, "  do { \\\n");
      fprintf(g, "    Save_Machine_Regs(pl_buff_signal_reg); \\\n");
      fprintf(g, "    pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 1; \\\n");
      fprintf(g, "  } while(0)\n");

      fprintf(g, "\n\n#define Stop_Protect_Regs_For_Signal \\\n");
      fprintf(g, "  pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 0; \\\n");

      fprintf(g, "\n\n#define Restore_Protect_Regs_For_Signal \\\n");
      fprintf(g, "  do { \\\n");
      fprintf(g, "    if (pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS]) { \\\n");
      fprintf(g, "      Restore_Machine_Regs(pl_buff_signal_reg); \\\n");
      fprintf(g, "      Stop_Protect_Regs_For_Signal; \\\n");
      fprintf(g, "    } \\\n");
#ifndef NO_MACHINE_REG_FOR_REG_BANK
      fprintf(g, "    pl_reg_bank = save_reg_bank; \\\n");
#endif
      fprintf(g, "  } while(0)\n");
    }
  else
    {
      fprintf(g, "\n\n\n\n#define Start_Protect_Regs_For_Signal\n");
      fprintf(g, "\n\n#define Stop_Protect_Regs_For_Signal\n");
      fprintf(g, "\n\n#define Restore_Protect_Regs_For_Signal\n");
    }




  fprintf(g, "\n\n   /*--- End Register Generation ---*/\n\n");
}




/*-------------------------------------------------------------------------*
 * GENERATE_TAGS                                                           *
 *                                                                         *
 * tag description:                                                        *
 *    @tag name type value                                                 *
 *         type: long_int/short_uns/address                                *
 *         value: >= 0                                                     *
 *-------------------------------------------------------------------------*/
void
Generate_Tags(FILE *f, FILE *g)
{
  static char str[STR_LENGTH];
  char *p1, *p2;
  TagInf tag[128];
  int nb_tag = 0;
  int tag_size, tag_size_low, tag_size_high, value_size;
  int max_value = 0;
  PlULong tag_mask;
  PlLong min_integer, max_integer;
  int i;


  for (;;)
    {
      if (feof(f) || !fgets(str, sizeof(str), f))
        Pl_Fatal_Error("Syntax error: end expected for @begin tag");

      if (*str != '@')
        {
          if (*str != '\n')
            fputs(str, g);
          continue;
        }

      strcpy(save_str, str);
      p1 = Read_Identifier(str + 1, 1, &p2);

      if (strcmp(p1, "end") == 0)
        break;

      if (strcmp(p1, "tag") == 0)
        {
          strcpy(tag[nb_tag].name, Read_Identifier(p2 + 1, 1, &p2));
          p1 = Read_Identifier(p2 + 1, 1, &p2);

          if (strcmp(p1, "long_int") == 0)
            tag[nb_tag].type = LONG_INT;
          else if (strcmp(p1, "short_uns") == 0)
            tag[nb_tag].type = SHORT_UNS;
          else if (strcmp(p1, "address") == 0)
            tag[nb_tag].type = ADDRESS;
          else
            Pl_Fatal_Error("Syntax error: wrong tag type in: %s", save_str);

          tag[nb_tag].value = Pl_Read_Integer(p2 + 1, &p2);
          if (tag[nb_tag].value > max_value)
            max_value = tag[nb_tag].value;
          nb_tag++;
          continue;
        }

      Pl_Fatal_Error("Syntax error: incorrect @ declaration in: %s", save_str);
    }


  fprintf(g, "\n\n   /*--- Begin Tag Generation ---*/\n\n");

#define Mk_Tag_Mask(x) ((((PlULong) (x) >> tag_size_low) << (value_size + tag_size_low)) | ((x) & ((1 << tag_size_low) - 1)))

#if 0
  tag_size = 4;
#else
  max_value++;
  if (max_value < nb_tag)
    Pl_Fatal_Error("There is an invalid tag value (repetition ?)\n");

  for (tag_size = 0; (1 << tag_size) < max_value; tag_size++)
    ;
#endif

#if WORD_SIZE == 32
  tag_size_low = 2;
#else
  tag_size_low = 3;
#endif

  tag_size_high = tag_size - tag_size_low;
  value_size = WORD_SIZE - tag_size;

  tag_mask = Mk_Tag_Mask((1 << tag_size) - 1);

  max_integer = (1 << (WORD_SIZE - tag_size - 1)) - 1;
  min_integer = -max_integer - 1;

  fprintf(fg_c, "#define PL_MIN_INTEGER\t\t%" PL_FMT_d "\n",  min_integer);
  fprintf(fg_c, "#define PL_MAX_INTEGER\t\t%" PL_FMT_d "\n",  max_integer);
  fputc('\n', fg_c);


  fprintf(g, "#define TAG_SIZE     \t\t%d\n", tag_size);
  fprintf(g, "#define TAG_SIZE_LOW \t\t%d\n", tag_size_low);
  fprintf(g, "#define TAG_SIZE_HIGH\t\t%d\n", tag_size_high);
  fprintf(g, "#define VALUE_SIZE   \t\t%d\n", value_size);
  fprintf(g, "#define TAG_MASK     \t\t(PlULong)%#" PL_FMT_x "\n", tag_mask);
  fprintf(g, "#define VALUE_MASK   \t\t(PlULong)%#" PL_FMT_x "\n", ~tag_mask);

  fprintf(g, "#define Tag_Mask_Of(w)\t\t((PlLong) (w) & (TAG_MASK))\n");

  if (tag_size_high > 0)
    fprintf(g, "#define Tag_From_Tag_Mask(w) \t(((PlULong) (w) >> %d) | ((w) & %d))\n", value_size, (1 << tag_size_low) -1);
  else
    fprintf(g, "#define Tag_From_Tag_Mask(w) \t(w)\n");


  if (tag_size_high > 0)
    fprintf(g, "#define Tag_Of(w)     \t\t((((PlULong) (w) >> %d) << %d) | ((w) & %d))\n",
            WORD_SIZE-tag_size_high, tag_size_low, (1 << tag_size_low) -1);
  else
    fprintf(g, "#define Tag_Of(w)     \t\tTag_Mask_Of(w)\n");

  for (i = 0; i < nb_tag; i++) {
    fprintf(g, "#define TAG_%s_MASK\t\t(PlULong)%#" PL_FMT_x "\n",
            tag[i].name, Mk_Tag_Mask(tag[i].value));
  }
  fprintf(g, "\n");

  fprintf(g, "#define NB_OF_TAGS       \t%d\n", nb_tag);

  for (i = 0; i < nb_tag; i++) {
    fprintf(g,    "#define %-10s \t\t%-2d\n", tag[i].name, tag[i].value);
    fprintf(fg_c, "#define PL_%-10s \t\t%-2d\n", tag[i].name, tag[i].value);
  }


  fprintf(g, "\n");
  fprintf(g, "\t/* General Tag/UnTag macros */\n\n");
  fprintf(g, "#define Tag_Long_Int(tm, v)  \t((((PlLong) ((v) << %d)) >> %d) | (tm))\n",
          tag_size, tag_size_high);


  fprintf(g, "#define Tag_Short_Uns(tm, v)\t(((PlLong) (v) << %d) + (tm))\n", tag_size_low);

/* For Tag_Address the + (tm) is better than | (tm) since the C compiler can
 * optimizes things like Tag_Address(2, H + 1) with only 1 instruction (+ 6)
 * instead of 2 (1 for + 4, 1 for | TAG_STC_MASK)
 */
  fprintf(g, "#define Tag_Address(tm, v)  \t((PlLong) (v) + (tm))\n");

  fprintf(g, "\n");
  fprintf(g, "#define UnTag_Long_Int(w)    \t((PlLong) ((w) << %d) >> %d)\n",
          tag_size_high, tag_size);

  fprintf(g, "#define UnTag_Short_Uns(w)\tUnTag_Long_Int(w)\n");

  fprintf(g, "#define UnTag_Address(w)  \t((WamWord *) ((w) & VALUE_MASK))\n");

  fprintf(g, "\n");
  fprintf(g, "\n");
  fprintf(g, "\t/* Specialized Tag/UnTag macros */\n\n");

  fprintf(g, "\n");

  for (i = 0; i < nb_tag; i++)
    {
      fprintf(g, "#define Tag_%s(v)  \t\t", tag[i].name);
      switch(tag[i].type)
        {
        case LONG_INT:
          if (tag[i].value == 0)
            fprintf(g, "(((PlULong) (v) << %d) & VALUE_MASK)\n",
                    tag_size_low);
          /* testing if high bits are 1 should suffice below - TO DO */
          else if (tag[i].value == (1 << tag_size) - 1)
            fprintf(g, "(((PlULong) (v) << %d) | TAG_MASK)\n",
                    tag_size_low);
          else
            fprintf(g, "Tag_Long_Int(TAG_%s_MASK, v)\n", tag[i].name);
          break;

        case SHORT_UNS:
          fprintf(g, "Tag_Short_Uns(TAG_%s_MASK, v)\n", tag[i].name);
          break;

        case ADDRESS:
          fprintf(g, "Tag_Address(TAG_%s_MASK, v)\n", tag[i].name);
          break;
        }
    }

  fprintf(g, "\n");

  for (i = 0; i < nb_tag; i++)
    {
      fprintf(g, "#define UnTag_%s(w)  \t\t", tag[i].name);
      switch(tag[i].type)
        {
        case LONG_INT:
          fprintf(g, "UnTag_Long_Int(w)\n");
          break;

        case SHORT_UNS:
          if (tag[i].value <= 3)
            fprintf(g, "((PlULong) (w) >> %d)\n", tag_size_low);
          else
            fprintf(g, "UnTag_Short_Uns(w)\n");
          break;

        case ADDRESS:
          if (tag[i].value == 0)
            fprintf(g, "((WamWord *) (w))\n");
          else
            fprintf(g, "UnTag_Address(w)\n");
          break;
        }
    }

  fprintf(g, "\n");

  for (i = 0; i < nb_tag; i++)
    {
      fprintf(g, "#define Tag_Is_%s(w)  \t\t(Tag_Mask_Of(w) == TAG_%s_MASK)\n", tag[i].name, tag[i].name);
    }


  fprintf(g, "\ntypedef enum\n");
  fprintf(g, "{\n");
  fprintf(g, "  LONG_INT,\n");
  fprintf(g, "  SHORT_UNS,\n");
  fprintf(g, "  ADDRESS\n");
  fprintf(g, "}TypTag;\n");

  fprintf(g, "\ntypedef struct\n");
  fprintf(g, "{\n");
  fprintf(g, "  char *name;\n");
  fprintf(g, "  TypTag type;\n");
  fprintf(g, "  int value;\n");
  fprintf(g, "  PlLong tag_mask;\n");
  fprintf(g, "}InfTag;\n\n\n");


  fprintf(g, "#ifdef ENGINE_FILE\n\n");
  fprintf(g, "InfTag pl_tag_tbl[] =\n{\n");

  for (i = 0; i < nb_tag; i++)
    {
      fprintf(g, "  { \"%s\", %s, %d, %" PL_FMT_x "}%s", tag[i].name,
              (tag[i].type == LONG_INT) ? "LONG_INT" :
              (tag[i].type == SHORT_UNS) ? "SHORT_UNS" : "ADDRESS",
              tag[i].value, Mk_Tag_Mask(tag[i].value),
              (i < nb_tag - 1) ? ",\n" : "\n};\n");
    }

  fprintf(g, "\n#else\n\n");
  fprintf(g, "extern InfTag pl_tag_tbl[];\n");
  fprintf(g, "\n#endif\n");


  fprintf(g, "\n\n   /*--- End Tag Generation ---*/\n\n");
}




/*-------------------------------------------------------------------------*
 * GENERATE_STACKS                                                         *
 *                                                                         *
 * stack description:                                                      *
 *    @stack name "description" default_size stack_top_macro               *
 *-------------------------------------------------------------------------*/
void
Generate_Stacks(FILE *f, FILE *g)
{
  static char str[STR_LENGTH];
  char *p1, *p2;
  int i;
  StackInf stack[12];
  int nb_stack = 0;

  for (;;)
    {
      if (feof(f) || !fgets(str, sizeof(str), f))
        Pl_Fatal_Error("Syntax error: end expected for @begin stack");

      if (*str != '@')
        {
          if (*str != '\n')
            fputs(str, g);
          continue;
        }

      strcpy(save_str, str);
      p1 = Read_Identifier(str + 1, 1, &p2);

      if (strcmp(p1, "end") == 0)
        break;

      if (strcmp(p1, "stack") == 0)
        {
          strcpy(stack[nb_stack].name, Read_Identifier(p2 + 1, 1, &p2));
          strcpy(stack[nb_stack].desc, Read_String(p2 + 1, &p2));
          i = Pl_Read_Integer(p2 + 1, &p2);
          stack[nb_stack].def_size = i * 1024 / sizeof(PlLong);

          strcpy(stack[nb_stack].top_macro,
                 Read_Identifier(p2 + 1, 1, &p2));
          nb_stack++;
          continue;
        }

      Pl_Fatal_Error("Syntax error: incorrect @ declaration in: %s", save_str);
    }


  fprintf(g, "\n\n   /*--- Begin Stack Generation ---*/\n\n");

  fprintf(g, "#include \"wam_stacks.h\"\n");

  fprintf(fw_s, "#define NB_OF_STACKS \t\t%d\n\n", nb_stack);


  for (i = 0; i < nb_stack; i++)
    {
      strcpy(str, stack[i].name);
      *str = toupper(*str);

      fprintf(fw_s, "#define %s_Stack       \t(pl_stk_tbl[%d].stack)\n", str, i);
      fprintf(fw_s, "#define %s_Size        \t(pl_stk_tbl[%d].size)\n", str, i);
      fprintf(fw_s, "#define %s_Offset(adr) \t((WamWord *)(adr) - %s_Stack)\n",
              str, str);
      fprintf(fw_s, "#define %s_Used_Size   \t%s_Offset(%s)\n\n", str, str,
              stack[i].top_macro);
    }

  fprintf(fw_s, "\n#define Stack_Top(s)       \t(");
  for (i = 0; i < nb_stack - 1; i++)
    fprintf(fw_s, "((s) == %d) ? %s : ", i, stack[i].top_macro);

  fprintf(fw_s, "%s)\n", stack[nb_stack - 1].top_macro);


  fprintf(fw_s, "\ntypedef struct\n");
  fprintf(fw_s, "{\n");
  fprintf(fw_s, "  char *name;\n");
  fprintf(fw_s, "  char *desc;\n");
  fprintf(fw_s, "  char *env_var_name;\n");
  fprintf(fw_s, "  PlLong *p_def_size;\t/* used for fixed_sizes */\n");
  fprintf(fw_s, "  int default_size; \t/* in WamWords */\n");
  fprintf(fw_s, "  int size;         \t/* in WamWords */\n");
  fprintf(fw_s, "  WamWord *stack;\n");
  fprintf(fw_s, "}InfStack;\n\n\n");


  fprintf(fw_s, "#ifdef ENGINE_FILE\n\n");

  fprintf(fw_s, "    /* these variables can be overwritten by top_comp.c (see stack size file) */\n");

  for (i = 0; i < nb_stack; i++)
    fprintf(fw_s, "PlLong pl_def_%s_size;\n", stack[i].name);
  fprintf(fw_s, "PlLong pl_fixed_sizes;\n\n");

  fprintf(fw_s, "InfStack pl_stk_tbl[] =\n{\n");

  for (i = 0; i < nb_stack; i++)
    {
      strcpy(str, stack[i].name);
      for (p1 = str; *p1; p1++)
        *p1 = toupper(*p1);

      fprintf(fw_s, " { \"%s\", %s, \"%sSZ\", &pl_def_%s_size, %d, 0, NULL }%s",
              stack[i].name, stack[i].desc, str, stack[i].name, stack[i].def_size,
              (i < nb_stack - 1) ? ",\n" : "\n};\n");
    }


  fprintf(fw_s, "\n#else\n\n");
  for (i = 0; i < nb_stack; i++)
    fprintf(fw_s, "extern PlLong pl_def_%s_size;\n", stack[i].name);
  fprintf(fw_s, "extern PlLong pl_fixed_sizes;\n\n\n");
  fprintf(fw_s, "extern InfStack pl_stk_tbl[];\n");
  fprintf(fw_s, "\n#endif\n");


  fprintf(g, "\n\n   /*--- End Stack Generation ---*/\n\n");
}




/*-------------------------------------------------------------------------*
 * PL_FATAL_ERROR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fatal_Error(char *format, ...)
{
  va_list arg_ptr;

  va_start(arg_ptr, format);
  vfprintf(stderr, format, arg_ptr);
  va_end(arg_ptr);

  fprintf(stderr, "\n");
  exit(1);
}
