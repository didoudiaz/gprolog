/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : src_rdr_c.c                                                     *
 * Descr.: Prolog source file reader - C part                              *
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
#include <errno.h>
#include <string.h>

#include "gp_config.h"

#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#endif


#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define DO                         0
#define UNDO                       1


#define REREAD_MASK                (1 << 16)
#define REFLECT_EOF_MASK           (1 << 17)
#define UNDO_DIRECTIVES_MASK       (1 << 18)




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum
{
  OP,
  SET_PROLOG_FLAG,
  CHAR_CONVERSION
} SRDirType;




typedef struct sr_one_direct *PSROneDirect;

typedef struct sr_one_direct
{
  SRDirType type;		/* directive type */
  WamWord a[2][3];		/* arguments: a[DO][...] and a[UNDO][...] */
  PSROneDirect next;		/* forward link (or NULL if last) */
  PSROneDirect prev;		/* backward link (or NULL if first) */
} SROneDirect;




typedef struct
{
  SROneDirect *first;		/* first directive or NULL */
  SROneDirect *last;		/* last directive or NULL */
} SRDirect;




typedef struct sr_file *PSRFile;

typedef struct sr_file
{
  int atom_file_name;		/* file name atom */
  int stm;			/* associated stream */
  Bool reposition;		/* is it repositionable ? */
  char *tmp_path;		/* tmp used for reread when !reposition */
  int tmp_stm;			/* stm of the tmp file */
  PSRFile next;			/* link to next file */
				/* --- include stack information --- */
  Bool eof_reached;		/* is the EOF reached for this file ? */
  int include_line;		/* line # this file includes a child file */
  PSRFile parent_file;		/* link to the parent file (includer) */
} SRFile;




typedef struct sr_module *PSRModule;

typedef struct sr_module
{
  int atom_module_name;		/* module atom */
  int i_atom_file_def;		/* interface: file name of definition */
  int i_line_def;		/* interface: line # of definition */
  int b_atom_file_def;		/* body: file name of current body (or -1) */
  int b_line_def;		/* body: line # of current body */
  SRDirect direct_lst;		/* list of directives (interface + body) */
  PSRModule next;		/* next module */
} SRModule;




typedef struct
{
  Bool in_use;			/* open ? */
  Bool close_master_at_end;	/* close master at sr_close/1 ? */
  int mask;			/* see src_rdr.pl */
  SRFile *file_first;		/* queue of all files - first */
  SRFile *file_last;		/* queue of all files - last */
  SRFile *file_top;		/* stack of open files (top = current) */
  SRFile *next_to_reread;	/* NULL: in pass 1 or no more to reread */
  int cur_l1, cur_l2;		/* position (lines) of last read term */
  int char_count;		/* nb chars read in processed files */
  int line_count;		/* nb lines read in processed files */
  int error_count;		/* nb of errors emitted */
  int warning_count;		/* nb of warnings emitted */
  int out_sora_word;		/* SorA for writing (or NOT_A_WAM_WORD) */
  SRDirect direct_lst;		/* list of directives */
  SRModule *module_lst;		/* list of defined modules */
  SRModule *cur_module;		/* NULL or current module */
  Bool interface;		/* in interface or body of current module */
} SRInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static SRInf *sr_tbl = NULL;	/* table (mallocated) */
static int sr_tbl_size = 0;	/* allocated size */
static int sr_last_used = -1;	/* last sr used */
static SRInf *cur_sr;		/* the current sr entry used */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


static void Common_Clean(SRInf *sr, Bool for_reread);

static SRInf *Get_Descriptor(WamWord desc_word, Bool accept_none);

static void Do_Directives(SRDirect *direct);

static void Undo_Directives(SRDirect *direct);

static void Exec_One_Directive(SROneDirect *o, int do_undo);

static void Close_Current_Module(void);

static StmInf *Write_Location(WamWord sora_word, WamWord list_word,
			      int atom_file_name, int l1, int l2c);

static void Write_Message_Text(StmInf *pstm,  WamWord sora_word,
			       WamWord type_word,
			       WamWord format_word, WamWord args_word);

				/* from oper_c.c */
void Pl_Op_3(WamWord prec_word, WamWord specif_word, WamWord oper_word);

				/* form flag_c.c */
Bool Pl_Set_Prolog_Flag_2(WamWord flag_word, WamWord value_word);

				/* from read_c.c */
void Pl_Char_Conversion_2(WamWord in_char_word, WamWord out_char_word);

				/* from write_c.c */

void Pl_Write_2(WamWord sora_word, WamWord term_word);

				/* from format_c.c */

void Pl_Format_3(WamWord sora_word, WamWord format_word, WamWord args_word);



#define Interf_Body(interf) ((interf) ? "module" : "body")


#define SR_CURRENT_DESC_ALT        X1_2473725F63757272656E745F64657363726970746F725F616C74

Prolog_Prototype(SR_CURRENT_DESC_ALT, 0);


/* TODO:
 * - use a table of pointers SRInf *[] + Malloc + Free
 * - do not use a dup of !repositionable stream but
 *   change the mirror before the read_term and restore after
 * ???????????????????????????????????????????????????????????????????????
 */


/*-------------------------------------------------------------------------*
 * PL_SR_INIT_OPEN_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Init_Open_2(WamWord desc_word, WamWord out_sora_word)
{
  SRInf *sr;
  int desc;

  if (sr_tbl == NULL)		/* first allocation */
    {
      sr_tbl_size = 8;
      sr_last_used = -1;
      sr_tbl = (SRInf *) Calloc(sr_tbl_size, sizeof(SRInf));
    }

  for(desc = 0; desc < sr_tbl_size; desc++)
    if (!sr_tbl[desc].in_use)
      break;

  if (desc == sr_tbl_size)
    Pl_Extend_Array((char **) &sr_tbl, &sr_tbl_size, sizeof(SRInf), TRUE);

  if (desc > sr_last_used)
    sr_last_used = desc;

  sr = cur_sr = sr_tbl + desc;

  if (sr->file_top)		    /* to due a previous aborted sr_open/3 */
    {
      Free(sr->file_top);
      sr->file_top = NULL;
    }

  sr->mask = SYS_VAR_OPTION_MASK;

  sr->file_first = NULL;
  sr->file_last = NULL;
  sr->next_to_reread = NULL;	/* 1st read mode */

  sr->cur_l1 = sr->cur_l2 = 0;
  sr->char_count = 0;
  sr->line_count = 0;
  sr->error_count = 0;
  sr->warning_count = 0;

  if (pl_sys_var[1])
    {
      Pl_Get_Stream_Or_Alias(out_sora_word, STREAM_CHECK_VALID);
      sr->out_sora_word = out_sora_word;
    }
  else
    sr->out_sora_word = NOT_A_WAM_WORD;

  sr->direct_lst.first = NULL;
  sr->direct_lst.last = NULL;

  sr->module_lst = NULL;
  sr->cur_module = NULL;
  sr->interface = FALSE;

  Pl_Get_Integer(desc, desc_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_OPEN_FILE_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Open_File_2(WamWord file_name_word, WamWord from_stream_word)
{
  SRInf *sr = cur_sr;
  int atom_file_name;
  int stm;
  SRFile *file;
  Bool from_stream = Pl_Rd_Boolean(from_stream_word);
  Bool master_file = (sr->file_top == NULL);
  StmInf *pstm, *pstm_tmp;

  if (sr->next_to_reread == NULL)
    {
      if (from_stream)
	{
	  stm = Pl_Get_Stream_Or_Alias(file_name_word, STREAM_CHECK_INPUT);
	  Pl_Check_Stream_Type(stm, TRUE, TRUE);
	  atom_file_name = pl_stm_tbl[stm]->atom_file_name;
	}
      else
	{
	  atom_file_name = Pl_Rd_Atom(file_name_word);
	  if (strcmp(pl_atom_tbl[atom_file_name].name, "user") == 0)
#if 0
	    stm = pl_stm_input;
#else
	  {
	    stm = Pl_Add_Stream(0, (PlLong) 0, pl_stm_tbl[pl_stm_input]->prop,
		    NULL, NULL, NULL, NULL, NULL, NULL, NULL);
	    *pl_stm_tbl[stm] = *pl_stm_tbl[pl_stm_input];
	  }
#endif
	  else
	    {
	      stm = Pl_Add_Stream_For_Stdio_File(pl_atom_tbl[atom_file_name].name,
					      STREAM_MODE_READ, TRUE);
	      if (stm < 0)
		{
		  if (errno == ENOENT || errno == ENOTDIR)
		    Pl_Err_Existence(pl_existence_source_sink,
				     file_name_word);
		  else
		    Pl_Err_Permission(pl_permission_operation_open,
				      pl_permission_type_source_sink,
				      file_name_word);
		}
	    }
	}
      pstm = pl_stm_tbl[stm];
      file = (SRFile *) Malloc(sizeof(SRFile));
      file->atom_file_name = atom_file_name;
      file->stm = stm;
      file->reposition = pstm->prop.reposition;
      if (!file->reposition)
	{
	  file->tmp_path = Pl_M_Tempnam(NULL, NULL);
	  file->tmp_stm = Pl_Add_Stream_For_Stdio_File(file->tmp_path,
						    STREAM_MODE_WRITE, TRUE);
	  if (file->tmp_stm < 0)
	    Pl_Fatal_Error("cannot create tmp file %s in %s:%d", file->tmp_path,
			__FILE__, __LINE__);

				/* try to be similar to original file */
	  pstm_tmp = pl_stm_tbl[file->tmp_stm];
	  pstm_tmp->atom_file_name = atom_file_name;
	  pstm_tmp->prop.eof_action = pstm->prop.eof_action;
	  if (pstm_tmp->prop.buffering != pstm->prop.buffering)
	    {
	      pstm_tmp->prop.buffering = pstm->prop.buffering;
	      Pl_Stdio_Set_Buffering((FILE *) pstm_tmp->file,
				  pstm_tmp->prop.buffering);
	    }
	  Pl_Add_Mirror_To_Stream(stm, file->tmp_stm);
	}
      else
	{
	  file->tmp_path = NULL;
	  file->tmp_stm = -1;
	}
      file->next = NULL;
      if (sr->file_first == NULL)
	sr->file_first = file;
      else
	sr->file_last->next = file;
      sr->file_last = file;
    }
  else
    file = sr->next_to_reread;


  file->eof_reached = FALSE;
  file->parent_file = sr->file_top;
  if (sr->file_top)
    sr->file_top->include_line = sr->cur_l1;
  sr->file_top = file;

  if (master_file)		/* we see here the master file */
    {
      sr->close_master_at_end = !from_stream;
      sr->in_use = TRUE;
    }
}




/*-------------------------------------------------------------------------*
 * PL_SR_CLOSE_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Close_1(WamWord desc_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  SRFile *file, *file1;

  file = sr->file_first;
  if (!sr->close_master_at_end && file->tmp_path == NULL)
    goto skip_first;
  do
    {
      Pl_Close_Stm(file->stm, TRUE);
      if (file->tmp_path)
	unlink(file->tmp_path);

    skip_first:
      file1 = file;
      file = file->next;
      Free(file1);
    }
  while(file);
  sr->file_top = NULL;

  Common_Clean(sr, FALSE);

  sr->in_use = FALSE;

  while(sr_last_used >= 0 && !sr_tbl[sr_last_used].in_use)
    sr_last_used--;
}




/*-------------------------------------------------------------------------*
 * COMMON_CLEAN                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Common_Clean(SRInf *sr, Bool for_reread)
{
  SROneDirect *o, *o1;
  SRModule *m, *m1;

  if (for_reread || (sr->mask & UNDO_DIRECTIVES_MASK) != 0)
    {
      if (sr->cur_module)
	Undo_Directives(&sr->cur_module->direct_lst);
      Undo_Directives(&sr->direct_lst);
    }

  o = sr->direct_lst.first;
  while(o)
    {
      o1 = o;
      o = o->next;
      Free(o1);
    }

  m = sr->module_lst;
  while(m)
    {
      o = m->direct_lst.first;
      while(o)
	{
	  o1 = o;
	  o = o->next;
	  Free(o1);
	}
      m1 = m;
      m = m->next;
      Free(m1);
    }

  if (for_reread)
    {
      sr->cur_l1 = sr->cur_l2 = 0;
      sr->char_count = 0;
      sr->line_count = 0;
      sr->error_count = 0;
      sr->warning_count = 0;
      sr->direct_lst.first = NULL;
      sr->direct_lst.last = NULL;

      sr->module_lst = NULL;
      sr->cur_module = NULL;
      sr->interface = FALSE;
    }
}




/*-------------------------------------------------------------------------*
 * PL_SR_NEW_PASS_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_New_Pass_1(WamWord desc_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  StmInf *pstm;
  SRFile *file;

  if ((sr->mask & REREAD_MASK) == 0)
    return FALSE;

  if (!sr->file_last->reposition && !sr->file_top->eof_reached)
    {
      pstm = pl_stm_tbl[sr->file_top->stm];
      while(Pl_Stream_Getc(pstm) != EOF) /* read until EOF for mirror */
	;
    }

  sr->next_to_reread = sr->file_first->next;

  for(file = sr->file_first; file; file = file->next)
    {
      if (file->reposition)
	Pl_Stream_Set_Position(pl_stm_tbl[file->stm], SEEK_SET, 0, 0, 0, 0);
      else
	{
	  if (file != sr->file_first || sr->close_master_at_end)
	    Pl_Close_Stm(file->stm, TRUE);
	  Pl_Close_Stm(file->tmp_stm, TRUE); /* close mirror file */
	  file->stm = Pl_Add_Stream_For_Stdio_File(file->tmp_path,
						STREAM_MODE_READ, TRUE);
	  file->reposition = TRUE;
	}
    }
  sr->file_top = sr->file_first;
  sr->file_top->eof_reached = FALSE;

  Common_Clean(sr, TRUE);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_SR_ADD_DIRECTIVE_7                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Add_Directive_7(WamWord type_word,
		   WamWord d1_word, WamWord d2_word, WamWord d3_word,
		   WamWord u1_word, WamWord u2_word, WamWord u3_word)
{
  SRInf *sr = cur_sr;
  SRDirect *d;
  SROneDirect one, *o;
  WamWord word, tag_mask;

  if (sr->cur_module == NULL)
    d = &sr->direct_lst;
  else
    d = &(sr->cur_module->direct_lst);

  o = &one;
  o->type = Pl_Rd_Integer(type_word);

  DEREF(d1_word, word, tag_mask);
  o->a[0][0] = word;

  DEREF(d2_word, word, tag_mask);
  o->a[0][1] = word;

  DEREF(d3_word, word, tag_mask);
  o->a[0][2] = word;

  DEREF(u1_word, word, tag_mask);
  o->a[1][0] = word;

  DEREF(u2_word, word, tag_mask);
  o->a[1][1] = word;

  DEREF(u3_word, word, tag_mask);
  o->a[1][2] = word;

  o->next = NULL;
  o->prev = d->last;

  Exec_One_Directive(o, DO);
				/* if exec OK, allocate and add it in lst */

  o = (SROneDirect *) Malloc(sizeof(SROneDirect));
  *o = one;

  if (d->first == NULL)
    d->first = o;
  else
    d->last->next = o;

  d->last = o;
}




/*-------------------------------------------------------------------------*
 * DO_DIRECTIVES                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Do_Directives(SRDirect *direct)
{
  SROneDirect *o;

  for (o = direct->first; o; o = o->next)
    Exec_One_Directive(o, DO);
}




/*-------------------------------------------------------------------------*
 * UNDO_DIRECTIVES                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Undo_Directives(SRDirect *direct)
{
  SROneDirect *o;

  for (o = direct->last; o; o = o->prev)
    Exec_One_Directive(o, UNDO);
}



/*-------------------------------------------------------------------------*
 * EXEC_ONE_DIRECTIVE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Exec_One_Directive(SROneDirect *o, int do_undo)
{
  switch(o->type)
    {
    case OP:
      Pl_Op_3(o->a[do_undo][0], o->a[do_undo][1], o->a[do_undo][2]);
      break;

    case SET_PROLOG_FLAG:
      Pl_Set_Prolog_Flag_2(o->a[do_undo][0], o->a[do_undo][1]);
      break;

    case CHAR_CONVERSION:
      Pl_Char_Conversion_2(o->a[do_undo][0], o->a[do_undo][1]);
      break;
    }
}




/*-------------------------------------------------------------------------*
 * PL_SR_CHANGE_OPTIONS_0                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Change_Options_0(void)
{
  SRInf *sr = cur_sr;
  int reread_mask = sr->mask & REREAD_MASK;

  sr->mask = (SYS_VAR_OPTION_MASK & (~REREAD_MASK)) | reread_mask;
}




/*-------------------------------------------------------------------------*
 * SR_GET_STM__FOR_READ_TERM_1                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Get_Stm_For_Read_Term_1(WamWord stm_word)
{
  SRInf *sr = cur_sr;
  SRFile *file;

  for(;;)
    {
      file = sr->file_top;

      if (!file->eof_reached)
	break;
				/* a EOF is reached */

      if (file->parent_file == NULL)
	break;

      sr->char_count += pl_stm_tbl[file->stm]->char_count;
      sr->line_count += pl_stm_tbl[file->stm]->line_count;
      sr->file_top = file->parent_file;
    }

  Pl_Get_Integer(sr->file_top->stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_EOF_REACHED_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_EOF_Reached_1(WamWord err_word)
{
  SRInf *sr = cur_sr;

  sr->file_top->eof_reached = TRUE;	/* delay close at next read */

  if (sr->file_top->parent_file == NULL)
    {
      if (sr->cur_module)
	{
	  sprintf(pl_glob_buff, "end_%s(%s) not encoutered - assumed found",
		  Interf_Body(sr->interface),
		  pl_atom_tbl[sr->cur_module->atom_module_name].name);

	  Close_Current_Module();
	  Pl_Un_String(pl_glob_buff, err_word);
	}

      return TRUE;		/* always reflect EOF for master file */
    }

  return (sr->mask & REFLECT_EOF_MASK);
}




/*-------------------------------------------------------------------------*
 * PL_SR_UPDATE_POSITION_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Update_Position_0(void)
{
  SRInf *sr = cur_sr;

  sr->cur_l1 = pl_last_read_line;
  sr->cur_l2 = pl_stm_tbl[sr->file_top->stm]->line_count;
  if (pl_stm_tbl[sr->file_top->stm]->line_pos > 0)
    sr->cur_l2++;
}




/*-------------------------------------------------------------------------*
 * PL_SR_START_MODULE_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Start_Module_3(WamWord module_name_word, WamWord interface_word,
		  WamWord err_word)
{
  SRInf *sr = cur_sr;
  int atom_module_name = Pl_Rd_Atom_Check(module_name_word);
  Bool interface = Pl_Rd_Boolean(interface_word);
  SRModule *m;

  *pl_glob_buff = '\0';

  for(m = sr->module_lst; m; m = m->next)
    if (m->atom_module_name == atom_module_name)
      break;

  if (m == NULL)
    {
      if (!interface)
	{
	  sprintf(pl_glob_buff, "module(%s) not encoutered - interface assumed empty",
		  pl_atom_tbl[atom_module_name].name);
	}
      m = (SRModule *) Malloc(sizeof(SRModule));
      m->atom_module_name = atom_module_name;
      m->i_atom_file_def = sr->file_top->atom_file_name;
      m->i_line_def = sr->cur_l1;
      m->b_atom_file_def = -1;
      m->b_line_def = 0;
      m->direct_lst.first = NULL;
      m->direct_lst.last = NULL;
      m->next = sr->module_lst;
      sr->module_lst = m;
    }
  else
    {
      if (interface)
	{
	  sprintf(pl_glob_buff, "module(%s) already found at %s:%d - directive ignored",
		  pl_atom_tbl[atom_module_name].name,
		  pl_atom_tbl[m->i_atom_file_def].name,
		  m->i_line_def);
	  goto error;
	}
    }

  if (sr->cur_module)
    {
      sprintf(pl_glob_buff, "end_%s(%s) not encoutered - assumed found",
	      Interf_Body(sr->interface),
	      pl_atom_tbl[sr->cur_module->atom_module_name].name);

      Close_Current_Module();
    }

  if (!interface)
    {
      m->b_atom_file_def = sr->file_top->atom_file_name;
      m->b_line_def = sr->cur_l1;
    }

  sr->cur_module = m;
  sr->interface = interface;

  Do_Directives(&m->direct_lst);

  if (*pl_glob_buff)
    {
    error:
      Pl_Un_String(pl_glob_buff, err_word);
    }
}




/*-------------------------------------------------------------------------*
 * PL_SR_STOP_MODULE_3                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Stop_Module_3(WamWord module_name_word, WamWord interface_word,
		 WamWord err_word)
{
  SRInf *sr = cur_sr;
  int atom_module_name = Pl_Rd_Atom_Check(module_name_word);
  Bool interface = Pl_Rd_Boolean(interface_word);
  SRModule *m = sr->cur_module;

  *pl_glob_buff = '\0';

  if (m == NULL)
    {
      sprintf(pl_glob_buff, "corresponding directive %s(%s) not found - directive ignored",
	      Interf_Body(interface),
	      pl_atom_tbl[atom_module_name].name);
      goto error;
    }

  if (interface != sr->interface || atom_module_name != m->atom_module_name)
    {
      sprintf(pl_glob_buff, "directive mismatch wrt %s:%d - replaced by end_%s(%s)",
	      (sr->interface) ?
	      pl_atom_tbl[m->i_atom_file_def].name :
	      pl_atom_tbl[m->b_atom_file_def].name,
	      (sr->interface) ? m->i_line_def : m->b_line_def,
	      Interf_Body(sr->interface),
	      pl_atom_tbl[m->atom_module_name].name);
    }

  Close_Current_Module();

  if (*pl_glob_buff)
    {
    error:
      Pl_Un_String(pl_glob_buff, err_word);
    }
}




/*-------------------------------------------------------------------------*
 * CLOSE_CURRENT_MODULE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static
void Close_Current_Module(void)
{
  SRInf *sr = cur_sr;

  Undo_Directives(&sr->cur_module->direct_lst);
  sr->cur_module = NULL;
}




/*-------------------------------------------------------------------------*
 * PL_SR_CURRENT_DESCRIPTOR_1                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Current_Descriptor_1(WamWord desc_word)
{
  WamWord word, tag_mask;
  int desc = 0;

  DEREF(desc_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      desc = UnTag_INT(word);
      return (desc >= 0 && desc <= sr_last_used && sr_tbl[desc].in_use);
    }
  if (tag_mask != TAG_REF_MASK)
    Pl_Err_Type(pl_type_integer, word);

  for (; desc <= sr_last_used; desc++)
    if (sr_tbl[desc].in_use)
      break;

  if (desc >= sr_last_used)
    {
      if (desc > sr_last_used)
	return FALSE;
    }
  else				/* non deterministic case */
    {
      A(0) = desc_word;
      A(1) = desc + 1;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(SR_CURRENT_DESC_ALT, 0),
			  2);
    }
  return Pl_Get_Integer(desc, desc_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_CURRENT_DESCRIPTOR_ALT_0                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Current_Descriptor_Alt_0(void)
{
  WamWord desc_word;
  int desc;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(SR_CURRENT_DESC_ALT, 0), 0);

  desc_word = AB(B, 0);
  desc = AB(B, 1);

  for (; desc <= sr_last_used; desc++)
    if (sr_tbl[desc].in_use)
      break;

  if (desc >= sr_last_used)
    {
      Delete_Last_Choice_Point();
      if (desc > sr_last_used)
	return FALSE;
    }
  else				/* non deterministic case */
    {
#if 0				/* the following data is unchanged */
      AB(B, 0) = desc_word;
#endif
      AB(B, 1) = desc + 1;
    }

  return Pl_Get_Integer(desc, desc_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_IS_BIT_SET_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Is_Bit_Set_1(WamWord bit_word)
{
  return cur_sr->mask & (1 << Pl_Rd_Integer(bit_word));
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_STM_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Stm_2(WamWord desc_word, WamWord stm_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  return Pl_Get_Integer(sr->file_top->stm, stm_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_MODULE_3                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Module_3(WamWord desc_word, WamWord module_name_word,
		WamWord interface_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  SRModule *m = sr->cur_module;

  Pl_Check_For_Un_Atom(module_name_word);
  Pl_Check_For_Un_Atom(interface_word);

  if (m == NULL)
    return FALSE;

  if (!Pl_Get_Atom(m->atom_module_name, module_name_word))
    return FALSE;

  if (sr->interface)
    return Pl_Un_String("interface", interface_word);

  return Pl_Un_String("body", interface_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_FILE_NAME_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_File_Name_2(WamWord desc_word, WamWord file_name_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  return Pl_Un_Atom_Check(sr->file_top->atom_file_name, file_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_POSITION_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Position_3(WamWord desc_word, WamWord l1_word, WamWord l2_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);

  Pl_Check_For_Un_Integer(l1_word);
  Pl_Check_For_Un_Integer(l2_word);

  return Pl_Get_Integer(sr->cur_l1, l1_word) &&
    Pl_Get_Integer(sr->cur_l2, l2_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_INCLUDE_LIST_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Include_List_2(WamWord desc_word, WamWord list_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  SRFile *file;
  WamWord word;

  Pl_Check_For_Un_List(list_word);

				/* skip 1st file (current) */
  for(file = sr->file_top->parent_file; file; file = file->parent_file)
    {
      word = Pl_Put_Structure(ATOM_CHAR(':'), 2);
      Pl_Unify_Atom(file->atom_file_name);
      Pl_Unify_Integer(file->include_line);

      if (!Pl_Get_List(list_word) || !Pl_Unify_Value(word))
        return FALSE;

      list_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_INCLUDE_STREAM_LIST_2                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Include_Stream_List_2(WamWord desc_word, WamWord list_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  SRFile *file;
  WamWord word;

  Pl_Check_For_Un_List(list_word);

				/* skip 1st file (current) */
  for(file = sr->file_top->parent_file; file; file = file->parent_file)
    {
      word = Pl_Put_Structure(pl_atom_stream, 1);
      Pl_Unify_Integer(file->stm);

      if (!Pl_Get_List(list_word) || !Pl_Unify_Value(word))
        return FALSE;

      list_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_SIZE_COUNTERS_3                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Size_Counters_3(WamWord desc_word, WamWord chars_word,
		       WamWord lines_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  SRFile *file;
  int char_count, line_count;

  Pl_Check_For_Un_Integer(chars_word);
  Pl_Check_For_Un_Integer(lines_word);

  char_count = sr->char_count;
  line_count = sr->line_count;

  for(file = sr->file_top; file; file = file->parent_file)
    {
      char_count += pl_stm_tbl[file->stm]->char_count;
      line_count += pl_stm_tbl[file->stm]->line_count;
    }

  return Pl_Get_Integer(char_count, chars_word) &&
    Pl_Get_Integer(line_count, lines_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_GET_ERROR_COUNTERS_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_SR_Get_Error_Counters_3(WamWord desc_word, WamWord errors_word,
			WamWord warnings_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);

  Pl_Check_For_Un_Integer(errors_word);
  Pl_Check_For_Un_Integer(warnings_word);

  return Pl_Get_Integer(sr->error_count, errors_word) &&
    Pl_Get_Integer(sr->warning_count, warnings_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_SET_ERROR_COUNTERS_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Set_Error_Counters_3(WamWord desc_word, WamWord errors_word,
			WamWord warnings_word)
{
  SRInf *sr = Get_Descriptor(desc_word, FALSE);
  int errors = Pl_Rd_Integer_Check(errors_word);
  int warnings = Pl_Rd_Integer_Check(warnings_word);

  sr->error_count = errors;
  sr->warning_count = warnings;
}




/*-------------------------------------------------------------------------*
 * PL_SR_CHECK_DESCRIPTOR_1                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Check_Descriptor_1(WamWord desc_word)
{
  Get_Descriptor(desc_word, FALSE);
}




/*-------------------------------------------------------------------------*
 * GET_DESCRIPTOR                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static SRInf *
Get_Descriptor(WamWord desc_word, Bool accept_none)
{
  WamWord word, tag_mask;
  int desc;
  int atom;

  if (accept_none)
    {
      DEREF(desc_word, word, tag_mask);
      atom = UnTag_ATM(word);
      if (tag_mask == TAG_ATM_MASK &&
	  strcmp(pl_atom_tbl[atom].name, "none") == 0)
	{
	  cur_sr = NULL;
	  return cur_sr;
	}

    }
  desc = Pl_Rd_Integer_Check(desc_word);

  if (desc < 0 || desc > sr_last_used || !sr_tbl[desc].in_use)
    Pl_Err_Existence(pl_existence_sr_descriptor, desc_word);

  cur_sr = sr_tbl + desc;
  SYS_VAR_OPTION_MASK = cur_sr->mask;
  return cur_sr;
}




/*-------------------------------------------------------------------------*
 * PL_SR_WRITE_MESSAGE_4                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Write_Message_4(WamWord desc_word,
		   WamWord type_word, WamWord format_word, WamWord args_word)
{
  SRInf *sr = Get_Descriptor(desc_word, TRUE);
  StmInf *pstm;
  int atom_file_name;
  int l1, l2c;
  WamWord sora_word;

  if (sr)
    {
      sora_word = sr->out_sora_word;
      atom_file_name = sr->file_top->atom_file_name;
      l1 = sr->cur_l1;
      l2c = sr->cur_l2;
    }
  else
    {
      sora_word = NOT_A_WAM_WORD;
      atom_file_name = pl_atom_void;
      l1 = 0;
      l2c = 0;
    }

  pstm = Write_Location(sora_word, NOT_A_WAM_WORD, atom_file_name, l1, l2c);
  Write_Message_Text(pstm, sora_word, type_word, format_word, args_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_WRITE_MESSAGE_6                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Write_Message_6(WamWord desc_word,
		   WamWord l1_word, WamWord l2c_word,
		   WamWord type_word, WamWord format_word, WamWord args_word)
{
  SRInf *sr = Get_Descriptor(desc_word, TRUE);
  StmInf *pstm;
  int atom_file_name;
  int l1, l2c;
  WamWord sora_word;

  if (sr)
    {
      sora_word = sr->out_sora_word;
      atom_file_name = sr->file_top->atom_file_name;
    }
  else
    {
      sora_word = NOT_A_WAM_WORD;
      atom_file_name = pl_atom_void;
    }

  l1 = Pl_Rd_Integer_Check(l1_word);
  l2c = Pl_Rd_Integer_Check(l2c_word);

  pstm = Write_Location(sora_word, NOT_A_WAM_WORD, atom_file_name, l1, l2c);
  Write_Message_Text(pstm, sora_word, type_word, format_word, args_word);
}




/*-------------------------------------------------------------------------*
 * PL_SR_WRITE_MESSAGE_8                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_SR_Write_Message_8(WamWord desc_word, WamWord list_word,
		   WamWord file_name_word,
		   WamWord l1_word, WamWord l2c_word,
		   WamWord type_word, WamWord format_word, WamWord args_word)
{
  SRInf *sr = Get_Descriptor(desc_word, TRUE);
  StmInf *pstm;
  int atom_file_name;
  int l1, l2c;
  WamWord sora_word;

  if (!Pl_Blt_List(list_word))
    Pl_Err_Type(pl_type_list, list_word);

  if (sr)
    {
      sora_word = sr->out_sora_word;
    }
  else
    {
      sora_word = NOT_A_WAM_WORD;
    }

  sora_word = sr->out_sora_word;
  atom_file_name = Pl_Rd_Atom_Check(file_name_word);
  l1 = Pl_Rd_Integer_Check(l1_word);
  l2c = Pl_Rd_Integer_Check(l2c_word);

  pstm = Write_Location(sora_word, list_word, atom_file_name, l1, l2c);
  Write_Message_Text(pstm, sora_word, type_word, format_word, args_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_LOCATION                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static StmInf *
Write_Location(WamWord sora_word, WamWord list_word, int atom_file_name,
	       int l1, int l2c)

{
  WamWord word, tag_mask;
  int stm;
  StmInf *pstm;
  WamWord *lst_adr;
  Bool first;
  SRInf *sr = cur_sr;
  SRFile *file = NULL;
  int char_count;

  stm = (sora_word == NOT_A_WAM_WORD)
    ? pl_stm_output : Pl_Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = pl_stm_tbl[stm];

  pl_last_output_sora = sora_word;
  Pl_Check_Stream_Type(stm, TRUE, FALSE);

  if (list_word == NOT_A_WAM_WORD && sr != NULL)
    file = sr->file_top->parent_file;

  for (first = TRUE; ; first = FALSE)
    {
      if (list_word != NOT_A_WAM_WORD)
	{
	  DEREF(list_word, word, tag_mask);

	  if (word == NIL_WORD)
	    break;
	}
      else
	if (file == NULL)
	  break;


      if (first)
	{
	  if (pstm->line_pos != 0)
	    Pl_Stream_Putc('\n', pstm);
	  Pl_Stream_Puts("In file included from ", pstm);
	}
      else
	Pl_Stream_Printf(pstm, ",\n%*s from ", 16, "");

      if (list_word != NOT_A_WAM_WORD)
	{
	  lst_adr = UnTag_LST(word);
				/* accepts sora_word = NOT_A_WAM_WORD */
	  Pl_Write_2(sora_word, Car(lst_adr));
	  list_word = Cdr(lst_adr);
	}
      else
	{
	  Pl_Stream_Printf(pstm, "%s:%d",
			pl_atom_tbl[file->atom_file_name].name,
			file->include_line);
	  file = file->parent_file;
	}
    }

  if (!first)
    Pl_Stream_Puts(":\n", pstm);


  char_count = pstm->char_count;

  if (atom_file_name != pl_atom_void)
    Pl_Stream_Puts(pl_atom_tbl[atom_file_name].name, pstm);

  if (l1 > 0)
    {
      Pl_Stream_Printf(pstm, ":%d", l1);

      if (l2c != l1)
	{
	  if (l2c > 0)
	    Pl_Stream_Printf(pstm, "--%d", l2c);
	  else
	    Pl_Stream_Printf(pstm, ":%d", -l2c);
	}
    }

  if (char_count != pstm->char_count)
    Pl_Stream_Puts(": ", pstm);

  return pstm;
}




/*-------------------------------------------------------------------------*
 * WRITE_MESSAGE_TEXT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Write_Message_Text(StmInf *pstm, WamWord sora_word,
		   WamWord type_word, WamWord format_word, WamWord args_word)
{
  SRInf *sr = cur_sr;
  char *type = Pl_Rd_String_Check(type_word);

  if (*type)
    {
      Pl_Stream_Printf(pstm, "%s: ", type);
      if (sr)
	{
	  if (strstr(type, "error") || strstr(type, "exception"))
	    sr->error_count++;
	  else if (strstr(type, "warning"))
	    sr->warning_count++;
	}
    }
				/* accepts sora_word = NOT_A_WAM_WORD */
  Pl_Format_3(sora_word, format_word, args_word);
}
