/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_archi.def (gives rise to wam_archi.h)                       *
 * Descr.: Wam architecture definition - description file                  *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

#include "pl_long.h"

/*---------------------------------*
 * Register Descriptions           *
 *---------------------------------*/

typedef intptr_t WamWord;       /* a WamWord can store a ptr (32/64 bits) */

typedef void (*CodePtr) ();     /* a code pointer is a ptr to fct */

typedef CodePtr WamCont;        /* a continuation is a code pointer */

#ifndef ONLY_TAG_PART

#define X(x)                       (pl_reg_bank[x])
#define A(a)                       (pl_reg_bank[a])

typedef WamWord *WamWordP;




   /*--- Begin Register Generation ---*/

register WamWord 		*pl_reg_bank asm ("r12");

register WamWordP		TR  asm ("r13");
register WamWordP		B   asm ("r14");
register WamWordP		H   asm ("r15");


#define HB1			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+0])
#define CP			(((WamCont  *) pl_reg_bank)[NB_OF_X_REGS+1])
#define E			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+2])
#define CS			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+3])
#define S			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+4])
#define STAMP			(((WamWord  *) pl_reg_bank)[NB_OF_X_REGS+5])
#define BCI			(((WamWord  *) pl_reg_bank)[NB_OF_X_REGS+6])
#define LSSA			(((WamWordP *) pl_reg_bank)[NB_OF_X_REGS+7])


#define NB_OF_REGS          	11
#define NB_OF_ALLOC_REGS    	3
#define NB_OF_NOT_ALLOC_REGS	8
#define REG_BANK_SIZE       	(NB_OF_X_REGS+NB_OF_NOT_ALLOC_REGS)




#define NB_OF_USED_MACHINE_REGS 4

#ifdef ENGINE_FILE

WamWord *save_reg_bank;

WamWord pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS + 1];

char *pl_reg_tbl[] = { "TR", "B", "H", "HB1", "CP", "E", "CS", "S", "STAMP", "BCI", "LSSA"};

#else

extern WamWord *save_reg_bank;

extern WamWord pl_buff_signal_reg[];

extern char *pl_reg_tbl[];

#endif

#define Init_Reg_Bank(x)  save_reg_bank = pl_reg_bank = x


#define Reg(i)			(((i)==0) ? (WamWord) TR 	: \
				 ((i)==1) ? (WamWord) B  	: \
				 ((i)==2) ? (WamWord) H  	: \
				 ((i)==3) ? (WamWord) HB1	: \
				 ((i)==4) ? (WamWord) CP 	: \
				 ((i)==5) ? (WamWord) E  	: \
				 ((i)==6) ? (WamWord) CS 	: \
				 ((i)==7) ? (WamWord) S  	: \
				 ((i)==8) ? (WamWord) STAMP	: \
				 ((i)==9) ? (WamWord) BCI	: \
				            (WamWord) LSSA)




#define Save_All_Regs(buff_save) \
  do { \
    buff_save[0] = (WamWord) TR; \
    buff_save[1] = (WamWord) B; \
    buff_save[2] = (WamWord) H; \
    buff_save[3] = (WamWord) HB1; \
    buff_save[4] = (WamWord) CP; \
    buff_save[5] = (WamWord) E; \
    buff_save[6] = (WamWord) CS; \
    buff_save[7] = (WamWord) S; \
    buff_save[8] = (WamWord) STAMP; \
    buff_save[9] = (WamWord) BCI; \
    buff_save[10] = (WamWord) LSSA; \
  } while(0)




#define Restore_All_Regs(buff_save) \
  do { \
    TR     = (WamWordP) buff_save[0]; \
    B      = (WamWordP) buff_save[1]; \
    H      = (WamWordP) buff_save[2]; \
    HB1    = (WamWordP) buff_save[3]; \
    CP     = (WamCont ) buff_save[4]; \
    E      = (WamWordP) buff_save[5]; \
    CS     = (WamWordP) buff_save[6]; \
    S      = (WamWordP) buff_save[7]; \
    STAMP  = (WamWord ) buff_save[8]; \
    BCI    = (WamWord ) buff_save[9]; \
    LSSA   = (WamWordP) buff_save[10]; \
  } while(0)




#define Save_Machine_Regs(buff_save) \
  do { \
    buff_save[0] = (WamWord) pl_reg_bank; \
    buff_save[1] = (WamWord) TR; \
    buff_save[2] = (WamWord) B; \
    buff_save[3] = (WamWord) H; \
  } while(0)


#define Restore_Machine_Regs(buff_save) \
  do { \
    pl_reg_bank = (WamWordP) buff_save[0]; \
    TR = (WamWordP) buff_save[1]; \
    B = (WamWordP) buff_save[2]; \
    H = (WamWordP) buff_save[3]; \
  } while(0)




#define Start_Protect_Regs_For_Signal \
  do { \
    Save_Machine_Regs(pl_buff_signal_reg); \
    pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 1; \
  } while(0)


#define Stop_Protect_Regs_For_Signal \
  pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS] = 0; \


#define Restore_Protect_Regs_For_Signal \
  do { \
    if (pl_buff_signal_reg[NB_OF_USED_MACHINE_REGS]) { \
      Restore_Machine_Regs(pl_buff_signal_reg); \
      Stop_Protect_Regs_For_Signal; \
    } \
    pl_reg_bank = save_reg_bank; \
  } while(0)


   /*--- End Register Generation ---*/


#endif


/*---------------------------------*
 * Tag Descriptions                *
 *---------------------------------*/




   /*--- Begin Tag Generation ---*/

#define TAG_SIZE     		3
#define TAG_SIZE_LOW 		3
#define TAG_SIZE_HIGH		0
#define VALUE_SIZE   		61
#define TAG_MASK     		(PlULong)0x7
#define VALUE_MASK   		(PlULong)0xfffffffffffffff8
#define Tag_Mask_Of(w)		((PlLong) (w) & (TAG_MASK))
#define Tag_From_Tag_Mask(w) 	(((PlULong) (w) >> 61) | ((w) & 7))
#define Tag_Of(w)     		Tag_Mask_Of(w)
#define TAG_REF_MASK		(PlULong)0
#define TAG_LST_MASK		(PlULong)0x1
#define TAG_STC_MASK		(PlULong)0x2
#define TAG_ATM_MASK		(PlULong)0x3
#define TAG_FLT_MASK		(PlULong)0x4
#define TAG_FDV_MASK		(PlULong)0x5
#define TAG_INT_MASK		(PlULong)0x7

#define NB_OF_TAGS       	7
#define REF        		0 
#define LST        		1 
#define STC        		2 
#define ATM        		3 
#define FLT        		4 
#define FDV        		5 
#define INT        		7 

	/* General Tag/UnTag macros */

#define Tag_Long_Int(tm, v)  	((((PlLong) ((v) << 3)) >> 0) | (tm))
#define Tag_Short_Uns(tm, v)	(((PlLong) (v) << 3) + (tm))
#define Tag_Address(tm, v)  	((PlLong) (v) + (tm))

#define UnTag_Long_Int(w)    	((PlLong) ((w) << 0) >> 3)
#define UnTag_Short_Uns(w)	UnTag_Long_Int(w)
#define UnTag_Address(w)  	((WamWord *) ((w) & VALUE_MASK))


	/* Specialized Tag/UnTag macros */


#define Tag_REF(v)  		Tag_Address(TAG_REF_MASK, v)
#define Tag_LST(v)  		Tag_Address(TAG_LST_MASK, v)
#define Tag_STC(v)  		Tag_Address(TAG_STC_MASK, v)
#define Tag_ATM(v)  		Tag_Short_Uns(TAG_ATM_MASK, v)
#define Tag_FLT(v)  		Tag_Address(TAG_FLT_MASK, v)
#define Tag_FDV(v)  		Tag_Address(TAG_FDV_MASK, v)
#define Tag_INT(v)  		(((PlULong) (v) << 3) | TAG_MASK)

#define UnTag_REF(w)  		((WamWord *) (w))
#define UnTag_LST(w)  		UnTag_Address(w)
#define UnTag_STC(w)  		UnTag_Address(w)
#define UnTag_ATM(w)  		((PlULong) (w) >> 3)
#define UnTag_FLT(w)  		UnTag_Address(w)
#define UnTag_FDV(w)  		UnTag_Address(w)
#define UnTag_INT(w)  		UnTag_Long_Int(w)

#define Tag_Is_REF(w)  		(Tag_Mask_Of(w) == TAG_REF_MASK)
#define Tag_Is_LST(w)  		(Tag_Mask_Of(w) == TAG_LST_MASK)
#define Tag_Is_STC(w)  		(Tag_Mask_Of(w) == TAG_STC_MASK)
#define Tag_Is_ATM(w)  		(Tag_Mask_Of(w) == TAG_ATM_MASK)
#define Tag_Is_FLT(w)  		(Tag_Mask_Of(w) == TAG_FLT_MASK)
#define Tag_Is_FDV(w)  		(Tag_Mask_Of(w) == TAG_FDV_MASK)
#define Tag_Is_INT(w)  		(Tag_Mask_Of(w) == TAG_INT_MASK)

typedef enum
{
  LONG_INT,
  SHORT_UNS,
  ADDRESS
}TypTag;

typedef struct
{
  char *name;
  TypTag type;
  int value;
  PlLong tag_mask;
}InfTag;


#ifdef ENGINE_FILE

InfTag pl_tag_tbl[] =
{
  { "REF", ADDRESS, 0, 0},
  { "LST", ADDRESS, 1, 1},
  { "STC", ADDRESS, 2, 2},
  { "ATM", SHORT_UNS, 3, 3},
  { "FLT", ADDRESS, 4, 4},
  { "FDV", ADDRESS, 5, 5},
  { "INT", LONG_INT, 7, 7}
};

#else

extern InfTag pl_tag_tbl[];

#endif


   /*--- End Tag Generation ---*/





/*---------------------------------*
 * Stack Descriptions              *
 *---------------------------------*/

#ifndef ONLY_TAG_PART

#define KBytes_To_Wam_Words(kb)    ((1024 * kb + sizeof(WamWord) - 1) / sizeof(WamWord))

#define Wam_Words_To_KBytes(ww)    (ww * sizeof(WamWord) / 1024)

#define Local_Top                  ((B >= E) ? B : E)



   /*--- Begin Stack Generation ---*/

#include "wam_stacks.h"


   /*--- End Stack Generation ---*/


#endif
