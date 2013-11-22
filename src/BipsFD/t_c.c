
#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"




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

void
Dist_LE(Range *s1, long s2, long d, long size_y)
{
  int x1 = s2 / size_y;
  int y1 = s2 % size_y;
  int x2, y2, n;
  int size_x = size_y;

  Vector_Allocate(s1->vec);
  Pl_Vector_Empty(s1->vec);

  for(x2 = x1 - d; x2 <= x1 + d; x2++)
    {
      if (x2 < 0)
	continue;
      if (x2 >= size_x)
	break;

      n = d - abs(x1 - x2);
      for(y2 = y1 - n; y2 <= y1 + n; y2++)
	if (y2 >= 0 && y2 < size_y)
	  Vector_Set_Value(s1->vec, x2 * size_y + y2);
    }

  Pl_Range_From_Vector(s1);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_I                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_I(Range *i, WamWord *l)
{
  int n = *l;                   /* I in 1..N in sparse mode */

  Range_Init_Interval(i, 1, n);
  Pl_Range_Becomes_Sparse(i);
}


/*-------------------------------------------------------------------------*
 * PL_FD_ALL_DISTINCT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_All_Distinct(WamWord **array)
{
#if 0
  WamWord *fdv_adr = array[i];

  if (Fd_Variable_Is_Ground(fdv_adr))
    return Pl_Fd_Tell_Int_Range(fdv_adr, v);

  return Pl_Fd_Tell_Range_Range(fdv_adr, v);
#endif
}
