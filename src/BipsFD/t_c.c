
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
  Vector_Empty(s1->vec);

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

  Range_From_Vector(s1);
}
