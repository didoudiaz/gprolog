
/*-------------------------------------------------------------------------*
 * Unit Test suite for fd_range.[ch]                                       *
 * Note: There could still be problems with memory allocation, as this is  *
 * 		 done with mallocs + free                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/* Define added so that memory management can be separated */
#define USE_MALLOC_FOR_CHUNKS

// Dummy method implementations to remain separated
void Pl_Err_Resource(int atom) { }
int Pl_Create_Atom(char *str) {	return 0; }

#include <stdio.h>
#include "fd_range.c"
  	
/* Altered version of minunit unit tests */
#define unit_assert(message, test) 		\
	do { 							\
		tests_run++; 				\
		if (!(test)) 				\
			return message; 		\
	} while (0)

#define chunk_test(message, chunk, correct) \
	do { 							\
		tests_run++;			\
		test_message = check_chunk(chunk,correct);			\
		if (test_message != NULL) 			\
			return message;			\
	} while (0)

#define range_test(message, range, correct) \
	do { 							\
		tests_run++;			\
		test_message = check_range(range,correct);			\
		if (test_message != NULL) 			\
			return message;			\
	} while (0)

#define run_test(test, testname) 	\
	do { 							\
		char *message = test(); 	\
		methods_tested++; 			\
		last_test = testname;		\
		if (message) 				\
			return message; 		\
	} while (0)

int methods_tested = 0;
int tests_run = 0;
char *last_test = "-";
char *test_message = "-";

/*-------------------------------------------------------------------------*
 *  Helper methods                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/* Initializes Range with min and max bounds (Is_Interval should return 1) */
Range * create_range_old_mm(int min, int max) {
	Range *r = malloc(sizeof(Range));
	Range_Init_Interval(r,min,max);
	return r;
}

Range * create_range_old() {
	return create_range_old_mm(INTERVAL_MIN_INTEGER,INTERVAL_MAX_INTEGER);
}

/* for interval range use create_range_old_from_string("-5..5") */
/* for sparse range use create_range_old_from_string("-5..5:7:10..20") */
/* Force range to be sparse with create_range_old_from_string("-5..5:")*/
/* MIN and MAX are used to denote the bound variables*/
Range * create_range(char *str) { 

	//printf("Parsing range: %s\n", str);
	/* Assume that the range is sparse and create chunks *
	 * Set back to range if only one chunk exists        */

	int min_number = INTERVAL_MAX_INTEGER + 1;
	int value_sign = 1;
	int current_number = 0;
	int is_range_interval = 1;

	Range *range = malloc(sizeof(Range));
	Chunk *first = Pl_Create_Interval_Chunk(min_number,min_number);
	Chunk *last = first;

	char *c;
	for (c = str; *c; ++c) {
		if (*c == '-') value_sign = -1;
		else if (*c == 'M') {
			++c; ++c;
			if (*c == 'X') /* MAX */
				current_number = INTERVAL_MAX_INTEGER;
			else if (*c == 'N') /* MIN */
				current_number = INTERVAL_MIN_INTEGER;
		}
		else if (*c >= '0' && *c <= '9') {
			current_number = (current_number * 10) + (*c - '0');
		} else if (*c == '.' || *c >= ':') {
			current_number *= value_sign;
			if (*c == '.') {
				++c;
				/* set minimum */
				min_number = current_number;
			} else {
				/* Range will be sparse now */
				is_range_interval = 0;

				// An [n..n] interval
				if (min_number == INTERVAL_MAX_INTEGER + 1)
					min_number = current_number;

				/* Update the bounds of */
				last->min = min_number;
				last->max = current_number;

				/* Set min_number back to its inital value */
				min_number = INTERVAL_MAX_INTEGER + 1;

				/* update the last */
				last->next = Pl_Create_Interval_Chunk(min_number,min_number);
				((Chunk*)last->next)->prev = last;
				last = last->next;
			}
			value_sign = 1;
			current_number = 0;

		} else {
			printf("ERROR: Unable to parse range: %s\n", str);
		}
	}
	if (*--c != ':') {
		current_number *= value_sign;

		// An [n..n] interval
		if (min_number == INTERVAL_MAX_INTEGER + 1)
			min_number = current_number;

		/* Update the bounds of */
		last->min = min_number;
		last->max = current_number;

	} else {
		/* get rid of last chunk */
		last = last->prev;
		last->next = NULL;
	}

	range->min = first->min;
	range->max = last->max;

	if (!is_range_interval) {
		range->first = first;
		range->last = last;
	}
	//printf("range: (%d) [%d..%d] %s\n", Is_Sparse(range), range->min, range->max, Pl_Range_To_String(range));

  return range;
}

/* Will check if range is equal to the given string, while also */
/* checking the number of chunks, the range->[min,max] and the prev */
/* and next pointers for each chunk */
/* if the range is equal, return NULL. Otherwise return an error message */
char * check_range(Range *range, char *str) {
	static char buff[4096];

	Range *test_range = create_range(str);

	static char new_str[4096];
	sprintf(new_str,"%s",Pl_Range_To_String(test_range));

	/* NULL check */
	if (range == NULL) {
		sprintf(buff,"Range is NULL instead of %s", new_str);
	}
	
	/* One Empty, one Not Empty */
	else if (Is_Empty(range) && Is_Not_Empty(test_range)) {
		sprintf(buff,"Range is Empty(%s) instead of Not Empty(%s)", Pl_Range_To_String(range), new_str);
	} else if (Is_Not_Empty(range) && Is_Empty(test_range)) {
		sprintf(buff,"Range is Not Empty(%s) instead of Empty(%s)", Pl_Range_To_String(range), new_str);
	}
	/* Both empty is OK */
	else if (Is_Empty(range) && Is_Empty(test_range)) {
		if (range->first != NULL || range->last != NULL) {
			sprintf(buff,"Empty Range contains Chunks (%s) instead of Empty(%s)", Pl_Range_To_String(range), new_str);
		}
		return NULL;
	}

	/* One sparse, one Interval */
	else if (Is_Interval(range) && Is_Sparse(test_range)) {
		sprintf(buff,"Range is Interval(%s) instead of Sparse(%s)", Pl_Range_To_String(range), new_str);
	} else if (Is_Sparse(range) && Is_Interval(test_range)) {
		sprintf(buff,"Range is Sparse(%s) instead of Interval(%s)", Pl_Range_To_String(range), new_str);
	}
	/* Both Interval*/
	else if (Is_Interval(range) && Is_Interval(test_range)) {
		if (range->min != test_range->min || range->max != test_range->max)
			sprintf(buff,"Interval Range is %s instead of %s", Pl_Range_To_String(range), new_str);
		else 
			return NULL; /* Interval Ranges are equal */
	}

	/* Both Sparse */
	else if (Is_Sparse(range) && Is_Sparse(test_range)) {
		int failure = 0;


		/* Check if the sparse range is equal */
		Chunk *c1 = range->first;
		Chunk *c2 = test_range->first; 
		while (!failure && c1 != NULL && c2 != NULL) {
			if (c1->min != c2->min ||  c1->max != c2->max) 
				failure = 1;
			c1 = c1->next;
			c2 = c2->next;
		} 
		if (failure ||c1 != NULL || c2 != NULL) {
			sprintf(buff,"Sparse Range is %s instead of %s", Pl_Range_To_String(range), new_str);
			return buff;
		}

		/* range->last is not set up correctly */
		if (range->last->min != test_range->last->min || 
				range->last->min != test_range->last->min) {
			sprintf(buff,"Sparse Range is %s instead of %s", Pl_Range_To_String(range), new_str);
			return buff;
		}


		/* Check if the bounds of the range are updated properly*/
		if (range->min != test_range->min || range->first->min != test_range->first->min || 
				range->max != test_range->max || range->last->max != test_range->last->max) {
			sprintf(buff,"Sparse Range bounds are [%d..%d] for %s instead of [%d..%d] for %s",
				range->min, range->max, Pl_Range_To_String(range), test_range->min, test_range->max, new_str);
			return buff;
		}

		/* Check if the next and previous pointers of each Chunk are set up correctly */
		c1 = range->first;
		c2 = test_range->first; 
		while (c1 != NULL && c2 != NULL) {
			if (c1->prev != NULL && c2->prev == NULL) {
				sprintf(buff,"Chunk prev: [%d..%d]<-[%d..%d] for %s instead of NULL<-[%d..%d] for %s", 
					((Chunk*)c1->prev)->min,((Chunk*)c1->prev)->max, c1->min, c1->max, Pl_Range_To_String(range), c2->min, c2->max, new_str);
				return buff;
			} else if (c1->prev == NULL && c2->prev != NULL) {
				sprintf(buff,"Chunk prev: NULL<-[%d..%d] for %s instead of [%d..%d]<-[%d..%d] for %s", 
					c1->min, c1->max, Pl_Range_To_String(range), ((Chunk*)c2->prev)->min,((Chunk*)c2->prev)->max, c2->min, c2->max, new_str);
				return buff;
			} 
			else if (c1->prev != NULL && c2->prev != NULL) {
				if (((Chunk*)c1->prev)->min != ((Chunk*)c2->prev)->min ||
					  ((Chunk*)c1->prev)->max != ((Chunk*)c2->prev)->max) {
					sprintf(buff,"Chunk prev: [%d..%d]<-[%d..%d] for %s instead of [%d..%d]<-[%d..%d] for %s", 
						((Chunk*)c1->prev)->min,((Chunk*)c1->prev)->max, c1->min, c1->max, Pl_Range_To_String(range), 
						((Chunk*)c2->prev)->min,((Chunk*)c2->prev)->max, c2->min, c2->max, new_str);
					return buff;
				} 
			}
			c1 = c1->next;
			c2 = c2->next;
		}

		/* No problems found */
		if (!failure) {
			return NULL;
		}
	}
	else 
		sprintf(buff,"Undefined error: Range is %s instead of %s", Pl_Range_To_String(range), new_str);

	return buff;
}

/* Will check if the bounds of the chunk are correctly set */
/* if the chunk is equal, return NULL. Otherwise return an error message */
char * check_chunk(Chunk *chunk, int min, int max) {
	static char buff[4096];
	if (chunk == NULL)
		sprintf(buff,"Chunk is NULL instead of [%d..%d]", min, max);
	else if (chunk->min != min || chunk->max != max) 
		sprintf(buff,"Chunk is [%d..%d] instead of [%d..%d]", chunk->min, chunk->max, min, max);
	else return NULL;

	return buff;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Create_Interval_Chunk                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Create_Interval_Chunk() {
	Chunk *c = Pl_Create_Interval_Chunk(-1000,1000);
	unit_assert("c->min != -1000",c->min == -1000);
	unit_assert("c->max != 1000",c->max == 1000);
	unit_assert("c->next != NULL",c->next == NULL);
	unit_assert("c->prev != NULL",c->prev == NULL);
	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Becomes_Sparse                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Becomes_Sparse() {
	Range *r = create_range("-5..10");

	range_test("Initial is Interval", r, "-5..10");

	Pl_Range_Becomes_Sparse(r);
	range_test("Sparse", r, "-5..10:");

	Pl_Range_Becomes_Sparse(r);
	range_test("Sparse when already sparse", r, "-5..10:");

	r = create_range("1..0");
	Pl_Range_Becomes_Sparse(r);
	range_test("Empty Sparse", r, "1..0:");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Set_Chunk_At_End_Of_Range                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Set_Chunk_At_End_Of_Range() {

	Range *r = create_range("-5..10:");

	Pl_Set_Chunk_At_End_Of_Range(r,Pl_Create_Interval_Chunk(20,30));
	range_test("2 Chunks", r, "-5..10:20..30");

	Pl_Set_Chunk_At_End_Of_Range(r,Pl_Create_Interval_Chunk(40,50));
	range_test("3 Chunks", r, "-5..10:20..30:40..50");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Set_Chunk_Before_Chunk                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Set_Chunk_Before_Chunk() {
	Range *r = create_range("10..20:");

	Pl_Set_Chunk_Before_Chunk(r,Pl_Create_Interval_Chunk(-20,-10), r->first);
	range_test("Set before first", r, "-20..-10:10..20");

	Pl_Set_Chunk_Before_Chunk(r,Pl_Create_Interval_Chunk(0,5), r->last);
	range_test("Set before last", r, "-20..-10:0..5:10..20");

	Pl_Set_Chunk_Before_Chunk(r,Pl_Create_Interval_Chunk(-3,-3), r->first->next);
	range_test("Set before second", r, "-20..-10:-3:0..5:10..20");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Get_Chunk_For_Value                                      	   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Get_Chunk_For_Value() {
	Range *r = create_range("0..100:200..300:500..600");
	Chunk *c1 = r->first;
	Chunk *c2 = r->first->next;
	Chunk *c3 = r->last;

	/* Note: Chunks should be sorted */

	unit_assert("Pl_Get_Chunk_For_Value(r,-1000) != c1", Pl_Get_Chunk_For_Value(r,-100) == c1);
	unit_assert("Pl_Get_Chunk_For_Value(r,0) != c1", Pl_Get_Chunk_For_Value(r,0) == c1);
	unit_assert("Pl_Get_Chunk_For_Value(r,50) != c1", Pl_Get_Chunk_For_Value(r,50) == c1);
	unit_assert("Pl_Get_Chunk_For_Value(r,100) != c1", Pl_Get_Chunk_For_Value(r,100) == c1);
	unit_assert("Pl_Get_Chunk_For_Value(r,150) != c2", Pl_Get_Chunk_For_Value(r,150) == c2);
	unit_assert("Pl_Get_Chunk_For_Value(r,200) != c2", Pl_Get_Chunk_For_Value(r,200) == c2);
	unit_assert("Pl_Get_Chunk_For_Value(r,250) != c2", Pl_Get_Chunk_For_Value(r,250) == c2);
	unit_assert("Pl_Get_Chunk_For_Value(r,300) != c2", Pl_Get_Chunk_For_Value(r,300) == c2);
	unit_assert("Pl_Get_Chunk_For_Value(r,400) != c2", Pl_Get_Chunk_For_Value(r,400) == c3);
	unit_assert("Pl_Get_Chunk_For_Value(r,500) != c2", Pl_Get_Chunk_For_Value(r,500) == c3);
	unit_assert("Pl_Get_Chunk_For_Value(r,550) != c2", Pl_Get_Chunk_For_Value(r,550) == c3);
	unit_assert("Pl_Get_Chunk_For_Value(r,600) != c2", Pl_Get_Chunk_For_Value(r,600) == c3);
	unit_assert("Pl_Get_Chunk_For_Value(r,1000) != c2", Pl_Get_Chunk_For_Value(r,1000) == c3);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Test_Value (Also tests Pl_Sparse_Test_Value)              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Test_Value() {
	Range *r = create_range("0..100");

	/* Interval Chunks */
	unit_assert("Interval Test Value(r,-1000) != FALSE", Pl_Range_Test_Value(r,-100) == FALSE);
	unit_assert("Interval Test Value(r,0) != TRUE", Pl_Range_Test_Value(r,0) == TRUE);
	unit_assert("Interval Test Value(r,50) != TRUE", Pl_Range_Test_Value(r,50) == TRUE);
	unit_assert("Interval Test Value(r,100) != TRUE", Pl_Range_Test_Value(r,100) == TRUE);
	unit_assert("Interval Test Value(r,150) != FALSE", Pl_Range_Test_Value(r,150) == FALSE);

	r = create_range("0..100:200..300:500..600");

	/* Sparse Chunks */
	unit_assert("Sparse Test Value(r,-1000) != FALSE", Pl_Range_Test_Value(r,-100) == FALSE);
	unit_assert("Sparse Test Value(r,0) != TRUE", Pl_Range_Test_Value(r,0) == TRUE);
	unit_assert("Sparse Test Value(r,50) != TRUE", Pl_Range_Test_Value(r,50) == TRUE);
	unit_assert("Sparse Test Value(r,100) != TRUE", Pl_Range_Test_Value(r,100) == TRUE);
	unit_assert("Sparse Test Value(r,150) != FALSE", Pl_Range_Test_Value(r,150) == FALSE);
	unit_assert("Sparse Test Value(r,200) != TRUE", Pl_Range_Test_Value(r,200) == TRUE);
	unit_assert("Sparse Test Value(r,250) != TRUE", Pl_Range_Test_Value(r,250) == TRUE);
	unit_assert("Sparse Test Value(r,300) != TRUE", Pl_Range_Test_Value(r,300) == TRUE);
	unit_assert("Sparse Test Value(r,400) != FALSE", Pl_Range_Test_Value(r,400) == FALSE);
	unit_assert("Sparse Test Value(r,500) != TRUE", Pl_Range_Test_Value(r,500) == TRUE);
	unit_assert("Sparse Test Value(r,550) != TRUE", Pl_Range_Test_Value(r,550) == TRUE);
	unit_assert("Sparse Test Value(r,600) != TRUE", Pl_Range_Test_Value(r,600) == TRUE);
	unit_assert("Sparse Test Value(r,1000) != FALSE", Pl_Range_Test_Value(r,1000) == FALSE);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Chunk_Count						                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Chunk_Count() {
	Range *r = create_range("-4..-5");
	unit_assert("(Interval Range) Pl_Chunk_Count(r) != 0", Pl_Chunk_Count(r)==0);
	Pl_Range_Becomes_Sparse(r);
	Chunk *c0 = r->first;
	unit_assert("(Sparse Range 1) Pl_Chunk_Count(r) != 1", Pl_Chunk_Count(r)==1);
	Chunk *c1 = Pl_Create_Interval_Chunk(0,100);
	Pl_Set_Chunk_At_End_Of_Range(r,c1);
	unit_assert("(Sparse Range 2) Pl_Chunk_Count(r) != 2", Pl_Chunk_Count(r)==2);
	Chunk *c2 = Pl_Create_Interval_Chunk(200,300);
	Pl_Set_Chunk_At_End_Of_Range(r,c2);
	unit_assert("(Sparse Range 3) Pl_Chunk_Count(r) != 3", Pl_Chunk_Count(r)==3);
	Chunk *c3 = Pl_Create_Interval_Chunk(400,400);
	Pl_Set_Chunk_At_End_Of_Range(r,c3);
	unit_assert("(Sparse Range 4) Pl_Chunk_Count(r) != 4", Pl_Chunk_Count(r)==4);
	Chunk *c4 = Pl_Create_Interval_Chunk(450,449);
	Pl_Set_Chunk_At_End_Of_Range(r,c4);
	unit_assert("(Sparse Range 5) Pl_Chunk_Count(r) != 5", Pl_Chunk_Count(r)==5);
	Chunk *c5 = Pl_Create_Interval_Chunk(500,600);
	Pl_Set_Chunk_At_End_Of_Range(r,c5);
	unit_assert("(Sparse Range 6) Pl_Chunk_Count(r) != 6", Pl_Chunk_Count(r)==6);
	Chunk *c6 = Pl_Create_Interval_Chunk(700,650);
	Pl_Set_Chunk_At_End_Of_Range(r,c6);
	unit_assert("(Sparse Range 7) Pl_Chunk_Count(r) != 7", Pl_Chunk_Count(r)==7);

	/* Remove c1,c2,c3,c5 : should not change anything */
	Pl_Remove_Interval_Chunk_If_Needed(r,c1);
	Pl_Remove_Interval_Chunk_If_Needed(r,c2);
	Pl_Remove_Interval_Chunk_If_Needed(r,c3);
	Pl_Remove_Interval_Chunk_If_Needed(r,c5);
	unit_assert("(Sparse Range 7) Pl_Chunk_Count(r) != 7", Pl_Chunk_Count(r)==7);

	/* Remove c0 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c0);
	unit_assert("(removed c0) Pl_Chunk_Count(r) != 6", Pl_Chunk_Count(r)==6);

	/* Remove c4 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c4);
	unit_assert("(removed c4) Pl_Chunk_Count(r) != 5", Pl_Chunk_Count(r)==5);

	/* Remove c6 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c6);
	unit_assert("(removed c6) Pl_Chunk_Count(r) != 4", Pl_Chunk_Count(r)==4);


	/* Range r2 should become empty after removal of Chunk */
	Range *r2 = create_range("-4..-5:");
	Chunk *r2_c = r2->first;

	/* Remove r2_c (results in empty range) */
	Pl_Remove_Interval_Chunk_If_Needed(r2,r2_c);
	unit_assert("(removed r2_c) Pl_Chunk_Count(r2) != 0", Pl_Chunk_Count(r2)==0);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Remove_Interval_Chunk_If_Needed                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Remove_Interval_Chunk_If_Needed() {
	Range *r = create_range("-4..-5:0..100:200..300:400:450..449:500..600:700..650");
	Chunk *c0 = r->first;
	Chunk *c1 = c0->next;
	Chunk *c2 = c1->next;
	Chunk *c3 = c2->next;
	Chunk *c4 = c3->next;
	Chunk *c5 = c4->next;
	Chunk *c6 = c5->next;

	range_test("Initial test", r, "-4..-5:0..100:200..300:400:450..449:500..600:700..650");

	/* Remove c1,c2,c3,c5 : should not change anything */
	Pl_Remove_Interval_Chunk_If_Needed(r,c1);
	Pl_Remove_Interval_Chunk_If_Needed(r,c2);
	Pl_Remove_Interval_Chunk_If_Needed(r,c3);
	Pl_Remove_Interval_Chunk_If_Needed(r,c5);
	range_test("Nothing should be removed yet", r, "-4..-5:0..100:200..300:400:450..449:500..600:700..650");

	/* Remove c0 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c0);
	range_test("Removed c0", r, "0..100:200..300:400:450..449:500..600:700..650");

	/* Remove c4 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c4);
	range_test("Removed c4", r, "0..100:200..300:400:500..600:700..650");

	/* Remove c6 */
	Pl_Remove_Interval_Chunk_If_Needed(r,c6);
	range_test("Removed c6", r, "0..100:200..300:400:500..600");

	/* Range r2 should become empty after removal of Chunk */
	Range *r2 = create_range("-4..-5:");
	Chunk *r2_c = r2->first;

	/* Remove r2_c (results in empty range) */
	Pl_Remove_Interval_Chunk_If_Needed(r2,r2_c);
	range_test("Empty after removal of Chunk", r2, "1..0");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Set_Value (Also tests Pl_Sparse_Set_Value)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Set_Value() {

	Range *ri = create_range("20..30");
	/* Interval Tests */
	Pl_Range_Set_Value(ri,20);
	Pl_Range_Set_Value(ri,25);
	Pl_Range_Set_Value(ri,30);
	range_test("Interval in bounds", ri, "20..30");

	/* Empty range */
	ri = create_range("1..0");
	Pl_Range_Set_Value(ri,20);
	range_test("Test 1", ri, "20");


	/* Extend min max range */
	Pl_Range_Set_Value(ri,19);
	range_test("Test 2", ri, "19..20");
	Pl_Range_Set_Value(ri,21);
	range_test("Test 3", ri, "19..21");

	/* Make it sparse */
	Pl_Range_Set_Value(ri,30);
	range_test("Test 4 - Sparse", ri, "19..21:30");

	/* Sparse tests */
	Range *r = create_range("20..30:40..50:70..80");

	/* Chunk Interval representation */
	/* Initial tests */
	range_test("Initial sparse", r, "20..30:40..50:70..80");

	/* Set values at already set intervals : should not do anything*/
	Pl_Range_Set_Value(r,20);
	Pl_Range_Set_Value(r,25);
	Pl_Range_Set_Value(r,40);
	Pl_Range_Set_Value(r,45);
	Pl_Range_Set_Value(r,50);
	Pl_Range_Set_Value(r,70);
	Pl_Range_Set_Value(r,75);
	Pl_Range_Set_Value(r,80);
	range_test("Already set values", r, "20..30:40..50:70..80");

	/* Set value before min */
	Pl_Range_Set_Value(r,10);
	range_test("Set before min", r, "10:20..30:40..50:70..80");

	/* Set value after max */
	Pl_Range_Set_Value(r,100);
	range_test("Set after max", r, "10:20..30:40..50:70..80:100");

	/* Set value at middle */
	Pl_Range_Set_Value(r,55);
	range_test("Set at middle", r, "10:20..30:40..50:55:70..80:100");

	/* Extend min and max */
	Pl_Range_Set_Value(r,19);
	range_test("Extend min of chunk", r, "10:19..30:40..50:55:70..80:100");

	Pl_Range_Set_Value(r,31);
	range_test("Extend max of chunk", r, "10:19..31:40..50:55:70..80:100");


	//debug_print_chunk(r,1);

	/* Fill in hole*/
	r->first->max = 17;
	range_test("Before fill in hole", r, "10..17:19..31:40..50:55:70..80:100");
	Pl_Range_Set_Value(r,18);
	range_test("Fill in hole", r, "10..31:40..50:55:70..80:100");
	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Reset_Value (Also tests Pl_Sparse_Reset_Value)            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Reset_Value() {

	Range *ri = create_range("20..30");
	/* Interval Tests */
	Pl_Range_Reset_Value(ri,10);
	Pl_Range_Reset_Value(ri,19);
	Pl_Range_Reset_Value(ri,31);
	Pl_Range_Reset_Value(ri,40);

	range_test("Interval - Reset values out of bounds", ri, "20..30");

	/* Empty range */
	Set_To_Empty(ri);
	Pl_Range_Reset_Value(ri,1);
	range_test("Reset empty range", ri, "1..0");

	/* Make empty range */
	ri = create_range("10");
	Pl_Range_Reset_Value(ri,10);
	range_test("Reset makes range empty", ri, "1..0");

	/* Remove min max range */
	ri = create_range("20..30");
	Pl_Range_Reset_Value(ri,20);
	range_test("Interval - Reset start of range", ri, "21..30");
	Pl_Range_Reset_Value(ri,30);
	range_test("Interval - Reset end of range", ri, "21..29");

	/* Make it sparse */
	Pl_Range_Reset_Value(ri,25);
	range_test("Interval - Reset middle of range", ri, "21..24:26..29");

	/* Sparse tests */
	Range *r = create_range("20..30:40..50:70..80");

	/* Reset values outside intervals : should not do anything*/
	Pl_Range_Reset_Value(r,0);
	Pl_Range_Reset_Value(r,19);
	Pl_Range_Reset_Value(r,31);
	Pl_Range_Reset_Value(r,39);
	Pl_Range_Reset_Value(r,51);
	Pl_Range_Reset_Value(r,69);
	Pl_Range_Reset_Value(r,81);
	Pl_Range_Reset_Value(r,100);
	range_test("Sparse - Reset values out of bounds", r, "20..30:40..50:70..80");

	/* Reset value at min of chunk */
	Pl_Range_Reset_Value(r,40);
	range_test("Sparse - Reset min of chunk", r, "20..30:41..50:70..80");

	/* Reset value at last of chunk */
	Pl_Range_Reset_Value(r,50);
	range_test("Sparse - Reset max of chunk", r, "20..30:41..49:70..80");

	/* Reset value in middle of chunk */
	Pl_Range_Reset_Value(r,45);
	range_test("Sparse - Reset middle of chunk", r, "20..30:41..44:46..49:70..80");

	/* Adapt min and max */
	Pl_Range_Reset_Value(r,20);
	range_test("Sparse - Adapt min of range", r, "21..30:41..44:46..49:70..80");
	Pl_Range_Reset_Value(r,80);
	range_test("Sparse - Adapt max of range", r, "21..30:41..44:46..49:70..79");

	/* Remove chunk */
	r->first->max = 21;
	range_test("Before remove first chunk", r, "21:41..44:46..49:70..79");
	Pl_Range_Reset_Value(r,21);
	range_test("Removed first chunk", r, "41..44:46..49:70..79");

	((Chunk*)r->first->next)->min = 49;
	range_test("Before remove middle chunk", r, "41..44:49..49:70..79");
	Pl_Range_Reset_Value(r,49);
	range_test("Removed middle chunk", r, "41..44:70..79");

	r->last->min = 79;
	range_test("Before remove last chunk", r, "41..44:79");
	Pl_Range_Reset_Value(r,79);
	range_test("Removed last chunk", r, "41..44:");

	/* Remove all chunks */
	r->last->max = 41;
	r->max = 41;
	range_test("Before remove all chunks", r, "41..41:");
	Pl_Range_Reset_Value(r,41);
	range_test("Remove all chunks", r, "1..0");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Copy                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Copy() {
	Range *r = create_range("20..30");
	Range *rc = create_range("1..0"); // to prevent unitialized warning

	/* Interval range copy */
	Pl_Range_Copy(rc,r);
	range_test("Interval range copy", rc, "20..30");

	/* Copy range with one interval chunk */
	r = create_range("20..30:");
	Pl_Range_Copy(rc,r);
	range_test("1 Interval Chunk range copy", rc, "20..30:");
	unit_assert("1 Interval Chunk range copy - rc->first == r->first", rc->first != r->first);

	/* Copy range with three interval chunks */
	r = create_range("20..30:40..50:70..80");
	Pl_Range_Copy(rc,r);
	range_test("3 Chunks range copy", rc, "20..30:40..50:70..80");
	unit_assert("3 Interval Chunks range copy - rc->first == r->first", rc->first != r->first);
	unit_assert("3 Interval Chunks range copy - rc->last == r->last", rc->last != r->last);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Inter (Also tests Pl_Sparse_Inter)                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Inter() {
	Range *r = create_range("20..40");
	Range *r1 = create_range("30..50"); 

	/* Interval */
	Pl_Range_Inter(r,r1);
	range_test("Interval test 1", r, "30..40");

	r = create_range("10..20");
	r1 = create_range("30..50"); 
	Pl_Range_Inter(r,r1);
	range_test("Interval test 2 (empty)", r, "1..0");

	r = create_range("70..80");
	r1 = create_range("30..50"); 
	Pl_Range_Inter(r,r1);
	range_test("Interval test 3 (separated)", r, "1..0");

	r = create_range("10..20");
	r1 = create_range("20..50"); 
	Pl_Range_Inter(r,r1);
	range_test("Interval test 4 (Single value)", r, "20");

	r = create_range("50..70");
	r1 = create_range("20..50"); 
	Pl_Range_Inter(r,r1);
	range_test("Interval test 5 (Single value)", r, "50");

	r = create_range("1..0");
	r1 = create_range("-100..100"); 
	Pl_Range_Inter(r,r1);
	range_test("Interval test 6 (Empty)", r, "1..0");

	/* Sparse */
	/* Both have 1 interval chunk (IC) (4 combinations tested) */
	{
		r = create_range("20..30");
		r1 = create_range("50..60:"); 
		Pl_Range_Inter(r,r1);
		range_test("Interval with sparse (separated)", r, "1..0");

		r = create_range("20..30");
		r1 = create_range("20..30:"); 
		Pl_Range_Inter(r,r1);
		range_test("Interval with sparse (make sparse)",r,"20..30:");

		r = create_range("20..40:");
		r1 = create_range("30..50"); 
		Pl_Range_Inter(r,r1);
		range_test("Sparse with interval",r,"30..40:");

		r = create_range("20..30:");
		r1 = create_range("10..40:"); 
		Pl_Range_Inter(r,r1);
		range_test("Sparse with Sparse - r1 surounds r",r,"20..30:");

		r = create_range("20..30:");
		r1 = create_range("22..28:"); 
		Pl_Range_Inter(r,r1);
		range_test("Sparse with Sparse - r surounds r1",r,"22..28:");

		r = create_range("20..30:");
		r1 = create_range("22..40:"); 
		Pl_Range_Inter(r,r1);
		range_test("Sparse with Sparse - Test 1",r,"22..30:");

		r = create_range("20..30:");
		r1 = create_range("10..28:"); 
		Pl_Range_Inter(r,r1);
		range_test("Sparse with Sparse - Test 2",r,"20..28:");
	}

	r = create_range("-1000..1000:");
	r1 = create_range("20..30:40..50:70..80"); 
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 3",r,"20..30:40..50:70..80");
	unit_assert("Sparse with Sparse - Test 3 - r->first == r1->first", r->first != r1->first);
	unit_assert("Sparse with Sparse - Test 3 - r->last == r1->last", r->last != r1->last);

	r = create_range("20..30:40..50:70..80"); 
	r1 = create_range("-1000..1000:");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 4",r,"20..30:40..50:70..80");
	unit_assert("Sparse with Sparse - Test 4 - r->first == r1->first", r->first != r1->first);
	unit_assert("Sparse with Sparse - Test 4 - r->last == r1->last", r->last != r1->last);

	/* Some other tricky situations */

	r = create_range("20..30:40..50:70..80:100..1000"); 
	r1 = create_range("-1000..25:30..45:47..120");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 5",r,"20..25:30:40..45:47..50:70..80:100..120");

	r = create_range("1:4:9:12"); 
	r1 = create_range("-1000..1000");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with interval - Test 6",r,"1:4:9:12");

	r = create_range("17..25:30..40"); 
	r1 = create_range("3..11:");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 7",r,"1..0");

	// Remove chunks but don't remove too much
	r = create_range("17..25:30..40"); 
	r1 = create_range("3..11:20..40");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 8",r,"20..25:30..40");

	r = create_range("0..7:9..17:19..27:29..268435455"); 
	r1 = create_range("6..8:10:12:14..15:17..19");
	Pl_Range_Inter(r,r1);
	range_test("Sparse with Sparse - Test 9",r,"6..7:10:12:14..15:17:19");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Union (Also tests Pl_Sparse_Union)                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Union() {
	Range *r = create_range("20..40");
	Range *r1 = create_range("30..50"); 

	/* Interval */
	Pl_Range_Union(r,r1);
	range_test("Interval test 1", r, "20..50");

	r = create_range("20..50");
	r1 = create_range("51..60"); 
	Pl_Range_Union(r,r1);
	range_test("Interval test 2", r, "20..60");

	r = create_range("20..50");
	r1 = create_range("10..19"); 
	Pl_Range_Union(r,r1);
	range_test("Interval test 3", r, "10..50");

	r = create_range("10..20");
	r1 = create_range("30..40"); 
	Pl_Range_Union(r,r1);
	range_test("Interval test 4", r, "10..20:30..40");

	/* Sparse */
	/* Both have 1 interval chunk (IC) (4 combinations tested) */
	r = create_range("20..30");
	r1 = create_range("50..60:"); 
	Pl_Range_Union(r,r1);
	range_test("Interval Sparse", r, "20..30:50..60");

	r = create_range("20..40:");
	r1 = create_range("10..50"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Interval", r, "10..50:");

	r = create_range("20..30:");
	r1 = create_range("10..40:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 1", r, "10..40:");

	r = create_range("20..40:");
	r1 = create_range("10..30:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 2", r, "10..40:");

	r = create_range("10..40:");
	r1 = create_range("20..30:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 3", r, "10..40:");

	r = create_range("50..60:");
	r1 = create_range("20..30:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 4", r, "20..30:50..60");
	
	/* Combine two ICs */
	r = create_range("20..30:50..60");
	r1 = create_range("10..70:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 5", r, "10..70:");

	r = create_range("10..30:50..70");
	r1 = create_range("31..49:"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 6", r, "10..70:");

	/* Some tricky cases */
	r = create_range("-10..0:10..30:40..60:70..80:90..100:111..130:140..150");
	r1 = create_range("-30..-20:20..50:70..110:120..140:170..180:190..200"); 
	Pl_Range_Union(r,r1);
	range_test("Sparse Sparse - test 7", r, "-30..-20:-10..0:10..60:70..150:170..180:190..200");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Compl (Also tests Pl_Sparse_Compl)                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Compl() {
	Range *r = create_range("MIN..MAX");
	
	/* NB: the complement of a complement should be the original range again,*
	 * However, the range might have become sparse (with one chunk) so it is *
	 * not exactly the same range. The bounds should be the same though      */

	/* Interval */
	// MIN_INT..MAX_INT
	Pl_Range_Compl(r);
	range_test("MIN..MAX - Test 1", r, "1..0");
	Pl_Range_Compl(r);
	range_test("MIN..MAX - Test 2", r, "MIN..MAX");

	r = create_range("MIN..40");
	Pl_Range_Compl(r);
	range_test("MIN..40 - Test 1", r, "41..MAX");
	Pl_Range_Compl(r);
	range_test("MIN..40 - Test 2", r, "MIN..40");

	r = create_range("40..MAX");
	Pl_Range_Compl(r);
	range_test("40..MAX - Test 1", r, "MIN..39");
	Pl_Range_Compl(r);
	range_test("40..MAX - Test 2", r, "40..MAX");

	// X..Y
	r = create_range("20..30");
	Pl_Range_Compl(r);
	range_test("20..30 - Test 1", r, "MIN..19:31..MAX");
	Pl_Range_Compl(r);
	range_test("20..30 - Test 2", r, "20..30:");

	// X
	r = create_range("20");
	Pl_Range_Compl(r);
	range_test("20 - Test 1", r, "MIN..19:21..MAX");
	Pl_Range_Compl(r);
	range_test("20 - Test 2", r, "20:");

	/* Sparse */

	// X
	r = create_range("20:");
	Pl_Range_Compl(r);
	range_test("20 - Test 1", r, "MIN..19:21..MAX");
	Pl_Range_Compl(r);
	range_test("20 - Test 2", r, "20:");

	// [MIN_INT..MAX_INT]
	Set_To_Empty(r);
	r = create_range("MIN..MAX:");
	Pl_Range_Compl(r);
	range_test("Sparse MIN..MAX - Test 1", r, "1..0");
	Pl_Range_Compl(r);
	range_test("Sparse MIN..MAX - Test 2", r, "MIN..MAX");

	// [MIN_INT..X]
	r = create_range("MIN..40:");
	Pl_Range_Compl(r);
	range_test("Sparse MIN..40 - Test 1", r, "41..MAX:");
	Pl_Range_Compl(r);
	range_test("Sparse MIN..40 - Test 2", r, "MIN..40:");

	// [X..MAX_INT]
	r = create_range("40..MAX:");
	Pl_Range_Compl(r);
	range_test("Sparse 40..MAX - Test 1", r, "MIN..39:");
	Pl_Range_Compl(r);
	range_test("Sparse 40..MAX - Test 2", r, "40..MAX:");

	// X..Y
	r = create_range("20..30:");
	Pl_Range_Compl(r);
	range_test("Sparse 20..30 - Test 1", r, "MIN..19:31..MAX");
	Pl_Range_Compl(r);
	range_test("Sparse 20..30 - Test 2", r, "20..30:");

	// [X..Y][X..Y][X..Y]
	r = create_range("20..30:40..50:70..80");
	Pl_Range_Compl(r);
	range_test("Sparse 1 - Test 1", r, "MIN..19:31..39:51..69:81..MAX");
	Pl_Range_Compl(r);
	range_test("Sparse 1 - Test 2", r, "20..30:40..50:70..80");

	r = create_range("MIN..30:40..50:70..80:90..MAX");
	Pl_Range_Compl(r);
	range_test("Sparse 2 - Test 1", r, "31..39:51..69:81..89");
	Pl_Range_Compl(r);
	range_test("Sparse 2 - Test 2", r, "MIN..30:40..50:70..80:90..MAX");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Nb_Elem (Also tests Pl_Sparse_Nb_Elem)                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Nb_Elem() {
	// Interval
	Range *r = create_range("0..9");
	unit_assert("(0..9) !Pl_Range_Nb_Elem(r) != 10", Pl_Range_Nb_Elem(r) == 10);
	r = create_range("-10..9");
	unit_assert("(-10..9) !Pl_Range_Nb_Elem(r) != 20", Pl_Range_Nb_Elem(r) == 20);
	r = create_range("-10..-9");
	unit_assert("(-10..-9) !Pl_Range_Nb_Elem(r) != 2", Pl_Range_Nb_Elem(r) == 2);
	r = create_range("-10");
	unit_assert("(-10..-10) !Pl_Range_Nb_Elem(r) != 1", Pl_Range_Nb_Elem(r) == 1);
	r = create_range("9..10");
	unit_assert("(9..10) !Pl_Range_Nb_Elem(r) != 2", Pl_Range_Nb_Elem(r) == 2);
	r = create_range("10");
	unit_assert("(10..10) !Pl_Range_Nb_Elem(r) != 1", Pl_Range_Nb_Elem(r) == 1);
	r = create_range("10..-10");
	unit_assert("(10..-10) !Pl_Range_Nb_Elem(r) != 0", Pl_Range_Nb_Elem(r) == 0);
	r = create_range("1..0");
	unit_assert("(10..10) !Pl_Range_Nb_Elem(r) != 0", Pl_Range_Nb_Elem(r) == 0);
	r = create_range("MIN..MAX");
	unit_assert("(MIN..MAX) !Pl_Range_Nb_Elem(r) != MAX - (MIN) + 1", 
		Pl_Range_Nb_Elem(r) == INTERVAL_MAX_INTEGER - (INTERVAL_MIN_INTEGER) + 1);
	// for some reason, there need to be brackets around INTERVAL_MIN_INTEGER

	// Sparse

	// 1 Chunk
	r = create_range("0..9:");
	unit_assert("([0..9]) !Pl_Range_Nb_Elem(r) != 10", Pl_Range_Nb_Elem(r) == 10);
	r = create_range("-10..9:");
	unit_assert("([-10..9]) !Pl_Range_Nb_Elem(r) != 20", Pl_Range_Nb_Elem(r) == 20);
	r = create_range("-10..-9:");
	unit_assert("([-10..-9]) !Pl_Range_Nb_Elem(r) != 2", Pl_Range_Nb_Elem(r) == 2);
	r = create_range("-10:");
	unit_assert("([-10..-10]) !Pl_Range_Nb_Elem(r) != 1", Pl_Range_Nb_Elem(r) == 1);
	r = create_range("9..10:");
	unit_assert("([9..10]) !Pl_Range_Nb_Elem(r) != 2", Pl_Range_Nb_Elem(r) == 2);
	r = create_range("10:");
	unit_assert("([10]) !Pl_Range_Nb_Elem(r) != 1", Pl_Range_Nb_Elem(r) == 1);
	r = create_range("10..-10:");
	unit_assert("([10]) !Pl_Range_Nb_Elem(r) != 0", Pl_Range_Nb_Elem(r) == 0);
	r = create_range("MIN..MAX:");
	unit_assert("([MIN..MAX]) !Pl_Range_Nb_Elem(r) != MAX - (MIN) + 1", 
		Pl_Range_Nb_Elem(r) == INTERVAL_MAX_INTEGER - (INTERVAL_MIN_INTEGER) + 1);

	// Multiple Chunks
	r = create_range("10..20:30..40");
	unit_assert("([10..20][30..40]) !Pl_Range_Nb_Elem(r) != 22", Pl_Range_Nb_Elem(r) == 22);
	r = create_range("10..20:30..40:50..60");
	unit_assert("([10..20][30..40][50..60]) !Pl_Range_Nb_Elem(r) != 33", Pl_Range_Nb_Elem(r) == 33);
	r = create_range("10..20:30..40:50..60:70");
	unit_assert("([10..20][30..40][50..60][70]) !Pl_Range_Nb_Elem(r) != 34", Pl_Range_Nb_Elem(r) == 34);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Mul_Value (Also tests Pl_Sparse_Mul_Value)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Mul_Value() {
	Range *r = create_range("-5..5");

	Pl_Range_Mul_Value(r,1);
	range_test("(-5..5)*1", r, "-5..5");

	r = create_range("-5..5");
	Pl_Range_Mul_Value(r,2);
	range_test("(-5..5)*2", r, "-10:-8:-6:-4:-2:0:2:4:6:8:10");

	r = create_range("-5..5");
	Pl_Range_Mul_Value(r,3);
	range_test("(-5..5)*3", r, "-15:-12:-9:-6:-3:0:3:6:9:12:15");

	r = create_range("-15:-12:-9:-6:-3:0:3:6:9:12:15");
	Pl_Range_Mul_Value(r,2);
	range_test("((-5..5)*3)*2", r, "-30:-24:-18:-12:-6:0:6:12:18:24:30");

	// prevent overflow
	int number = (INTERVAL_MAX_INTEGER) / 2 - 4;
	Set_To_Empty(r);
	r->min = number;
	r->max = INTERVAL_MAX_INTEGER;
	Pl_Range_Mul_Value(r,2);
	range_test("(MAX/2-4..MAX)*2", r, "268435446:268435448:268435450:268435452:268435454");

	// prevent underflow
	number = ((INTERVAL_MIN_INTEGER) / 2) + 4;
	Set_To_Empty(r);
	r->min = INTERVAL_MIN_INTEGER;
	r->max = number;
	Pl_Range_Mul_Value(r,2);
	range_test("(MIN..MIN/2+4)*2", r, "-268435454:-268435452:-268435450:-268435448:-268435446");

	// two separate chunks
	r = create_range("20..22:30:100..101");
	Pl_Range_Mul_Value(r,5);
	range_test("[20..22][30][100..101]*2", r, "100:105:110:150:500:505");

	r = create_range("0..3:5");
	Pl_Range_Mul_Value(r,-2);
	range_test("(0..3:5)*2", r, "-10:-6:-4:-2:0");

	r = create_range("-4:0..3:5");
	Pl_Range_Mul_Value(r,-2);
	range_test("(-4:0..3:5)*2", r, "-10:-6:-4:-2:0:8");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Div_Value (Also tests Pl_Sparse_Div_Value)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Div_Value() {

	/* Interval */
	Range *r = create_range("-15..5");
	Pl_Range_Div_Value(r,3);
	range_test("(-15..5)/3", r , "-5..1");

	r = create_range("-15..5");
	Pl_Range_Div_Value(r,-3);
	range_test("(-15..5)/-3", r , "-1..5");

	r = create_range("-15..-2");
	Pl_Range_Div_Value(r,4);
	range_test("(-15..-2)/4", r , "-3..-1");

	r = create_range("10..13");
	Pl_Range_Div_Value(r,7);
	range_test("(10..13)/7", r , "1..0");
	
	/* Sparse */
	r = create_range("-5..5");
	Pl_Range_Mul_Value(r,2);
	Pl_Range_Div_Value(r,2);
	range_test("((-5..5)*2)/2", r , "-5..5:");

	r = create_range("-5..5");
	Pl_Range_Mul_Value(r,2);
	Pl_Range_Div_Value(r,3);
	range_test("((-5..5)*2)/3", r , "-2:0:2");

	r = create_range("-5..5");
	Pl_Range_Div_Value(r,0);
	range_test("(-5..5)/0", r , "1..0");

	r = create_range("2:6:10");
	Pl_Range_Div_Value(r,2);
	range_test("(2:6:10)/2", r , "1:3:5");

	r = create_range("2:4:6:8:11..21");
	Pl_Range_Div_Value(r,-2);
	range_test("(2:4:6:8:11..21)/-2", r , "-10..-6:-4..-1");

	r = create_range("2:6:10");
	Pl_Range_Div_Value(r,-2);
	range_test("(2:6:10)/-2", r , "-5:-3:-1");

	// rounding
	r = create_range("-11..-5:5..11");
	Pl_Range_Div_Value(r,2);
	range_test("(-11..-5:5..11)/2", r , "-5..-3:3..5");

	r = create_range("-11..-5:5..11");
	Pl_Range_Div_Value(r,-2);
	range_test("(-11..-5:5..11)/-2", r , "-5..-3:3..5");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Add_Value (Also tests Pl_Sparse_Add_Value)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Add_Value() {
	/* Interval */
	Range *r = create_range("20..30");
	
	Pl_Range_Add_Value(r,2);
	range_test("(20..30)+2", r, "22..32");

	r = create_range("20..30");
	Pl_Range_Add_Value(r,-2);
	range_test("(20..30)-2", r, "18..28");

	r = create_range("20..30");
	Pl_Range_Add_Value(r,-100);
	range_test("(20..30)-100", r, "-80..-70");

	r = create_range("20..30");
	Pl_Range_Add_Value(r,INTERVAL_MAX_INTEGER);
	range_test("(20..30)+MAX", r, "1..0");

	r = create_range("-30..-20");
	Pl_Range_Add_Value(r,INTERVAL_MIN_INTEGER);
	range_test("(-30..-20)+MIN", r, "1..0");

	r = create_range("-20..30");
	Pl_Range_Add_Value(r,INTERVAL_MAX_INTEGER);
	range_test("(-20..30)+MAX", r, "268435435..268435455");

	r = create_range("-20..30");
	Pl_Range_Add_Value(r,INTERVAL_MIN_INTEGER);
	range_test("(-20..30)+MIN", r, "-268435455..-268435425");

	/* Sparse 1 chunk */
	r = create_range("20..30:");
	Pl_Range_Add_Value(r,2);
	range_test("[20..30]+2", r, "22..32:");

	r = create_range("20..30:");
	Pl_Range_Add_Value(r,-2);
	range_test("[20..30]-2", r, "18..28:");

	r = create_range("20..30:");
	Pl_Range_Add_Value(r,-100);
	range_test("[20..30]-100", r, "-80..-70:");

	r = create_range("20..30:");
	Pl_Range_Add_Value(r,INTERVAL_MAX_INTEGER);
	range_test("[20..30]+MAX", r, "1..0");

	r = create_range("-30..-20:");
	Pl_Range_Add_Value(r,INTERVAL_MIN_INTEGER);
	range_test("[-30..-20]+MIN", r, "1..0");

	r = create_range("-20..30:");
	Pl_Range_Add_Value(r,INTERVAL_MAX_INTEGER);
	range_test("[-20..30]+MAX", r, "268435435..268435455:");

	r = create_range("-20..30:");
	Pl_Range_Add_Value(r,INTERVAL_MIN_INTEGER);
	range_test("[-20..30]+MIN", r, "-268435455..-268435425:");
	

	// tricky cases
	r = create_range("-100..-80:-50..10:100..200:268435355..268435405:268435425..268435435");
	Pl_Range_Add_Value(r,50);
	range_test("Chunks test", r, "-50..-30:0..60:150..250:268435405..268435455");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Mod_Value (Also tests Pl_Sparse_Mod_Value)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Mod_Value() {
	
	Range *r = create_range("20..30");

	/* Interval */
	Pl_Range_Mod_Value(r,5);
	range_test("(20..30) mod 5", r, "0..4");

	r = create_range("0..4");
	Pl_Range_Mod_Value(r,0);
	range_test("(0..4) mod 0", r, "1..0");

	r = create_range("1..0");
	Pl_Range_Mod_Value(r,5);
	range_test("Empty mod 5", r, "1..0");

	r = create_range("8..11");
	Pl_Range_Mod_Value(r,-5);
	range_test("(8..11) mod -5", r, "-4:-2..0");

	r = create_range("-11..-8");
	Pl_Range_Mod_Value(r,-5);
	range_test("(-11..-8) mod -5", r, "-4..-3:-1..0");

	r = create_range("-11..-8");
	Pl_Range_Mod_Value(r,5);
	range_test("(-11..-8) mod 5", r, "0..2:4");

	r = create_range("-11..-8");
	Pl_Range_Mod_Value(r,5);
	range_test("(-11..-8) mod 5", r, "0..2:4");

	r = create_range("1..100");
	Pl_Range_Mod_Value(r,5);
	range_test("(1..100) mod 5", r, "0..4");

	/* Sparse */

	r = create_range("0..4:");
	Pl_Range_Mod_Value(r,5);
	range_test("(0..4:) mod 5", r, "0..4:");

	r = create_range("0:2:4");
	Pl_Range_Mod_Value(r,5);
	range_test("(0:2:4) mod 5", r, "0:2:4");

	r = create_range("1..3:5..9");
	Pl_Range_Mod_Value(r,10);
	range_test("(1..3:5..9) mod 10", r, "1..3:5..9");

	r = create_range("9..13:15");
	Pl_Range_Mod_Value(r,10);
	range_test("(10..13:15) mod 10", r, "0..3:5:9");

	r = create_range("9..13:15:16..18:24");
	Pl_Range_Mod_Value(r,10);
	range_test("(9..13:15:16..18:24) mod 10", r, "0..9:");

	r = create_range("-11..-8:");
	Pl_Range_Mod_Value(r,5);
	range_test("(-11..-8:) mod 5", r, "0..2:4");

	r = create_range("-1..1:");
	Pl_Range_Mod_Value(r,-5);
	range_test("(-1..1) mod (-5)", r, "-4:-1..0");

	return 0;
}



/*-------------------------------------------------------------------------*
 * test_Pl_Range_Add_Range (Also tests Pl_Sparse_Add_Range)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Add_Range() {
	
	Range *r = create_range("20..30");
	Range *r1 = create_range("1..5");

	/* Interval */
	Pl_Range_Add_Range(r,r1);
	range_test("(20..30) + (1..5)", r, "21..35");

	r = create_range("1..5");
	r1 = create_range("100..500");
	Pl_Range_Add_Range(r,r1);
	range_test("(1..5) + (100..500)", r, "101..505");

	r = create_range("1..5");
	r1 = create_range("100");
	Pl_Range_Add_Range(r,r1);
	range_test("(1..5) + (100)", r, "101..105");

	r = create_range("100");
	r1 = create_range("1..5");
	Pl_Range_Add_Range(r,r1);
	range_test("(100) + (1..5)", r, "101..105");

	r = create_range("1..0");
	r1 = create_range("100");
	Pl_Range_Add_Range(r,r1);
	range_test("(Empty) + (100)", r, "1..0");

	r = create_range("-10..-5");
	r1 = create_range("-20..10");
	Pl_Range_Add_Range(r,r1);
	range_test("(-10..-5) + (-20..10)", r, "-30..5");

	/* Sparse */

	r = create_range("20..30:");
	r1 = create_range("1..5:");
	Pl_Range_Add_Range(r,r1);
	range_test("(20..30:) + (1..5:)", r, "21..35:");

	r = create_range("1..5:");
	r1 = create_range("100..500:");
	Pl_Range_Add_Range(r,r1);
	range_test("(1..5:) + (100..500:)", r, "101..505:");

	r = create_range("1..5:");
	r1 = create_range("100:");
	Pl_Range_Add_Range(r,r1);
	range_test("(1..5:) + (100:)", r, "101..105:");

	r = create_range("100:");
	r1 = create_range("1..5:");
	Pl_Range_Add_Range(r,r1);
	range_test("(100:) + (1..5:)", r, "101..105:");

	r = create_range("-10..-5:");
	r1 = create_range("-20..10:");
	Pl_Range_Add_Range(r,r1);
	range_test("(-10..-5:) + (-20..10:)", r, "-30..5:");

	r = create_range("1:3:5:7");
	r1 = create_range("2..3:");
	Pl_Range_Add_Range(r,r1);
	range_test("(1:3:5:7) + (2..3:)", r, "3..10:");

	r = create_range("10..20:40..50");
	r1 = create_range("2..5:8..10");
	Pl_Range_Add_Range(r,r1);
	range_test("(10..20:40..50) + (2..5:8..10)", r, "12..30:42..60");

	r = create_range("10..20:30..50");
	r1 = create_range("2..5:8..12");
	Pl_Range_Add_Range(r,r1);
	range_test("(10..20:30..50) + (2..5:8..12)", r, "12..62:");

	r = create_range("1:3");
	r1 = create_range("-2..-1:");
	Pl_Range_Add_Range(r,r1);
	range_test("(1:3) + (-2..-1:)", r, "-1..2:");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Sub_Range (Also tests Pl_Sparse_Sub_Range)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Sub_Range() {
	
	Range *r = create_range("20..30");
	Range *r1 = create_range("1..5");

	/* Interval */
	Pl_Range_Sub_Range(r,r1);
	range_test("(20..30) - (1..5)", r, "15..29");

	r = create_range("1..5");
	r1 = create_range("100..500");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1..5) - (100..500)", r, "-499..-95");

	r = create_range("1..5");
	r1 = create_range("100");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1..5) - (100)", r, "-99..-95");

	r = create_range("100");
	r1 = create_range("1..5");
	Pl_Range_Sub_Range(r,r1);
	range_test("(100) - (1..5)", r, "95..99");

	r = create_range("1..0");
	r1 = create_range("100");
	Pl_Range_Sub_Range(r,r1);
	range_test("(Empty) - (100)", r, "1..0");

	r = create_range("-10..-5");
	r1 = create_range("-20..10");
	Pl_Range_Sub_Range(r,r1);
	range_test("(-10..-5) - (-20..10)", r, "-20..15");

	/* Sparse */

	r = create_range("20..30:");
	r1 = create_range("1..5:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(20..30:) - (1..5:)", r, "15..29:");

	r = create_range("1..5:");
	r1 = create_range("100..500:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1..5:) - (100..500:)", r, "-499..-95:");

	r = create_range("1..5:");
	r1 = create_range("100:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1..5:) - (100:)", r, "-99..-95:");

	r = create_range("100:");
	r1 = create_range("1..5:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(100:) - (1..5:)", r, "95..99:");

	r = create_range("-10..-5:");
	r1 = create_range("-20..10:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(-10..-5:) - (-20..10:)", r, "-20..15:");

	r = create_range("1:3:5:7");
	r1 = create_range("2..3:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1:3:5:7) - (2..3:)", r, "-2..5:");

	r = create_range("10..20:40..50");
	r1 = create_range("2..5:8..10");
	Pl_Range_Sub_Range(r,r1);
	range_test("(10..20:40..50) - (2..5:8..10)", r, "0..18:30..48");

	r = create_range("10..20:30..50");
	r1 = create_range("2..5:8..12");
	Pl_Range_Sub_Range(r,r1);
	range_test("(10..20:30..50) - (2..5:8..12)", r, "-2..48:");

	r = create_range("1:3");
	r1 = create_range("-2..-1:");
	Pl_Range_Sub_Range(r,r1);
	range_test("(1:3) - (-2..-1:)", r, "2..5:");

	return 0;
}




/*-------------------------------------------------------------------------*
 * test_Pl_Range_Mul_Range (Also tests Pl_Sparse_Mul_Range)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Mul_Range() {
	Range *r = create_range("-5..5");
	Range *r1 = create_range("1");

	Pl_Range_Mul_Range(r,r1);
	range_test("(-5..5) * (1)", r, "-5..5:");

	r = create_range("-5..5");
	r1 = create_range("1..0");
	Pl_Range_Mul_Range(r,r1);
	range_test("(-5..5) * (Empty)", r, "1..0");

	r = create_range("1..3:5..10");
	r1 = create_range("2..3");
	Pl_Range_Mul_Range(r,r1);
	range_test("(1..3:5..10) * (2..3)", r, "2..4:6:9..10:12:14..16:18:20..21:24:27:30");

	r = create_range("0..3:5");
	r1 = create_range("-2:2");
	Pl_Range_Mul_Range(r,r1);
	range_test("(1..3:5) * (-2:2)", r, "-10:-6:-4:-2:0:2:4:6:10");

	r = create_range("-5:-3..-1:1:5:7:8");
	r1 = create_range("-3..-2:1");
	Pl_Range_Mul_Range(r,r1);
	range_test("(-5:-3..-1:1:5:7:8) * (-3..-2:1)", r, "-24:-21:-16..-14:-10:-5:-3..-1:1..10:15");


	return 0;
}



/*-------------------------------------------------------------------------*
 * test_Pl_Range_Div_Range (Also tests Pl_Sparse_Div_Range)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Div_Range() {
	Range *r = create_range("-5..5");
	Range *r1 = create_range("1..5");

	Pl_Range_Div_Range(r,r1);
	range_test("(-5..5) / (1)", r, "-5..5:");

	r = create_range("-5..5");
	r1 = create_range("1..0");
	Pl_Range_Div_Range(r,r1);
	range_test("(-5..5) / (Empty)", r, "1..0");

	r = create_range("2..4:6:9..10:12:14..16:18:20..21:24:27:30");
	r1 = create_range("2..3");
	Pl_Range_Div_Range(r,r1);
	range_test("(2..4:6:9..10:12:14..16:18:20..21:24:27:30) / (2..3)", r, "1..10:12:15");

	r = create_range("-10:-6:-4:-2:0:2:4:6:10");
	r1 = create_range("-2:2");
	Pl_Range_Div_Range(r,r1);
	range_test("(-10:-6:-4:-2:0:2:4:6:10) / (-2:2)", r, "-5:-3..3:5");

	r = create_range("-24:-21:-16..-14:-10:-5:-3..-1:1..10:15");
	r1 = create_range("2..3");
	Pl_Range_Div_Range(r,r1);
	range_test("(-24:-21:-16..-14:-10:-5:-3..-1:1..10:15) / (2..3)", r, "-12:-8..-7:-5:-1:1..5");

	r = create_range("-24:-21:-16..-14:-10:-5:-3..-1:1..10:15");
	r1 = create_range("-3..-2");
	Pl_Range_Div_Range(r,r1);
	range_test("(-24:-21:-16..-14:-10:-5:-3..-1:1..10:15) / (-3..-2)", r, "-5..-1:1:5:7..8:12");

	r = create_range("-100..100");
	r1 = create_range("-10..10");
	Pl_Range_Div_Range(r,r1);
	range_test("(1..100) / (1..10)", r, "-100..100:");

	return 0;
}



/*-------------------------------------------------------------------------*
 * test_Pl_Range_Mod_Range (Also tests Pl_Sparse_Mod_Range)                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Mod_Range() {
	Range *r = create_range("-5..5");
	Range *r1 = create_range("1..5");

	Pl_Range_Mod_Range(r,r1);
	range_test("(-5..5) mod (1..5)", r, "0..4:");

	r = create_range("-1..1");
	r1 = create_range("-5");
	Pl_Range_Mod_Range(r,r1);
	range_test("(-1..1) mod (-5)", r, "-4:-1..0");

	r = create_range("-1..1");
	r1 = create_range("-5:5");
	Pl_Range_Mod_Range(r,r1);
	range_test("(-1..1) mod (-5:5)", r, "-4:-1..1:4");

	r = create_range("-1..1");
	r1 = create_range("-5..5");
	Pl_Range_Mod_Range(r,r1);
	range_test("(-1..1) mod (-5..5)", r, "-4..4:");

	r = create_range("30..35");
	r1 = create_range("15:17");
	Pl_Range_Mod_Range(r,r1);
	range_test("(30..35) mod (15:17)", r, "0..5:13..16");

	return 0;
}






/*-------------------------------------------------------------------------*
 * test_Pl_Range_Ith_Elem (Also tests Pl_Sparse_Ith_Elem)                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Ith_Elem() {
	int fail = INTERVAL_MIN_INTEGER-1;

	/* Interval */
	Range *r = create_range("20..30");
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,-5) != MIN-1", Pl_Range_Ith_Elem(r,-5) == fail);
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,0) != MIN-1", Pl_Range_Ith_Elem(r,0) == fail);
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,12) != MIN-1", Pl_Range_Ith_Elem(r,12) == fail);
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,1) != 20", Pl_Range_Ith_Elem(r,1) == 20);
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,5) != 24", Pl_Range_Ith_Elem(r,5) == 24);
	unit_assert("((20..30)) Pl_Range_Ith_Elem(r,11) != 30", Pl_Range_Ith_Elem(r,11) == 30);

	/* Sparse */
	r = create_range("20..30:");
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,-5) != MIN-1", Pl_Range_Ith_Elem(r,-5) == fail);
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,0) != MIN-1", Pl_Range_Ith_Elem(r,0) == fail);
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,12) != MIN-1", Pl_Range_Ith_Elem(r,12) == fail);
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,1) != 20", Pl_Range_Ith_Elem(r,1) == 20);
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,5) != 24", Pl_Range_Ith_Elem(r,5) == 24);
	unit_assert("([20..30]) Pl_Range_Ith_Elem(r,11) != 30", Pl_Range_Ith_Elem(r,11) == 30);

	// multiple chunks
	r = create_range("20..30:40:60..70");
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,0) != MIN-1", Pl_Range_Ith_Elem(r,0) == fail);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,24) != MIN-1", Pl_Range_Ith_Elem(r,24) == fail);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,1) != 20", Pl_Range_Ith_Elem(r,1) == 20);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,11) != 30", Pl_Range_Ith_Elem(r,11) == 30);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,12) != 40", Pl_Range_Ith_Elem(r,12) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,13) != 60", Pl_Range_Ith_Elem(r,13) == 60);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,18) != 65", Pl_Range_Ith_Elem(r,18) == 65);
	unit_assert("([20..30][40][60..70]) Pl_Range_Ith_Elem(r,23) != 70", Pl_Range_Ith_Elem(r,23) == 70);

	Set_To_Empty(r);
	unit_assert("(Empty) Pl_Range_Ith_Elem(r,1) != MIN-1", Pl_Range_Ith_Elem(r,1) == fail);

	return 0;
}


/*-------------------------------------------------------------------------*
 * test_Pl_Range_Test_Null_Inter (Also tests Pl_Sparse_*_Test_Null_Inter)  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Test_Null_Inter() {
	Range *r = create_range("20..30");
	Range *r1 = create_range("0..10");

	/* Interval */
	unit_assert("(20..30) !null inter (0..10) 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter (0..10) 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->max = 19;
	unit_assert("(20..30) !null inter (0..19) 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter (0..19) 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->max = 20;
	unit_assert("(20..30) null inter (0..20) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (0..20) 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = 20;
	unit_assert("(20..30) null inter (20) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (20) 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->max = 30;
	unit_assert("(20..30) null inter (20..30) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (20..30) 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->max = 50;
	unit_assert("(20..30) null inter (20..50) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (20..50) 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = 30;
	unit_assert("(20..30) null inter (30..50) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (30..50) 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = 31;
	unit_assert("(20..30) !null inter (31..50) 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter (31..50) 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->min = 25;
	unit_assert("(20..30) null inter (25..50) 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter (25..50) 2", !Pl_Range_Test_Null_Inter(r1,r));

	/* Sparse with Interval*/
	r1 = create_range("0..10:");
	unit_assert("(20..30) !null inter [0..10] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter [0..10] 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->max = r1->first->max = 19;
	unit_assert("(20..30) !null inter [0..19] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter [0..19] 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->max = r1->first->max = 20;
	unit_assert("(20..30) null inter [0..20] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [0..20] 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = r1->first->min = 20;
	unit_assert("(20..30) null inter [20] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [20] 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->max = r1->first->max = 30;
	unit_assert("(20..30) null inter [20..30] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [20..30] 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->max = r1->first->max = 50;
	unit_assert("(20..30) null inter [20..50] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [20..50] 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = r1->first->min = 30;
	unit_assert("(20..30) null inter [30..50] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [30..50] 2", !Pl_Range_Test_Null_Inter(r1,r));
	r1->min = r1->first->min = 31;
	unit_assert("(20..30) !null inter [31..50] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) !null inter [31..50] 2", Pl_Range_Test_Null_Inter(r1,r));
	r1->min = r1->first->min = 25;
	unit_assert("(20..30) null inter [25..50] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("(20..30) null inter [25..50] 2", !Pl_Range_Test_Null_Inter(r1,r));

	/* Sparse with Sparse*/
	r = create_range("20..30:");
	r1 = create_range("25:");
	unit_assert("[20..30] null inter [25] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[20..30] null inter [25] 2", !Pl_Range_Test_Null_Inter(r1,r));

	r1 = create_range("19:31..40");
	unit_assert("[20..30] !null inter [19][31..40] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[20..30] !null inter [19][31..40] 2", Pl_Range_Test_Null_Inter(r1,r));

	r1 = create_range("19:31..40:50..60");
	unit_assert("[20..30] !null inter [19][31..40][50..60] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[20..30] !null inter [19][31..40][50..60] 2", Pl_Range_Test_Null_Inter(r1,r));

	r = create_range("20..30:42");
	unit_assert("[20..30][42] !null inter [19][31..40][50..60] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[20..30][42] !null inter [19][31..40][50..60] 2", Pl_Range_Test_Null_Inter(r1,r));

	r = create_range("20..30:42:48..52");
	unit_assert("[20..30][42][48..52] null inter [19][31..40][50..60] 1", !Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[20..30][42][48..52] null inter [19][31..40][50..60] 2", !Pl_Range_Test_Null_Inter(r1,r));

	r = create_range("2");
	r1 = create_range("1:3..11");
	unit_assert("[2] null inter [1][3..11] 1", Pl_Range_Test_Null_Inter(r,r1));
	unit_assert("[2] null inter [1][3..11] 2", Pl_Range_Test_Null_Inter(r1,r));

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Next_Before (Also tests Pl_Sparse_Next_Before)            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Next_Before() {
	int fail = INTERVAL_MIN_INTEGER-1;

	/* Interval */
	Range *r = create_range("20..30");
	unit_assert("((20..30)) Pl_Range_Next_Before(r,0) != MIN-1", Pl_Range_Next_Before(r,0) == fail);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,19) != MIN-1", Pl_Range_Next_Before(r,19) == fail);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,20) != MIN-1", Pl_Range_Next_Before(r,20) == fail);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,21) != 20", Pl_Range_Next_Before(r,21) == 20);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,25) != 24", Pl_Range_Next_Before(r,25) == 24);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,31) != 30", Pl_Range_Next_Before(r,31) == 30);
	unit_assert("((20..30)) Pl_Range_Next_Before(r,40) != 30", Pl_Range_Next_Before(r,40) == 30);

	r = create_range("MIN..MAX");
	unit_assert("((MIN..MAX)) Pl_Range_Next_Before(r,0) != -1", Pl_Range_Next_Before(r,0) == -1);
	unit_assert("((MIN..MAX)) Pl_Range_Next_Before(r,MIN) != -1", Pl_Range_Next_Before(r,INTERVAL_MIN_INTEGER) == fail);
	unit_assert("((MIN..MAX)) Pl_Range_Next_Before(r,MIN+1) != MIN", Pl_Range_Next_Before(r,INTERVAL_MIN_INTEGER+1) == INTERVAL_MIN_INTEGER);
	unit_assert("((MIN..MAX)) Pl_Range_Next_Before(r,MAX) != MAX-1", Pl_Range_Next_Before(r,INTERVAL_MAX_INTEGER) == INTERVAL_MAX_INTEGER-1);
	

	/* Sparse */
	r = create_range("20..30:");
	unit_assert("([20..30]) Pl_Range_Next_Before(r,0) != MIN-1", Pl_Range_Next_Before(r,0) == fail);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,19) != MIN-1", Pl_Range_Next_Before(r,19) == fail);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,20) != MIN-1", Pl_Range_Next_Before(r,20) == fail);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,21) != 20", Pl_Range_Next_Before(r,21) == 20);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,25) != 24", Pl_Range_Next_Before(r,25) == 24);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,31) != 30", Pl_Range_Next_Before(r,31) == 30);
	unit_assert("([20..30]) Pl_Range_Next_Before(r,40) != 30", Pl_Range_Next_Before(r,40) == 30);

	// multiple chunks
	r = create_range("20..30:40:60..70");
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,19) != MIN-1", Pl_Range_Next_Before(r,19) == fail);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,20) != MIN-1", Pl_Range_Next_Before(r,20) == fail);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,21) != 20", Pl_Range_Next_Before(r,21) == 20);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,25) != 24", Pl_Range_Next_Before(r,25) == 24);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,31) != 30", Pl_Range_Next_Before(r,31) == 30);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,38) != 30", Pl_Range_Next_Before(r,38) == 30);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,40) != 30", Pl_Range_Next_Before(r,40) == 30);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,41) != 40", Pl_Range_Next_Before(r,41) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,59) != 40", Pl_Range_Next_Before(r,59) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,60) != 40", Pl_Range_Next_Before(r,60) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,61) != 60", Pl_Range_Next_Before(r,61) == 60);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,70) != 69", Pl_Range_Next_Before(r,70) == 69);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,71) != 70", Pl_Range_Next_Before(r,71) == 70);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_Before(r,100) != 70", Pl_Range_Next_Before(r,100) == 70);

	Set_To_Empty(r);
	unit_assert("(Empty) Pl_Range_Next_Before(r,MAX) != MIN-1", Pl_Range_Next_Before(r,INTERVAL_MAX_INTEGER) == fail);

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_Pl_Range_Next_After (Also tests Pl_Sparse_Next_After)              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char * test_Pl_Range_Next_After() {
	int fail = INTERVAL_MIN_INTEGER-1;

	/* Interval */
	Range *r = create_range("20..30");
	unit_assert("((20..30)) Pl_Range_Next_After(r,0) != 20", Pl_Range_Next_After(r,0) == 20);
	unit_assert("((20..30)) Pl_Range_Next_After(r,19) != 20", Pl_Range_Next_After(r,19) == 20);
	unit_assert("((20..30)) Pl_Range_Next_After(r,20) != 21", Pl_Range_Next_After(r,20) == 21);
	unit_assert("((20..30)) Pl_Range_Next_After(r,25) != 26", Pl_Range_Next_After(r,25) == 26);
	unit_assert("((20..30)) Pl_Range_Next_After(r,29) != 30", Pl_Range_Next_After(r,29) == 30);
	unit_assert("((20..30)) Pl_Range_Next_After(r,30) != MIN-1", Pl_Range_Next_After(r,30) == fail);
	unit_assert("((20..30)) Pl_Range_Next_After(r,31) != MIN-1", Pl_Range_Next_After(r,31) == fail);
	unit_assert("((20..30)) Pl_Range_Next_After(r,40) != MIN-1", Pl_Range_Next_After(r,40) == fail);

	r = create_range("MIN..MAX");
	unit_assert("((MIN..MAX)) Pl_Range_Next_After(r,0) != 1", Pl_Range_Next_After(r,0) == 1);
	unit_assert("((MIN..MAX)) Pl_Range_Next_After(r,MIN) != MIN+1", Pl_Range_Next_After(r,INTERVAL_MIN_INTEGER) == INTERVAL_MIN_INTEGER+1);
	unit_assert("((MIN..MAX)) Pl_Range_Next_After(r,MAX) != MIN-1", Pl_Range_Next_After(r,INTERVAL_MAX_INTEGER) == fail);
	unit_assert("((MIN..MAX)) Pl_Range_Next_After(r,MAX-1) != MAX", Pl_Range_Next_After(r,INTERVAL_MAX_INTEGER-1) == INTERVAL_MAX_INTEGER);
	

	/* Sparse */
	r = create_range("20..30:");
	unit_assert("([20..30]) Pl_Range_Next_After(r,0) != 20", Pl_Range_Next_After(r,0) == 20);
	unit_assert("([20..30]) Pl_Range_Next_After(r,19) != 20", Pl_Range_Next_After(r,19) == 20);
	unit_assert("([20..30]) Pl_Range_Next_After(r,20) != 21", Pl_Range_Next_After(r,20) == 21);
	unit_assert("([20..30]) Pl_Range_Next_After(r,25) != 26", Pl_Range_Next_After(r,25) == 26);
	unit_assert("([20..30]) Pl_Range_Next_After(r,29) != 30", Pl_Range_Next_After(r,29) == 30);
	unit_assert("([20..30]) Pl_Range_Next_After(r,30) != MIN-1", Pl_Range_Next_After(r,30) == fail);
	unit_assert("([20..30]) Pl_Range_Next_After(r,31) != MIN-1", Pl_Range_Next_After(r,31) == fail);
	unit_assert("([20..30]) Pl_Range_Next_After(r,40) != MIN-1", Pl_Range_Next_After(r,40) == fail);

	// multiple chunks
	r = create_range("20..30:40:60..70");
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,MIN) != 20", Pl_Range_Next_After(r,INTERVAL_MIN_INTEGER) == 20);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,19) != 20", Pl_Range_Next_After(r,19) == 20);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,25) != 26", Pl_Range_Next_After(r,25) == 26);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,29) != 30", Pl_Range_Next_After(r,29) == 30);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,31) != 40", Pl_Range_Next_After(r,31) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,38) != 40", Pl_Range_Next_After(r,38) == 40);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,40) != 60", Pl_Range_Next_After(r,40) == 60);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,41) != 60", Pl_Range_Next_After(r,41) == 60);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,59) != 60", Pl_Range_Next_After(r,59) == 60);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,60) != 61", Pl_Range_Next_After(r,60) == 61);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,61) != 62", Pl_Range_Next_After(r,61) == 62);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,69) != 70", Pl_Range_Next_After(r,69) == 70);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,70) != MIN-1", Pl_Range_Next_After(r,70) == fail);
	unit_assert("([20..30][40][60..70]) Pl_Range_Next_After(r,100) != MIN-1", Pl_Range_Next_After(r,100) == fail);

	Set_To_Empty(r);
	unit_assert("(Empty) Pl_Range_Next_After(r,MIN) != MIN-1", Pl_Range_Next_After(r,INTERVAL_MIN_INTEGER) == fail);

	return 0;
}


/*-------------------------------------------------------------------------*
 * General Test-related Methods                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
 
static char * all_tests() {
	run_test(test_Pl_Create_Interval_Chunk,"Pl_Create_Interval_Chunk");
	run_test(test_Pl_Range_Becomes_Sparse,"Pl_Range_Becomes_Sparse");
	run_test(test_Pl_Set_Chunk_At_End_Of_Range,"Pl_Set_Chunk_At_End_Of_Range");
	run_test(test_Pl_Set_Chunk_Before_Chunk,"Pl_Set_Chunk_Before_Chunk");
	run_test(test_Pl_Get_Chunk_For_Value,"Pl_Get_Chunk_For_Value");
	run_test(test_Pl_Range_Test_Value,"Pl_Range_Test_Value");
	run_test(test_Pl_Chunk_Count,"Pl_Chunk_Count");
	run_test(test_Pl_Remove_Interval_Chunk_If_Needed,"Pl_Remove_Interval_Chunk_If_Needed");
	run_test(test_Pl_Range_Set_Value,"Pl_Range_Set_Value");
	run_test(test_Pl_Range_Reset_Value,"Pl_Range_Reset_Value"); 
	run_test(test_Pl_Range_Copy,"Pl_Range_Copy");
	run_test(test_Pl_Range_Inter,"Pl_Range_Inter");
	run_test(test_Pl_Range_Union,"Pl_Range_Union");
	run_test(test_Pl_Range_Compl,"Pl_Range_Compl");
	run_test(test_Pl_Range_Nb_Elem,"Pl_Range_Nb_Elem");
	run_test(test_Pl_Range_Mul_Value,"Pl_Range_Mul_Value");
	run_test(test_Pl_Range_Div_Value,"Pl_Range_Div_Value");
	
	run_test(test_Pl_Range_Add_Value,"Pl_Range_Add_Value");
	run_test(test_Pl_Range_Mod_Value,"Pl_Range_Mod_Value");
	run_test(test_Pl_Range_Ith_Elem,"Pl_Range_Ith_Elem");
	run_test(test_Pl_Range_Test_Null_Inter,"Pl_Range_Test_Null_Inter");
	run_test(test_Pl_Range_Next_Before,"Pl_Range_Next_Before");
	run_test(test_Pl_Range_Next_After,"Pl_Range_Next_After");

	run_test(test_Pl_Range_Add_Range,"Pl_Range_Add_Range");
	run_test(test_Pl_Range_Sub_Range,"Pl_Range_Sub_Range");
	run_test(test_Pl_Range_Mul_Range,"Pl_Range_Mul_Range");
	run_test(test_Pl_Range_Div_Range,"Pl_Range_Div_Range");
	run_test(test_Pl_Range_Mod_Range,"Pl_Range_Mod_Range");
	
	return 0;
}
 
int main(int argc, char **argv) {
	char *result;
	printf("FD tests (range implementation)\n");
  	result = all_tests();
	printf("Methods tested:  %d\n", methods_tested);
	printf("Total Tests run: %d\n", tests_run);
	if (result != 0) {
		printf("\nTEST FAIL: %s\n\n%s\n\nIn %s\n\n", result, test_message, last_test);
	}
	else {
		printf("All FD tests passed\n");
	}

	return result != 0;
}
