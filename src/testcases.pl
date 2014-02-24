/*-------------------------------------------------------------------------*/
/* Test set (testing correctness of constraints)                           */
/*                                                                         */
/* Name           : testcases.pl                                           */
/* Title          : testcases                                              */
/* Original Source: Vincent Bloemen                                        */
/* Date           : February 2014                                          */
/*                                                                         */
/* These testcases are initially constructed for testing the negative      */
/* domain. It will print out the ranges for constrained variables.         */
/* Using manual inspection, or by finding the difference with              */ 
/* testresults.ok (when tests in this file are changed, testresults.ok     */ 
/* needs to be updated as well).                                           */
/*-------------------------------------------------------------------------*/

q :-
	statistics(runtime, _),
	write('test set'),
	nl,
	statistics(runtime, [_, Y]),
	MIN_INT is -268435456,
	MAX_INT is 268435455,
	test_range(MIN_INT,MAX_INT,5),
	test_range(-200,100,5),

	test_math_add(MIN_INT,MAX_INT,MIN_INT,MAX_INT,5),
	test_math_add(-400,300,-200,100,5),

	test_math_mult(MIN_INT,MAX_INT,MIN_INT,MAX_INT,5),
	test_math_mult(-400,300,-200,100,5),

	test_math_pow(MIN_INT,MAX_INT,MIN_INT,MAX_INT,2,5),
	test_math_pow(-10,5,-200,100,0,0),
	test_math_pow(-10,5,-200,100,1,0),
	test_math_pow(-10,5,-200,100,2,0),
	test_math_pow(-10,5,-200,100,2,1),
	test_math_pow(-10,5,-200,100,2,2),
	test_math_pow(-10,5,-200,100,2,3),
	test_math_pow(-10,5,-200,100,2,4),
	test_math_pow(-10,5,-200,100,2,5),
	nl,
	write('time : '),
	write(Y),
	nl.

test_range(MinX,MaxX,C) :-
	format('range interval tests (bounded X:[~d..~d], C:~d):',[MinX,MaxX,C]),
	fd_domain([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12],MinX,MaxX),
	
	print_test('X #> C'), 		X1 #> C, 	print_dom('X',X1),
	print_test('X #> -C'), 		X2 #> -C, 	print_dom('X',X2),
	print_test('X #>= C'), 		X3 #>= C, 	print_dom('X',X3),
	print_test('X #>= -C'), 	X4 #>= -C, 	print_dom('X',X4),
	print_test('X #< C'), 		X5 #< C, 	print_dom('X',X5),
	print_test('X #< C'), 		X6 #< -C, 	print_dom('X',X6),
	print_test('X #=< C'), 		X7 #=< C, 	print_dom('X',X7),
	print_test('X #=< -C'), 	X8 #=< -C, 	print_dom('X',X8),
	print_test('X #= C'), 		X9 #= C, 	print_dom('X',X9),
	print_test('X #= -C'), 		X10 #= -C, 	print_dom('X',X10),
	print_test('X #\\= C'), 	X11 #\= C, 	print_dom('X',X11),
	print_test('X #\\= -C'), 	X12 #\= -C, print_dom('X',X12),
	nl.

test_math_add(MinX,MaxX,MinZ,MaxZ,C) :-
	format('addition/subtraction tests (bounded X:[~d..~d], Z:[~d..~d], C:~d):',[MinX,MaxX,MinZ,MaxZ,C]),
	fd_domain([X1,X2,X3,X4,X5,X6,X7,X8],MinX,MaxX),
	fd_domain([Z7,Z8],MinZ,MaxZ),

	print_test('X + C #= Y'),	 X1 + C #= Y1, 		print_dom('X',X1), print_dom('Y',Y1),
	print_test('X - C #= Y'),	 X2 - C #= Y2, 		print_dom('X',X2), print_dom('Y',Y2),
	print_test('X + Y #= C'),	 X3 + Y3 #= C, 		print_dom('X',X3), print_dom('Y',Y3),
	print_test('X - Y #= C'),	 X4 - Y4 #= C, 		print_dom('X',X4), print_dom('Y',Y4),
	print_test('X + Y #= -C'),	 X5 + Y5 #= -C, 	print_dom('X',X5), print_dom('Y',Y5),
	print_test('X - Y #= -C'),	 X6 - Y6 #= -C, 	print_dom('X',X6), print_dom('Y',Y6),
	print_test('X + Y #= Z'),	 X7 + Y7 #= Z7, 	print_dom('X',X7), print_dom('Y',Y7), print_dom('Z',Z7),
	print_test('X - Y #= Z'),	 X8 - Y8 #= Z8, 	print_dom('X',X8), print_dom('Y',Y8), print_dom('Z',Z8),
	nl.

test_math_mult(MinX,MaxX,MinZ,MaxZ,C) :-
	format('multiplication/division tests (bounded X:[~d..~d], Z:[~d..~d], C:~d):',[MinX,MaxX,MinZ,MaxZ,C]),
	fd_domain([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12],MinX,MaxX),
	fd_domain([Z9,Z10,Z11,Z12],MinZ,MaxZ),

	print_test('X * C #= Y'),	 X1 * C #= Y1, 		print_dom('X',X1), print_dom('Y',Y1),
	print_test('X / C #= Y'),	 X2 / C #= Y2, 		print_dom('X',X2), print_dom('Y',Y2),
	print_test('X * -C #= Y'),	 X3 * -C #= Y3, 	print_dom('X',X3), print_dom('Y',Y3),
	print_test('X / -C #= Y'),	 X4 / -C #= Y4, 	print_dom('X',X4), print_dom('Y',Y4),
	print_test('X * Y #= C'),	 X5 * Y5 #= C, 		print_dom('X',X5), print_dom('Y',Y5),
	print_test('X / Y #= C'),	 X6 / Y6 #= C, 		print_dom('X',X6), print_dom('Y',Y6),
	print_test('X * Y #= -C'),	 X7 * Y7 #= -C, 	print_dom('X',X7), print_dom('Y',Y7),
	print_test('X / Y #= -C'),	 X8 / Y8 #= -C, 	print_dom('X',X8), print_dom('Y',Y8),
	print_test('X * Y #= Z'),	 X9 * Y9 #= Z9, 	print_dom('X',X9), print_dom('Y',Y9), print_dom('Z',Z9),
	print_test('X / Y #= Z'),	 X10 / Y10 #= Z10, 	print_dom('X',X10), print_dom('Y',Y10), print_dom('Z',Z10),
	print_test('X * Z #= Y'),	 X11 * Z11 #= Y11, 	print_dom('X',X11), print_dom('Y',Y11), print_dom('Z',Z11),
	print_test('X / Z #= Y'),	 X12 / Z12 #= Y12, 	print_dom('X',X12), print_dom('Y',Y12), print_dom('Z',Z12),
	nl.

test_math_pow(MinX,MaxX,MinZ,MaxZ,C1,C2) :-
	format('power tests (bounded X:[~d..~d], Z:[~d..~d], C1:~d, C2:~d):',[MinX,MaxX,MinZ,MaxZ,C1,C2]),
	fd_domain([X1,X4,X5],MinX,MaxX),
	fd_domain([Z1,Z2,Z3,Z4,Z5],MinZ,MaxZ),

	print_test('X ** C2 #= Z'),	 	X1 ** C2 #= Z1, 	print_dom('X',X1), print_dom('Z',Z1),
	print_test('C1 ** C2 #= Z'),	C1 ** C2 #= Z2, 	print_dom('Z',Z2), 
	print_test('-C1 ** C2 #= Z'),	(-C1) ** C2 #= Z3, 	print_dom('Z',Z3), 
	print_test('C1 ** X #= Z'),	 	C1 ** X4 #= Z4, 	print_dom('X',X4), print_dom('Z',Z4),
	print_test('-C1 ** X #= Z'),	(-C1) ** X5 #= Z5, 	print_dom('X',X5), print_dom('Z',Z5),
	nl.

print_test(Test) :-
	format('\n\t~p:   ',[Test]).

print_dom(Name,V) :-
	fd_min(V,MI),
	fd_max(V,MA),
	format('~p: [~d..~d]   ',[Name, MI,MA]).

:-	initialization(q).
