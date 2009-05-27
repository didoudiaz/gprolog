/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : cars.pl                                                */
/* Title          : car sequencing problem                                 */
/* Original Source: Dincbas, Simonis and Van Hentenryck                    */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* Car sequencing problem with 10 cars                                     */
/* Solution:                                                               */
/*   [1,2,6,3,5,4,4,5,3,6]                                                 */
/*   [1,3,6,2,5,4,3,5,4,6]                                                 */
/*   [1,3,6,2,6,4,5,3,4,5]                                                 */
/*   [5,4,3,5,4,6,2,6,3,1]                                                 */
/*   [6,3,5,4,4,5,3,6,2,1]                                                 */
/*   [6,4,5,3,4,5,2,6,3,1]                                                 */
/*                                                                         */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	(   cars(L, Lab),
	    write(L),
	    nl,
	    fail
	;   true
	),
	statistics(runtime, [_, Y]),
	write('time : '),
	write(Y),
	nl.




cars(X, Lab) :-
	fd_set_vector_max(6),
	X = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10],
	Y = [O11, O12, O13, O14, O15, O21, O22, O23, O24, O25, O31, O32, O33, O34, O35, O41, O42, O43, O44, O45, O51, O52, O53, O54, O55, O61, O62, O63, O64, O65, O71, O72, O73, O74, O75, O81, O82, O83, O84, O85, O91, O92, O93, O94, O95, O101, O102, O103, O104, O105],
	L1 = [1, 0, 0, 0, 1, 1],
	L2 = [0, 0, 1, 1, 0, 1],
	L3 = [1, 0, 0, 0, 1, 0],
	L4 = [1, 1, 0, 1, 0, 0],
	L5 = [0, 0, 1, 0, 0, 0],
	fd_domain(Y, 0, 1),
	fd_domain(X, 1, 6),
	fd_atmost(1, X, 1),
	fd_atmost(1, X, 2),
	fd_atmost(2, X, 3),
	fd_atmost(2, X, 4),
	fd_atmost(2, X, 5),
	fd_atmost(2, X, 6),
	fd_element(X1, L1, O11),
	fd_element(X1, L2, O12),
	fd_element(X1, L3, O13),
	fd_element(X1, L4, O14),
	fd_element(X1, L5, O15),
	fd_element(X2, L1, O21),
	fd_element(X2, L2, O22),
	fd_element(X2, L3, O23),
	fd_element(X2, L4, O24),
	fd_element(X2, L5, O25),
	fd_element(X3, L1, O31),
	fd_element(X3, L2, O32),
	fd_element(X3, L3, O33),
	fd_element(X3, L4, O34),
	fd_element(X3, L5, O35),
	fd_element(X4, L1, O41),
	fd_element(X4, L2, O42),
	fd_element(X4, L3, O43),
	fd_element(X4, L4, O44),
	fd_element(X4, L5, O45),
	fd_element(X5, L1, O51),
	fd_element(X5, L2, O52),
	fd_element(X5, L3, O53),
	fd_element(X5, L4, O54),
	fd_element(X5, L5, O55),
	fd_element(X6, L1, O61),
	fd_element(X6, L2, O62),
	fd_element(X6, L3, O63),
	fd_element(X6, L4, O64),
	fd_element(X6, L5, O65),
	fd_element(X7, L1, O71),
	fd_element(X7, L2, O72),
	fd_element(X7, L3, O73),
	fd_element(X7, L4, O74),
	fd_element(X7, L5, O75),
	fd_element(X8, L1, O81),
	fd_element(X8, L2, O82),
	fd_element(X8, L3, O83),
	fd_element(X8, L4, O84),
	fd_element(X8, L5, O85),
	fd_element(X9, L1, O91),
	fd_element(X9, L2, O92),
	fd_element(X9, L3, O93),
	fd_element(X9, L4, O94),
	fd_element(X9, L5, O95),
	fd_element(X10, L1, O101),
	fd_element(X10, L2, O102),
	fd_element(X10, L3, O103),
	fd_element(X10, L4, O104),
	fd_element(X10, L5, O105),
	1 #>= O11 + O21,
	1 #>= O21 + O31,
	1 #>= O31 + O41,
	1 #>= O41 + O51,
	1 #>= O51 + O61,
	1 #>= O61 + O71,
	1 #>= O71 + O81,
	1 #>= O81 + O91,
	1 #>= O91 + O101,
	2 #>= O12 + O22 + O32,
	2 #>= O22 + O32 + O42,
	2 #>= O32 + O42 + O52,
	2 #>= O42 + O52 + O62,
	2 #>= O52 + O62 + O72,
	2 #>= O62 + O72 + O82,
	2 #>= O72 + O82 + O92,
	2 #>= O82 + O92 + O102,
	1 #>= O13 + O23 + O33,
	1 #>= O23 + O33 + O43,
	1 #>= O33 + O43 + O53,
	1 #>= O43 + O53 + O63,
	1 #>= O53 + O63 + O73,
	1 #>= O63 + O73 + O83,
	1 #>= O73 + O83 + O93,
	1 #>= O83 + O93 + O103,
	2 #>= O14 + O24 + O34 + O44 + O54,
	2 #>= O24 + O34 + O44 + O54 + O64,
	2 #>= O34 + O44 + O54 + O64 + O74,
	2 #>= O44 + O54 + O64 + O74 + O84,
	2 #>= O54 + O64 + O74 + O84 + O94,
	2 #>= O64 + O74 + O84 + O94 + O104,
	1 #>= O15 + O25 + O35 + O45 + O55,
	1 #>= O25 + O35 + O45 + O55 + O65,
	1 #>= O35 + O45 + O55 + O65 + O75,
	1 #>= O45 + O55 + O65 + O75 + O85,
	1 #>= O55 + O65 + O75 + O85 + O95,
	1 #>= O65 + O75 + O85 + O95 + O105,
% redundant constraints
	O11 + O21 + O31 + O41 + O51 + O61 + O71 + O81 #>= 4,
	O11 + O21 + O31 + O41 + O51 + O61 #>= 3,
	O11 + O21 + O31 + O41 #>= 2,
	O11 + O21 #>= 1,
	O12 + O22 + O32 + O42 + O52 + O62 + O72 #>= 4,
	O12 + O22 + O32 + O42 #>= 2,
	O12 #>= 0,
	O13 + O23 + O33 + O43 + O53 + O63 + O73 #>= 2,
	O13 + O23 + O33 + O43 #>= 1,
	O13 #>= 0,
	O14 + O24 + O34 + O44 + O54 #>= 2,
	O15 + O25 + O35 + O45 + O55 #>= 1,
	lab(Lab, X).




lab(normal, L) :-
	fd_labeling(L).

lab(ff, L) :-
	fd_labelingff(L).




get_fd_labeling(Lab) :-
	argument_counter(C),
	get_labeling1(C, Lab).


get_labeling1(1, normal).

get_labeling1(2, Lab) :-
	argument_value(1, Lab).




:-	initialization(q).
