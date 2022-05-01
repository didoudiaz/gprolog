/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : srq.pl                                                 */
/* Title          : Self-Referential Quiz puzzle                           */
/* Original Source: M. Henz                                                */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : February 1997                                          */
/*                                                                         */
/*                                                                         */
/* Q1 : the first question whose answer is A is                            */
/*      (A) 4   (B) 3   (C) 2   (D) 1   (E) none of the above              */
/* Q2 : the only two consecutive questions with identical answers are      */
/*      (A) 3 and 4   (B) 4 and 5   (C) 5 and 6   (D) 6 and 7   (E) 7 and 8*/
/* Q3 : the next question with answer A is                                 */
/*      (A) 4   (B) 5   (C) 6   (D) 7   (E) 8                              */
/* Q4 : the first even numbered question with answer B is                  */
/*      (A) 2   (B) 4   (C) 6   (D) 8   (E) 10                             */
/* Q5 : the only odd numbered question with answer C is                    */
/*      (A) 1   (B) 3   (C) 5   (D) 7   (E) 9                              */
/* Q6 : a question with answer D                                           */
/*      (A) comes before this one but not after this one                   */
/*      (B) comes after this one but not before this one                   */
/*      (C) comes before and after this one                                */
/*      (D) does not occur at all                                          */
/*      (E) none of the above                                              */
/* Q7 : the last question whose answer is E is                             */
/*      (A) 5   (B) 6   (C) 7   (D) 8   (E) 9                              */
/* Q8 : the number of questions whose answers are conconants is            */
/*      (A) 7   (B) 6   (C) 5   (D) 4   (E) 3                              */
/* Q9 : the number of questions whose answers are vowels is                */
/*      (A) 0   (B) 1   (C) 2   (D) 3   (E) 4                              */
/* Q10: the answer of this question is                                     */
/*      (A) A   (B) B   (C) C   (D) D   (E) E                              */
/*                                                                         */
/* Solution:                                                               */
/* [3,1,2,2,1,2,5,2,5,4]                                                   */
/*  C,A,B,B,A,B,E,B,E,D                                                    */
/*-------------------------------------------------------------------------*/


q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	srq(L, Lab),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.




srq(L, Lab) :-
	L = [Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10],
	fd_domain(L, 1, 5),
	Q1 #= 1 #<=> Q4 #= 1 #/\ Q1 #\= 1 #/\ Q2 #\= 1 #/\ Q3 #\= 1,
	Q1 #= 2 #<=> Q3 #= 1 #/\ Q1 #\= 1 #/\ Q2 #\= 1,
	Q1 #= 3 #<=> Q2 #= 1 #/\ Q1 #\= 1,
	Q1 #= 4 #<=> Q1 #= 1,
%       Q1#=5 #<=> Q1#\=1 #/\ Q2#\=1 #/\ Q3#\=1 #/\ Q4#\=1,
	Q2 #= 1 #<=> Q3 #= Q4,
	Q2 #= 2 #<=> Q4 #= Q5,
	Q2 #= 3 #<=> Q5 #= Q6,
	Q2 #= 4 #<=> Q6 #= Q7,
	Q2 #= 5 #<=> Q7 #= Q8,
	Q3 #= 1 #<=> Q4 #= 1,
	Q3 #= 2 #<=> Q5 #= 1 #/\ Q4 #\= 1,
	Q3 #= 3 #<=> Q6 #= 1 #/\ Q4 #\= 1 #/\ Q5 #\= 1,
	Q3 #= 4 #<=> Q7 #= 1 #/\ Q4 #\= 1 #/\ Q5 #\= 1 #/\ Q6 #\= 1,
	Q3 #= 5 #<=> Q8 #= 1 #/\ Q4 #\= 1 #/\ Q5 #\= 1 #/\ Q6 #\= 1 #/\ Q7 #\= 1,
	Q4 #= 1 #<=> Q2 #= 2,
	Q4 #= 2 #<=> Q4 #= 2 #/\ Q2 #\= 2,
	Q4 #= 3 #<=> Q6 #= 2 #/\ Q2 #\= 2 #/\ Q4 #\= 2,
	Q4 #= 4 #<=> Q8 #= 2 #/\ Q2 #\= 2 #/\ Q4 #\= 2 #/\ Q6 #\= 2,
	Q4 #= 5 #<=> Q10 #= 2 #/\ Q2 #\= 2 #/\ Q4 #\= 2 #/\ Q6 #\= 2 #/\ Q8 #\= 2,
	Q5 #= 1 #<=> Q1 #= 3 #/\ Q3 #\= 3 #/\ Q5 #\= 3 #/\ Q7 #\= 3 #/\ Q9 #\= 3,
	Q5 #= 2 #<=> Q3 #= 3 #/\ Q1 #\= 3 #/\ Q5 #\= 3 #/\ Q7 #\= 3 #/\ Q9 #\= 3,
	Q5 #= 3 #<=> Q5 #= 3 #/\ Q1 #\= 3 #/\ Q3 #\= 3 #/\ Q7 #\= 3 #/\ Q9 #\= 3,
	Q5 #= 4 #<=> Q7 #= 3 #/\ Q1 #\= 3 #/\ Q2 #\= 3 #/\ Q5 #\= 3 #/\ Q9 #\= 3,
	Q5 #= 5 #<=> Q9 #= 3 #/\ Q1 #\= 3 #/\ Q3 #\= 3 #/\ Q5 #\= 3 #/\ Q7 #\= 3,
	BeforeQ4 #<=> Q1 #= 4 #\/ Q2 #= 4 #\/ Q3 #= 4 #\/ Q4 #= 4 #\/ Q5 #= 4,
	AfterQ4 #<=> Q7 #= 4 #\/ Q8 #= 4 #\/ Q9 #= 4 #\/ Q10 #= 4,
	Q6 #= 1 #<=> BeforeQ4 #/\ #\ AfterQ4,
	Q6 #= 2 #<=> #\ BeforeQ4 #/\ AfterQ4,
	Q6 #= 3 #<=> BeforeQ4 #/\ AfterQ4,
	Q6 #= 4 #<=> Q1 #\= 4 #/\ Q2 #\= 4 #/\ Q3 #\= 4 #/\ Q4 #\= 4 #/\ Q5 #\= 4 #/\ Q6 #\= 4 #/\ Q7 #\= 4 #/\ Q8 #\= 4 #/\ Q9 #\= 4 #/\ Q10 #\= 4,
%       Q6#=5 #<=> Q6#=4,
	Q7 #= 1 #<=> Q5 #= 5 #/\ Q6 #\= 5 #/\ Q7 #\= 5 #/\ Q8 #\= 5 #/\ Q9 #\= 5 #/\ Q10 #\= 5,
	Q7 #= 2 #<=> Q6 #= 5 #/\ Q7 #\= 5 #/\ Q8 #\= 5 #/\ Q9 #\= 5 #/\ Q10 #\= 5,
	Q7 #= 3 #<=> Q7 #= 5 #/\ Q8 #\= 5 #/\ Q9 #\= 5 #/\ Q10 #\= 5,
	Q7 #= 4 #<=> Q8 #= 5 #/\ Q9 #\= 5 #/\ Q10 #\= 5,
	Q7 #= 5 #<=> Q9 #= 5 #/\ Q10 #\= 5,
	BCD1 #<=> Q1 #>= 2 #/\ Q1 #=< 4,
	AE1 #<=> #\ BCD1,
	BCD2 #<=> Q2 #>= 2 #/\ Q2 #=< 4,
	AE2 #<=> #\ BCD2,
	BCD3 #<=> Q3 #>= 2 #/\ Q3 #=< 4,
	AE3 #<=> #\ BCD3,
	BCD4 #<=> Q4 #>= 2 #/\ Q4 #=< 4,
	AE4 #<=> #\ BCD4,
	BCD5 #<=> Q5 #>= 2 #/\ Q5 #=< 4,
	AE5 #<=> #\ BCD5,
	BCD6 #<=> Q6 #>= 2 #/\ Q6 #=< 4,
	AE6 #<=> #\ BCD6,
	BCD7 #<=> Q7 #>= 2 #/\ Q7 #=< 4,
	AE7 #<=> #\ BCD7,
	BCD8 #<=> Q8 #>= 2 #/\ Q8 #=< 4,
	AE8 #<=> #\ BCD8,
	BCD9 #<=> Q9 #>= 2 #/\ Q9 #=< 4,
	AE9 #<=> #\ BCD9,
	BCD10 #<=> Q10 #>= 2 #/\ Q10 #=< 4,
	AE10 #<=> #\ BCD10,
	BCD #= BCD1 + BCD2 + BCD3 + BCD4 + BCD5 + BCD6 + BCD7 + BCD8 + BCD9 + BCD10,
	AE #= AE1 + AE2 + AE3 + AE4 + AE5 + AE6 + AE7 + AE8 + AE9 + AE10,
	Q8 #= 1 #<=> BCD #= 7,
	Q8 #= 2 #<=> BCD #= 6,
	Q8 #= 3 #<=> BCD #= 5,
	Q8 #= 4 #<=> BCD #= 4,
	Q8 #= 5 #<=> BCD #= 3,
	Q9 #= 1 #<=> AE #= 0,
	Q9 #= 2 #<=> AE #= 1,
	Q9 #= 3 #<=> AE #= 2,
	Q9 #= 4 #<=> AE #= 3,
	Q9 #= 5 #<=> AE #= 4,
	lab(Lab, L).






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
