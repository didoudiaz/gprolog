/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : qg5.pl                                                 */
/* Title          : Quasi-group problem                                    */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : July 1998                                              */
/*                                                                         */
/* Find a semigroup table so that: ((ba)b)b=a under idempotency hypothesis.*/
/*                                                                         */
/* Solution:                                                               */
/* N=5 [[1,5,4,2,3],[3,2,5,1,4],[2,4,3,5,1],[5,3,1,4,2],[4,1,2,3,5]]       */
/*-------------------------------------------------------------------------*/

q:-     write('N ?'), read_integer(N),
        statistics(runtime,_),
	(qg5(N,A), 
	 write(A), nl,
	 write_array(A,'%3d',0),
         fail
	   ;
         write('No more solutions'), nl),
	statistics(runtime,[_,Y]),
        write('time : '), write(Y), nl.




qg5(N,A):-
	N2 is N*N,
	fd_set_vector_max(N2),
	create_array(N,N,A),
	array_to_list(A,L),
	fd_domain(L,1,N),
	diag_cstr(L,1,1,N),
	for_each_line(A,alldiff),
	for_each_column(A,alldiff),
        last(A,LastLine),
	isomorphic_cstr(LastLine,0),
	axioms_cstr(1,N,L),
	fd_labelingff(L).







array_to_list([],[]).

array_to_list([Line|A],L1):-
	array_to_list(A,L),
	append(Line,L,L1).





diag_cstr([],_,_,_).

diag_cstr([X|L],K,I,N):-
	(K=1 -> X=I,
                K1 is K+N,
	        I1 is I+1
             ;
                K1 is K-1,
	        I1=I),
	diag_cstr(L,K1,I1,N).

	
	


isomorphic_cstr([],_).

isomorphic_cstr([X|L],K):-
	X #>= K,
	K1 is K+1,
	isomorphic_cstr(L,K1).




axioms_cstr(I,N,L):-
	(I=<N -> axioms_cstr1(I,1,N,L),
	         I1 is I+1,
		 axioms_cstr(I1,N,L)
              ;
                 true).

axioms_cstr1(I,J,N,L):-
	(J=<N -> (I=\=J -> table(L,N,J,I,JI),
    	                   table(L,N,JI,J,JI_J),
			   table(L,N,JI_J,J,I)
                        ;
			   true),
	         J1 is J+1,
		 axioms_cstr1(I,J1,N,L)
               ;
                 true).


table(L,N,A,B,X):-
	N*(B-1)+A#=Z,
	fd_element_var(Z,L,X).




array_prog(alldiff,L):-
        fd_all_different(L).

array_prog(writeline,L):-
        write(L), nl.



:- include(array).

:- initialization(q).
