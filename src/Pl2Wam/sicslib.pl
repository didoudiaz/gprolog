:- op(0,fx,dynamic).

include(X):-
	[X].


prolog_file_name(PlFile,PlFile1):-
        decompose_file_name(PlFile,_Dir,_Prefix,Suffix),
        ((PlFile=user ; Suffix\=='')
               -> PlFile1=PlFile
               ;  atom_concat(PlFile,'.pl',PlFile1)).


last_read_start_line_column(Line,Col):-
	g_read('$last_line',Line),
	g_read('$last_col',Col).

stream_line_column(Stream,Line,Col):-
	line_count(Stream,Count),
	line_position(Stream,Pos),
	Line is Count+1,
	Col is Pos+1,
	g_assign('$last_line',Line),
	g_assign('$last_col',Col).



peek_code(S,X):-
	peek_char(S,X).

get_code(S,X):-
	get0(S,X).




:- dynamic(prolog_name/1).
:- dynamic(prolog_version/1).
:- dynamic(prolog_date/1).
:- dynamic(prolog_copyright/1).

prolog_name('SICStus Prolog').
prolog_version('?.?.?').
prolog_date('2000').
prolog_copyright('').


date_time(dt(0,0,0,0,0,0)).



reverse([],[]).

reverse([H|T],L):-
        reverse1(T,L,[H]).


reverse1([],L,L).

reverse1([H|T],L,L1):-
        reverse1(T,L,[H|L1]).




append([],L,L).

append([X|L1],L2,[X|L3]):-
        append(L1,L2,L3).


atom_length(A,L):-
        name(A,S),
        length(S,L).



stop:-
	abort.


catch(Goal,_,_):-
	call(Goal).

discontiguous(_).



atom_concat(A1,A2,A3):-
	(nonvar(A1) -> atom_codes(A1,LCodeA1) ; true),
	(nonvar(A2) -> atom_codes(A2,LCodeA2) ; true),
	(nonvar(A3) -> atom_codes(A3,LCodeA3) ; true),
        append(LCodeA1,LCodeA2,LCodeA3),
        atom_codes(A1,LCodeA1),
        atom_codes(A2,LCodeA2),
        atom_codes(A3,LCodeA3).




sub_atom(A,Before,L,After,A1):-
        atom_codes(A,LCodeA),
        atom_length(A,N),
        (integer(Before) 
            -> true
            ;  list_i_j(1,N,SL), '$member'(S,SL), Before is S-1),
        (integer(L)
            -> true
            ;
               MaxL is N-Before,
               list_i_j(0,MaxL,LL), '$member'(L,LL)),
        After is N-L-Before,
        length(X0,Before),
        length(LCodeA1,L),
        length(X2,After),
        append(X1,X2,LCodeA),
        append(X0,LCodeA1,X1),
        atom_codes(A1,LCodeA1).




list_i_j(I,J,L):-
        (I>J -> L=[]
              ;
                 I1 is I+1,
                 L=[I|L1],
                 list_i_j(I1,J,L1)).






'$member'(X,[H|T]):-
        X=H ; '$member'(X,T).





char_code(Char,Code):-
        atom_char(Char,Code).




number_atom(N,A):-
	(number(N) -> number_chars(N,LCode),
	              atom_chars(A,LCode)
                   ;
                      atom_chars(A,LCode),
		      number_chars(N,LCode)).


atom_codes(A,LCode):-
	atom_chars(A,LCode).





chars_codes([],[]).

chars_codes([Char|LChar],[Code|LCode]):-
        char_code(Char,Code),
        chars_codes(LChar,LCode).




number_codes(N,LCode):-
	number_chars(N,LCode).





/* g_vars */

:- dynamic(gvar/2).

g_assign(Var,Value):-
	(retract(gvar(Var,_)) ; true),
	!,
	asserta(gvar(Var,Value)).

g_read(Var,Value):-
	(gvar(Var,Value1) ; Value1=0),
	!,
        Value=Value1.





decompose_file_name(Path,Dir,Prefix,Suffix):-
	atom_length(Path,L),
	Before is L-1,
	find_dir_and_file_name(Path,Before,Dir,FileName),
	(sub_atom(FileName,LgPrefix,1,_,'.') 
              -> sub_atom(FileName,0,LgPrefix,LgSuffix,Prefix),
		 sub_atom(FileName,LgPrefix,LgSuffix,0,Suffix)
              ;  
	         Prefix=FileName, Suffix=''),
	!.


find_dir_and_file_name(Path,Before,'',Path):-
	Before<0,
	!.

find_dir_and_file_name(Path,Before,Dir,FileName):-
	sub_atom(Path,Before,1,After,'/'),
	Before1 is Before+1,
	sub_atom(Path,0,Before1,_,Dir),
	sub_atom(Path,_,After,0,FileName),
	!.

find_dir_and_file_name(Path,Before,Dir,FileName):-
	Before1 is Before-1,
	find_dir_and_file_name(Path,Before1,Dir,FileName).






'$sys_var_write'(Var,N):-
        g_assign(Var,N).

'$sys_var_read'(Var,N):-
        g_read(Var,N).

'$sys_var_inc'(Var):-
        g_read(Var,N),
	N1 is N+1,
        g_assign(Var,N1).

'$sys_var_dec'(Var):-
        g_read(Var,N),
	N1 is N-1,
        g_assign(Var,N1).

'$sys_var_set_bit'(Var,Bit):-
        g_read(Var,N),
	N1 is N\/(1<<Bit),
        g_assign(Var,N1).

'$sys_var_reset_bit'(Var,Bit):-
        g_read(Var,N),
	N1 is N/\ \(1<<Bit),
        g_assign(Var,N1).



'$catch'(Goal,_Catcher,_Recovery,_,_,_):-
	call(Goal).



'$aux_name'(Name):-
	sub_atom(Name,_,5,_,'_$aux'),
	!.



'$make_aux_name'(Pred,N,Aux,AuxName):-
        (sub_atom(Pred,LgBefore,5,_,'_$aux')
               -> sub_atom(Pred,0,LgBefore,_,Pred1)
               ;  
                  number_atom(N,AN),
                  atom_concat('$',Pred,Pred2),
                  atom_concat(Pred2,'/',Pred3),
                  atom_concat(Pred3,AN,Pred1)),
        number_atom(Aux,ANo),
        atom_concat('_$aux',ANo,AAux),
        atom_concat(Pred1,AAux,AuxName).




'$pred_without_aux'(Func,Arity,Func1,Arity1):-
        (sub_atom(Func,LgBefore,5,_,'_$aux')
              -> sub_atom(Func,B,1,_,'/'),
                 L is B-1,
                 sub_atom(Func,1,L,_,Func1),
                 B1 is B+1,
		 A is LgBefore-B-1,
                 sub_atom(Func,B1,A,_,SA1),
		 number_atom(Arity1,SA1), !
              ;  Func1=Func,
                 Arity1=Arity).
	

