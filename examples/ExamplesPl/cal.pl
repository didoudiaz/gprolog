/*
Calendar program:

This algorithm was published in comp.programming and comes from
    Andy Lowry, lowry@watson.ibm.com, (914) 784-7925
    IBM Research, P.O. Box 704, Yorktown Heights, NY 10598

% Original Prolog version by Peter Ludemann,
% optimized for BinProlog by Paul Tarau.

*/


% Last 10000 FoolsDays arithmetic benchmark:



cal(ShowResult):-
	N = 10000,
	day_of_week(1993,4,9,Day),
	(   ShowResult = true ->
	    write('April 9, 1993 is: '),
	    write(Day),
	    write(' fools_days = '), write(N), nl
	;   true),
	empty_loop(N),
	full_loop(N).



range(Min,Min,Max):-
	Min=<Max.

range(I,Min,Max):-
	Min<Max,
	Min1 is Min+1,
	range(I,Min1,Max).




empty_loop(Y):-
	range(_,1,Y),
	true,
	fail.

empty_loop(_).




full_loop(Max):-
	range(Year,1,Max),
	day_of_week(Year,4,1,_FoolsDay),
	fail.

full_loop(_).




% We associate a number with each day of the week:

dow(0, sun).
dow(1, mon).
dow(2, tue).
dow(3, wed).
dow(4, thu).
dow(5, fri).
dow(6, sat).




% The day of week computation is rather arcane, but it works.
% Note the correction for leap years.

day_of_week(Year,Month,Day, DayOfWeek):-
	cal_key(Month, Key, LeapC),
	compute_it(Year,Day,Key,LeapC,DayOfWeek).




compute_it(Year,Day,Key,LeapC,DayOfWeek):-
	Century is Year // 100,
	YearInCentury is Year - Century * 100,
	DOW0 is (Century * 5 + Century // 4 +
	         YearInCentury + YearInCentury // 4 +
	         Day + Key)
	         mod 7,
	leap_year(Year,DOW0,LeapC,DayOfWeek).




% A leap year is any year which is divisible by 4; if it is also
% divisible by 100 then it must also be divisible by 400 (thus,
% 1600 and 2000 are leap years; 1700, 1800, and 1900 are not).

leap_year(Year,DOW0,_,DayOfWeek):-
	0 =\= Year mod 4,
	!,
	dow(DOW0,DayOfWeek).

leap_year(Year,DOW0,LeapC,DayOfWeek):-
	0 =\= Year mod 100,
	!,
	DOW is DOW0-LeapC,
	dow(DOW,DayOfWeek).

leap_year(Year,DOW0,_,DayOfWeek):-
	0 =\= Year mod 400,
	!,
	dow(DOW0,DayOfWeek).

leap_year(_,DOW0,LeapC,DayOfWeek):-
	DOW is DOW0-LeapC,
	dow(DOW,DayOfWeek).




/*
There is a special key value for each month and a
correction factor for January and February in leap years.
*/

cal_key( 1, 6, 1).
cal_key( 2, 2, 1).
cal_key( 3, 2, 0).
cal_key( 4, 5, 0).
cal_key( 5, 0, 0).
cal_key( 6, 3, 0).
cal_key( 7, 5, 0).
cal_key( 8, 1, 0).
cal_key( 9, 4, 0).
cal_key(10, 6, 0).
cal_key(11, 2, 0).
cal_key(12, 4, 0).
cal_key(jan, 6, 1).
cal_key(feb, 2, 1).
cal_key(mar, 2, 0).
cal_key(apr, 5, 0).
cal_key(may, 0, 0).
cal_key(jun, 3, 0).
cal_key(jul, 5, 0).
cal_key(aug, 1, 0).
cal_key(sep, 4, 0).
cal_key(oct, 6, 0).
cal_key(nov, 2, 0).
cal_key(dec, 4, 0).
cal_key('January', 6, 1).
cal_key('February', 2, 1).
cal_key('March', 2, 0).
cal_key('April', 5, 0).
cal_key('May', 0, 0).
cal_key('June', 3, 0).
cal_key('July', 5, 0).
cal_key('August', 1, 0).
cal_key('September',4, 0).
cal_key('October', 6, 0).
cal_key('November',2, 0).
cal_key('December', 4, 0).


% benchmark interface

benchmark(ShowResult) :-
	cal(ShowResult).

:- include(common).
