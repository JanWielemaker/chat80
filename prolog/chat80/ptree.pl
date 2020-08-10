/*  Copyright 1986-2020 David H. D. Warren and Fernando C. N. Pereira

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* Print term as a tree */

 :- mode print_tree(+).
 :- mode pt(+,+).
 :- mode pl(+,+).
 :- mode as_is(+).

 :- public print_tree/1.

print_tree(T) :-
   numbervars(T,1,_),
   pt(T,0), nl, fail.
print_tree(_).

pt(A,I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt([T|Ts],I) :- !,
   pt(T,I),
   pl(Ts,I).
pt(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As,I0).

pl([],_) :- !.
pl([A|As],I) :- !,
   pt(A,I),
   pl(As,I).

as_is(A) :- atomic(A), !.
as_is('$VAR'(_)) :- !.
as_is(X) :-
   quote(X).


