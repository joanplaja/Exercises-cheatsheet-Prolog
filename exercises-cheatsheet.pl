% contains(Element,List)
% Returns true if the list contains the element
% Examples:
% contains(1,[3,4,5]) = false
% contains(element,[1,2,3]) = element = 1 or element = 2 or element = 3
contains(X,[_|XS]):- contains(X,XS).
contains(X,[X|_]).

% append(ListA,ListB,ResultList)
% resultList is the concatenation of listA and listB
%append([],Ys,Ys).          % the result of ppeding and empty list to Ys is the same Ys
%append([X|Xs],Ys,[X|Zs])   % the result of appending the first element X on YS is X followed by Zs
%    :- append(Xs,Ys,Zs).   % where Zs is the result of appending Xs to Ys

% permutation(List,PermutationList)
% permutationList is a permutation of list
% Example:
% permutation([1,2,3],L).
%   L = [1, 2, 3] ;
%   L = [1, 3, 2] ;
%   L = [2, 1, 3] ;
%   L = [2, 3, 1] ;
%   L = [3, 1, 2] ;
%   L = [3, 2, 1] ;
permutation([],[]).
permutation(L,[X|Xs]):- % A permutation of the list L is a list starting with the value X and followed with XS
    append(V,[X|P],L),  % where the value X is inside L, preceded with V and followed with the P
    append(V,P,W),      % and V apended P forms W, which is the rest of L without X
    permutation(W,Xs).  % and Xs is a permutation of W

% sum(List,Result+)
% result is the sum of all the elements in the list. Result must be a numeric value.
sum([],0).
sum([First|Rest],Result) :-
    sum(Rest,RestResult),
    Result is First+RestResult.

% evens(List,EvenList)
% EvenList is the list of even numbers that exists on the list
evens([],[]).         % If the list is empty the EvenList is empty too
evens([First|Rest],[First|RestEvenList])    % The first element is added on the even list if:
    :- First mod 2 =:= 0,!,                 % - The first element is divisible by 2
    evens(Rest,RestEvenList).               % The rest of the even list is constructed recrusivly inspecting the rest of the list
% If the first element is not even we still need to inspect the rest of the list
evens([First|Rest],EvenList)
    :- evens(Rest,EvenList).

% palindrome(List)
% Returns true if the list is a palindrome
palindrome([]).
palindrome([X]).
palindrome([First|Rest]) :-         % The list will be a palindrome if:
    append(Middle,[First],Rest),    % the first element is equal the last one on the list
    palindrome(Middle).             % and if the elements between the first and last ( middle elements ) are also a palindrome

% isOrderedAsc(List)
% Returns true if the list is ordered ascending
% Examples:
% isOrderedAsc([1,2,3,1]) = false
% isOrderedAsc([1,2,3]) = true
isOrderedAsc([]).
isOrderedAsc([Element]).
% If next line is missing the example isOrderedAsc([1,2,3,4,5,1]) will return true and its false,
% because the if the length of the list is not even then the last element will not be compared.
% Also we need to use ! to make sure that the last two elements it's only compared using this rule, otherwise
% the execution tree will analyze the last rule too.
isOrderedAsc([First,Second]) :- First=<Second, !.
isOrderedAsc([First,Second|Rest]) :- First=<Second, isOrderedAsc(Rest).

% indexOf(Element,List,Indexes)
% Indexes is a list of positions where element is present
% Examples:
% indexOf(1,[0,1,2,1],L). L = [1,3]
indexOf(Element,List,Indexes) :- iIndexOf(Element,List,Indexes,0).
iIndexOf(Element,[],[],Position).
iIndexOf(Element,[Element|Rest],[Position|IndexesRest],Position) :- NextPosition is Position+1,iIndexOf(Element,Rest,IndexesRest,NextPosition).
iIndexOf(Element,[First|Rest],Indexes,Position) :- Element =\= First, NextPosition is Position+1,iIndexOf(Element,Rest,Indexes,NextPosition).

% insert(Element,List,ResultList)
% ResultList is the result list of inserting the Element on the list List
% Examples:
% insert(1,[2,3,4],ResultList).
% insert(2,[1,3,4],ResultList).
% insert(5,[1,2,3,4],ResultList).
insert(Element,[],[Element]).
insert(Element,[First|Rest],[Element,First|Rest]) :- Element =< First.
insert(Element,[First|ListRest],[First|ResultListRest]) :- Element > First, insert(Element,ListRest,ResultListRest).

% sortAsc(List,SortedList)
% SortedList is the List sorted ascending
% Examples:
% sortAsc([3,1,4,5,10,0,2,2],SortedList).
sortAsc(List,SortedList) :- isortAsc(List,[],SortedList).
% Recrusive predicate to insert each element ordered on a temporal list
isortAsc([],SortedList,SortedList).
isortAsc([First|Rest],TemporalSortedList,SortedList) :- insert(First,TemporalSortedList,Result), isortAsc(Rest,Result,SortedList).

% mcd(M,N,D)
% Pre: M and N >= 1
% D is the maximum common divisor of M and N
mcd(M,N,D) :- M > N, imcd(M,N,N,D),!.
mcd(M,N,D) :- imcd(M,N,M,D),!.
% Recrusive predicate to find the maximum common divisor starting with the smallest value
imcd(M,N,1,1) :- !.
imcd(M,N,A,A) :- M mod A =:= 0,N mod A =:= 0,!.
imcd(M,N,A,D) :- Ap is A-1,imcd(M,N,Ap,D).

% divisors(Number,DivisorsList)
% DivisorsList is a list of numbers which are divisible for Number
% Example:
% divisors(10,DivisorsList).
% divisors(22,DivisorsList).
divisors(Number,DivisorsList) :- idivisors(Number,DivisorsList,1),!.
% recrusive call, starting from the value 1 to Number
idivisors(Number,[Half,Number],Half) :- Half is Number // 2.
idivisors(Number,[Value|DivisorsListRest],Value)
    :- Half is Number // 2, Value < Half,
    Number mod Value =:= 0,
    NextValue is Value+1,
    idivisors(Number,DivisorsListRest,NextValue).
idivisors(Number,DivisorsList,Value)
    :- Half is Number // 2, Value < Half,
    NextValue is Value+1,
    idivisors(Number,DivisorsList,NextValue).

% How to create tuples
predicateTuple(V1,V2,tuple(V1,V2)).
% Usage example:
%   findAndIncrement(Key,TupleList,ResultTupleList)
%   The ResultTupleList is the result of incrementing the tuple with key Key by 1, if the key doesnt exists, creates a new tuple with 1 value
%   Example:
%   findAndIncrement('joan',[tuple('joan',1)],ResultTupleList).
%   findAndIncrement('joan',[],ResultTupleList).
%   findAndIncrement('joan',[tuple('joan',1),tuple('josep',1)],ResultTupleList).
findAndIncrement(Key,[],[tuple(Key,1)]).
findAndIncrement(Key,[tuple(Key,Value)|Rest],[tuple(Key,NewValue)|Rest]) :- !, NewValue is Value+1.
findAndIncrement(Key,[tuple(Key,Value)|Rest],[tuple(Key,Value)|ResultRest]) :- findAndIncrement(Key,Rest,ResultRest).

% Quicksort:
% quicksort(List,SortedList)
% Example:
% quicksort([3,1,4,5,10,0,2,2],SortedList).
quicksort([],[]).
quicksort([First|Rest],SortedList) :-
    divideListBy(First,Rest,Greater,Lesser),
    quicksort(Lesser,SortedLesser),
    quicksort(Greater,SortedGreater),
    append(SortedLesser,[First|SortedGreater],SortedList).

% divideListBy(Value,List,Greater,Lesser)
% Greater are the values greater than the Value on the List, and lesser are the values lesser or equal
% Example:
% divideListBy(4,[3,1,4,5,10,0,2,2],Greater,Lesser).
divideListBy(Value,[],[],[]).
divideListBy(Value,[First|Rest],[First|GreaterRest],Lesser):-
    First > Value, divideListBy(Value,Rest,GreaterRest,Lesser).
divideListBy(Value,[First|Rest],Greater,[First|LesserRest]) :-
    First =< Value, divideListBy(Value,Rest,Greater,LesserRest).

% remove(Value,List,ResultList)
% The ResultList is the List without the value/s Value
% Examples:
% remove(1,[1,2,1,4,5,1,10,11,10],ResultList).
% remove(10,[10,2,1,4,5,10,10,11,10],ResultList).
remove(Value,[],[]).
remove(Value,[Value|Rest],ResultList) :- remove(Value,Rest,ResultList).
remove(Value,[First|Rest],[First|ResultListRest]) :- Value =\= First, remove(Value,Rest,ResultListRest).

% intersection(ListA,ListB,IntersectionSet)
% IntersectionSet is the intersection set between ListA and ListB
% Examples:
% intersection([1,2,3,1,4,1],[3,1,4,5],IntersectionSet).
intersection([],[],[]).
intersection([FirstA|RestListA],ListB,[FirstA|IntersectionSetRest]) :-
     contains(FirstA,ListB),                                % first element of the listA appears also on the listB,
                                                            % we could use also: append(FirstB,[FirstA|RestListB],ListB)
     remove(FirstA,RestListA,NewListA),                     % Remove the element from the bothLists
     remove(FirstA,ListB,NewListB),
     intersection(NewListA,NewListB,IntersectionSetRest),!.   % Calculates the rest of the intersection (! dont check other branches)
intersection([FirstA|RestListA],ListB,IntersectionSet)
    :- intersection(RestListA,ListB,IntersectionSet).       % Otherwise if does not exists, calcualtes the rest of the intersection

factoritza(N,F) :- ifactoritza(N,[],F,2).

ifactoritza(N,L,F,N) :- buscaAfegeixTupla(N,L,F).
ifactoritza(N,L,F,I) :-
    I < N, N mod I =:= 0,buscaAfegeixTupla(I,L,RESULTAT), Ip is N // I, ifactoritza(Ip,RESULTAT,F,2).
ifactoritza(N,L,F,I) :- I < N,Ip is I+1, ifactoritza(N,L,F,Ip).

%llistaUnica(X,LLLISTA,RESULTAT) afegeix a la llista si no existeix
llistaUnica(X,[],[X]).
llistaUnica(X,[X|XS],[X|XS]).
llistaUnica(X,[Y|YS],[Y|FS]) :- X =\= Y, llistaUnica(X,YS,FS).


%unio(X,Y,Z)
unio([],[],[]).
unio([X|XS],Y,Z) :- unio(XS,Y,RESULTAT), llistaUnica(X,RESULTAT,Z).
unio(X,[Y|YS],Z) :- unio(X,YS,RESULTAT), llistaUnica(Y,RESULTAT,Z).

%diferencia(X,Y,Z) suposem no repetits o utilitzem multiconjunt vist mes tard
%sa de fer just al contrari de dalt
%es adir si existeixen els dos costats els eliminem(no afegim) en comptes dafegit
diferencia([],[],[]).
diferencia([X|XS],Y,Z) :-
    append(Yi,[X|Ys],Y), % X apareix a Y
    append(Yi,Ys,Yp),    % creem la nova Y treient x
    diferencia(XS,Yp,Z). %calculem la diferencia de la altre i guardem a ZS
%sino existeix
diferencia([X|XS],Y,[X|ZS]) :- diferencia(XS,Y,ZS).

diferencia(X,[Y|YS],Z) :-
    append(Xi,[Y|Xs],X), % Y apareix a X
    append(Xi,Xs,Xp),    % creem la nova X treient Y
    interseccio(YS,Xp,Z). %calculem la diferencia de la altre i guardem a ZS
%sino existeix
diferencia(X,[Y|YS],[Y|ZS]) :- diferencia(X,YS,ZS).

%multi_a_conjunt(M, C)(X,Y,Z)
multi_a_conjunt([],[]).
multi_a_conjunt([X|XS],Z):- multi_a_conjunt(XS,RESULTAT), llistaUnica(X,RESULTAT,Z).

%una altre forma
multi_a_conjunt2([],[]).
multi_a_conjunt2([X|XS],Z):-
    append(_,[X|_],XS), multi_a_conjunt(XS,Z). %si apareix mes no lafegim
multi_a_conjunt2([X|XS],[X|ZS]):- multi_a_conjunt(XS,ZS). %sino apareix mes lafegim

%rang(B,D,L) L llista de numero enters de B a D, suposem B <= D
rang(B,D,L) :- irang(B,D,L,B).

irang(B,D,[],I) :- I >= D.
irang(B,D,[I|ZS],I) :- I < D, Ip is I+1, irang(B,D,ZS,Ip).

%daus(P,N,L) Sumar P tirant N daus, L llista expressa una forma
daus(0,0,[]).
daus(P,N,[X|LS]) :- rang(1,7,L),append(_,[X|_],L) ,X < 6, P-X >= 0,Pp is P-X,Np is N-1, daus(Pp,Np,LS).

daus2(P,N,L) :- idaus(P,N,L,[1,2,3,4,5,6]).

idaus(0,0,[],NUMEROS).
idaus(P,N,[NUM|RESTA],[NUM|NUMEROS]) :- P-NUM >= 0, Pp is P-NUM, Np is N-1, idaus(Pp,Np,RESTA,[NUM|NUMEROS]).
idaus(P,N,RESULTAT,[NUM|NUMEROS]) :- idaus(P,N,RESULTAT,NUMEROS).

% Z es X canviant nomes un valor
%# test1([],Z)
test1(L,Z) :- append(DAVANT,[X|REDERA],L), append(DAVANT,[Y|REDERA],Z), Y =\= X.
% test1([1,2],Z).

%palidrom(L)
palidrom([]).
% primer == ultim element, com obtenim ultim element?
% [PRIMER(....)ULTIM]
palidrom([X|XS]) :- append(_,[ULTIM],XS),X =:= ULTIM, append(MITG,[ULTIM],XS), palidrom(MITG).

creixent([]).
creixent([X]).
creixent([X,Y|XS]) :- X =< Y, creixent([Y|XS]).

%rota(N,L,R)
rota(0,L,L).
rota(N,[X],[X]).
%rota(N,[X|XS],R) :- Np is N-1,  append(XS,[X],LN), rota(Np,LN,R).

% hihasuma(L) L
% [1,2,3]
% suma(L) -> valor
% 1 -> ihihasuma([2,1,3]) -> append(l) suma(1)
hihasuma(L) :- append(A,[X|B],L), append(A,B,C), suma(C,X).
suma([],0).
suma([X|XS],N) :- suma(XS,Np), N is Np+X.


permutacio([],[]).
permutacio(L,[X|Xs]):- append(V,[X|P],L),
    append(V,P,W),
    permutacio(W,Xs).

eliminaNocurs(N,[],[]).
eliminaNocurs(N,[X|XS],[X|RS]) :- Np is N-1, count1(X,XS,Np), extreure(X,XS,SENSEX), eliminaNocurs(N,SENSEX,RS).
eliminaNocurs(N,[X|XS],R) :- extreure(X,XS,SENSEX), eliminaNocurs(N,SENSEX,R).


%count(NUMERO,LLISTA,VEGADES)
count1(NUMERO,[],0).
count1(NUMERO,[NUMERO|XS],VEGADES) :- count1(NUMERO,XS,Vp),!, VEGADES is Vp+1.
count1(NUMERO,[X|XS],VEGADES) :- count1(NUMERO,XS,VEGADES).

equal([],[]).
equal([X|XS],[X|ZS]) :- equal(XS,ZS).