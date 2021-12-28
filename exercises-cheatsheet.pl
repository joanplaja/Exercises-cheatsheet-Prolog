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

%mcd(M,N,D) D maxim comu divisor de M i N
mcd(M,N,D) :- M > N, imcd(M,N,N,D).
mcd(M,N,D) :- imcd(M,N,M,D).

imcd(M,N,0,D).
imcd(M,N,A,A) :- M mod A =:= 0,N mod A =:= 0,!.
imcd(M,N,A,D) :- Ap is A-1,imcd(M,N,Ap,D).

entupla(V1,V2,tupla(V1,V2)).

buscaAfegeixTupla(NOMBRE,[],[tupla(NOMBRE,1)]).
buscaAfegeixTupla(NOMBRE,[tupla(NOMBRE,E)|TS],[tupla(NOMBRE,Ep)|TS]) :- !, Ep is E+1.
buscaAfegeixTupla(NOMBRE,[tupla(N,E)|TS],[tupla(N,E)|LS]) :- buscaAfegeixTupla(NOMBRE,TS,LS).

factoritza(N,F) :- ifactoritza(N,[],F,2).

ifactoritza(N,L,F,N) :- buscaAfegeixTupla(N,L,F).
ifactoritza(N,L,F,I) :-
    I < N, N mod I =:= 0,buscaAfegeixTupla(I,L,RESULTAT), Ip is N // I, ifactoritza(Ip,RESULTAT,F,2).
ifactoritza(N,L,F,I) :- I < N,Ip is I+1, ifactoritza(N,L,F,Ip).

%MIRAR AL REVES REPEASSAR..
divisors(N,L) :- idivisors(N,L,1).

idivisors(N,[N],N).
idivisors(N,[I|F],I) :- I < N, N mod I =:= 0, Ip is I+1, idivisors(N,F,Ip), !.
idivisors(N,F,I) :- I < N, Ip is I+1, idivisors(N,F,Ip).

%llistaUnica(X,LLLISTA,RESULTAT) afegeix a la llista si no existeix
llistaUnica(X,[],[X]).
llistaUnica(X,[X|XS],[X|XS]).
llistaUnica(X,[Y|YS],[Y|FS]) :- X =\= Y, llistaUnica(X,YS,FS).


%unio(X,Y,Z)
unio([],[],[]).
unio([X|XS],Y,Z) :- unio(XS,Y,RESULTAT), llistaUnica(X,RESULTAT,Z).
unio(X,[Y|YS],Z) :- unio(X,YS,RESULTAT), llistaUnica(Y,RESULTAT,Z).

%interseccio(X,Y,Z) suposem que x i y no te repeticions o utilitzas multiconjunt abans
interseccio([],[],[]).
interseccio([X|XS],Y,[X|ZS]) :-
    append(Yi,[X|Ys],Y), % X apareix a Y
    append(Yi,Ys,Yp),    % creem la nova Y treient x
    interseccio(XS,Yp,ZS). %calculem la interseccio de la altre i guardem a ZS
%sino existeix
interseccio([X|XS],Y,Z) :- interseccio(XS,Y,Z).

interseccio(X,[Y|YS],[Y|ZS]) :-
    append(Xi,[Y|Xs],X), % Y apareix a X
    append(Xi,Xs,Xp),    % creem la nova X treient Y
    interseccio(YS,Xp,ZS). %calculem la interseccio de la altre i guardem a ZS
%sino existeix
interseccio(X,[Y|YS],Z) :- interseccio(X,YS,Z).

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

%ocurrencies X,L,O 0 es llista de posicions de L on apareix X
% [] for(i = length) (i==x)[i] else i++
ocurrencies(X,L,O) :- iocurrencies(X,L,O,0).
iocurrencies(X,[],[],N).
iocurrencies(X,[X|XS],[I|YS],I) :- Ip is I+1, iocurrencies(X,XS,YS,Ip).
iocurrencies(X,[Y|XS],YS,I) :- Ip is I+1, iocurrencies(X,XS,YS,Ip).
%        Y   XS                YS
%inserta 1, [0,4,5,6,7] ->  [0,1--]
% INSERTA 1 [4,5,6,7] -> [1,4,5,6,7]
inserta(Y,[],[Y]).
inserta(Y,[X|XS],[Y,X|XS]) :- Y =< X.
inserta(Y,[X|XS],[X|ZS]) :- inserta(Y,XS,ZS).

%rota(N,L,R)
rota(0,L,L).
rota(N,[X],[X]).
%rota(N,[X|XS],R) :- Np is N-1,  append(XS,[X],LN), rota(Np,LN,R).

%mcd 10 5 -> 5 < 10 -> provem 5 i anem baixant
% else provem a partir de laltre
% i = 5 fins que no sigui 1 _> si es 1 tornem

mcd(X1,X2,R) :- X1 > X2, imcd(X1,X2,R,X2),!.
mcd(X1,X2,R) :- imcd(X1,X2,R,X1).

%imcd(X1,X2,R,I)
imcd(X1,X2,1,1).
imcd(X1,X2,I,I) :- X1 mod I =:= 0, X2 mod I =:= 0,!.
imcd(X1,X2,R,I) :- Ip is I-1, imcd(X1,X2,R,Ip).

%divisors(NUMERO,LLISTA) LLISTA TE ELS DIVISORS DEL NUMERO
% 10  -> -- mira el mod fer append
divisors(N,[N|LS]) :- Np is N // 2, idivisors(N,LS,Np).

idivisors(N,[1],1) :- !.
idivisors(N,[I|LS],I) :- N mod I =:= 0,!, Ip is I-1, idivisors(N,LS,Ip).
idivisors(N,L,I) :- Ip is I-1, idivisors(N,L,Ip).

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


%eliminaNocurs(N,L,LR):
%eliminaNocurs(N,[],[]).
%eliminaNocurs(N,L,[X|XS]) :-append(A,[X|B],L), append(A,B,C),Np is N-1, count1(X,C,Np),!, extreure(X,L,SENSEX), eliminaNocurs(N,SENSEX,XS).

eliminaNocurs(N,[],[]).
eliminaNocurs(N,[X|XS],[X|RS]) :- Np is N-1, count1(X,XS,Np), extreure(X,XS,SENSEX), eliminaNocurs(N,SENSEX,RS).
eliminaNocurs(N,[X|XS],R) :- extreure(X,XS,SENSEX), eliminaNocurs(N,SENSEX,R).


%count(NUMERO,LLISTA,VEGADES)
count1(NUMERO,[],0).
count1(NUMERO,[NUMERO|XS],VEGADES) :- count1(NUMERO,XS,Vp),!, VEGADES is Vp+1.
count1(NUMERO,[X|XS],VEGADES) :- count1(NUMERO,XS,VEGADES).

%extreure(NUMERO,LLISTA,RESULTAT)
extreure(NUMERO,[],[]).
extreure(NUMERO,[NUMERO|XS],RESULTAT) :- extreure(NUMERO,XS,RESULTAT).
extreure(NUMERO,[X|XS],[X|RESULTAT]) :- X =\= NUMERO, extreure(NUMERO,XS,RESULTAT).

test1(L,Z) :- append(DAVANT,[X|REDERA],L), append(DAVANT,[Y|REDERA],Z).
% test1([1,2],Z).
% test1([1,2,3],Z).

equal([],[]).
equal([X|XS],[X|ZS]) :- equal(XS,ZS).

test2(w4(X),w4(Z)):- w4(X), w4(Z), \+(equal(X,Z)), test1(X,Z).
%test2(w4("aaas"),X).


