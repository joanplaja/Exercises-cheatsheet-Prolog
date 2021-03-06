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

% union_(ListA,ListB,UnionSet) ( union its implemented by prolog it seems so renamed to union_ )
% UnionSet is the union set between ListA and ListB
% Examples:
% union_([1,2,3,1,4,1],[3,1,3,11,100,4,5],UnionSet).
% Iterate both lists until both are empty
union_([],[],[]).
union_([FirstA|RestListA],ListB,[FirstA|UnionSetRest]) :-
    remove(FirstA,RestListA,NewListA),
    remove(FirstA,ListB,NewListB),
    union_(NewListA,NewListB,UnionSetRest),!.
union_(ListA,[FirstB|RestListB],[FirstB|UnionSetRest]) :-
    remove(FirstB,ListA,NewListA),
    remove(FirstB,RestListB,NewListB),
    union_(NewListA,NewListB,UnionSetRest),!.

% difference(ListA,ListB,DifferenceSet)
% DiffeerenceSet is the set containing elements which appears in one list but not in the other
% Examples:
% difference([1,2,2,3,3,4,5],[3,3,7,4,1,11],DifferenceSet).
difference([],[],[]).
difference([FirstA|RestListA],ListB,DifferenceSet) :-
    contains(FirstA,ListB),             %If the first element of listA appears on listB
    remove(FirstA,RestListA,NewListA),  %Remove the element from both list ( Avoid duplicates )
    remove(FirstA,ListB,NewListB),
    difference(NewListA,NewListB,DifferenceSet),!.
%Otherwise we add the first element of the listA to the DifferenceSet
%There's no problem doing this now because we are not modifying the listB
difference([FirstA|RestListA],ListB,[FirstA|DifferenceSetRest]) :-
    remove(FirstA,RestListA,NewListA), %remove the diferent element from the rest of the list
    difference(NewListA,ListB,DifferenceSetRest),!.
difference(ListA,[FirstB|RestListB],DifferenceSet) :-
    contains(FirstB,ListA),             %If the first element of listB appears on listA
    remove(FirstB,RestListB,NewListB),  %Remove the element from both list ( Avoid duplicates )
    remove(FirstB,ListA,NewListA),
    difference(NewListA,NewListB,DifferenceSet),!.
%Otherwise we add the first element of the listB to the DifferenceSet
difference(ListA,[FirstB|RestListB],[FirstB|DifferenceSetRest]) :-
    remove(FirstB,RestListB,NewListB), %remove the diferent element from the rest of the list
    difference(ListA,NewListB,DifferenceSetRest),!.

%rang(B,D,L) L llista de numero enters de B a D, suposem B <= D

% range(Lowest,Highest,RangeList)
% The RangeList contains the numbers starting from Lowest to Highest
% Examples:
% range(1,10,RangeList).
range(Lowest,Highest,RangeList) :- irange(Highest,RangeList,Lowest).
irange(Highest,[],Value) :- Value > Highest.
irange(Highest,[Value|RangeListRest],Value) :-
    Value =< Highest,
    NextValue is Value+1, irange(Highest,RangeListRest,NextValue).

%daus(P,NDices,L) Sumar P tirant N daus, L llista expressa una forma
% dices(Points,NDices,WaysList)
% The WaysList is a list which contains the NDices numbers representing dices values that sums Points.
% Example:
% dices(10,3,WaysList).
dices(0,0,[]).
dices(Points,NDices,[DiceValue|Rest]) :-
    range(1,6,PossibleDiceValues), % rang(lowest,highest,List) List contains the numbers from lowest to highest  - 1
    contains(DiceValue,PossibleDiceValues),
    Points - DiceValue >= 0,
    RestingPoints is Points - DiceValue,
    NewNDices is NDices - 1,
    dices(RestingPoints,NewNDices,Rest).

% insertSet(Element,Set,ResultSet)
% ResultSet is the result set of inserting the Element on the set ResultSet
% Examples:
% insertSet(5,[1,2,3,4,5],ResultSet).
% insertSet(5,[1,2,3,4,5,6,7],ResultSet).
% insertSet(5,[1,2,3,4,5,6],ResultSet).
% insertSet(5,[],ResultSet).
insertSet(Element,[],[Element]).
insertSet(Element,[First|SetRest],[Element,First|ResultSetRest]) :- Element < First, append([],SetRest,ResultSetRest).
insertSet(Element,[First|SetRest],[First|ResultSetRest]) :- Element > First, insertSet(Element,SetRest,ResultSetRest).
insertSet(Element,[First|SetRest],[First|ResultSetRest]) :- Element == First, append([],SetRest,ResultSetRest).

% toSet(List,Set)
% Example:
% toSet([1,2,1,2,3,3,4,5,5,4,10,1,11,100,1],Set).
% There's multiple ways for doing this:
% 1. The impelmented check if the element still exists, if it does continues iterating util it does not exists more and it's added to the set
% 2. Removing all First elements on the ListRest each time the first is visited
% 3. toSet2 way, start from the bottom of the "tree" with empty set and insert each element using insertSet
toSet([],[]).
toSet([First|ListRest],Set) :-
    contains(First,ListRest), toSet(ListRest,Set),!.
toSet([First|ListRest],[First|SetRest]) :- toSet(ListRest,SetRest).

toSet2([],[]).
toSet2([X|XS],Z):- toSet2(XS,RESULTAT), insertSet(X,RESULTAT,Z).
% toSet2([1,2,1,2,3,3,4,5,5,4,10,1,11,100,1],Set).

% onlyOneDifference(List,NewList)
% The NewList is equal to List just chaing one value
% Example:
% onlyOneDifference([1,2,3],[1,2,4]).
% This implementation cannot be used to find the list because
% elements should be initialized
onlyOneDifference(List,NewList) :-
    append(First,[Value|Rest],List),
    append(First,[DiferentValue|Rest],NewList), DiferentValue =\= Value.