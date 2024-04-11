:- use_module(library(clpfd)).

/* -------------------------------------------------------------------------------- */
query1(Semester, Name, Total) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    setup(Semester, as1, MaxAs1, PctAs1),
    setup(Semester, as2, MaxAs2, PctAs2),
    setup(Semester, as3, MaxAs3, PctAs3),
    setup(Semester, as4, MaxAs4, PctAs4),
    setup(Semester, midterm, MaxMid, PctMid),
    setup(Semester, final, MaxFinal, PctFinal),
    WeightedAs1 is As1 / MaxAs1 * PctAs1 * 100,
    WeightedAs2 is As2 / MaxAs2 * PctAs2 * 100,
    WeightedAs3 is As3 / MaxAs3 * PctAs3 * 100,
    WeightedAs4 is As4 / MaxAs4 * PctAs4 * 100,
    WeightedMidterm is Midterm / MaxMid * PctMid * 100,
    WeightedFinal is Final / MaxFinal * PctFinal * 100,
    Total is WeightedAs1 + WeightedAs2 + WeightedAs3 + WeightedAs4 + WeightedMidterm + WeightedFinal.

query2(Semester, Students) :-
    findall(Name,
        (c325(Semester, Name, _, _, _, _, Midterm, Final),
        setup(Semester, midterm, MaxMid, _),
        setup(Semester, final, MaxFinal, _),
        MidtermPct is Midterm / MaxMid,
        FinalPct is Final / MaxFinal,
        FinalPct > MidtermPct),
        Students).

query3(Semester, Name, Type, NewMark) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    (Type = as1 ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, NewMark, As2, As3, As4, Midterm, Final));
    Type = as2 ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, As1, NewMark, As3, As4, Midterm, Final));
    Type = as3 ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, As1, As2, NewMark, As4, Midterm, Final));
    Type = as4 ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, As1, As2, As3, NewMark, Midterm, Final));
    Type = midterm ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, As1, As2, As3, As4, NewMark, Final));
    Type = final ->
        retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
        assert(c325(Semester, Name, As1, As2, As3, As4, Midterm, NewMark));
    fail
    ).
query3(Semester, Name, Type, NewMark) :-
    \+ c325(Semester, Name, _, _, _, _, _, _),
    write('record not found'), nl, !.

/* -------------------------------------------------------------------------------- */
/*
Question 2.

Test Cases:

encrypt([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]). ==>> S=9,E=5,N=6,D=7,M=1,O=0,R=8,Y=2
encrypt([D,O,M,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]). ==>> D=5,O=2,M=6,A=4,L=8,G=1,E=9,R=7,B=3,T=0
encrypt([B,A,S,E],[B,A,L,L],[G,A,M,E,S]). ==>> B=7,A=4,S=8,E=3,L=5,G=1,M=9
encrypt([C,R,O,S,S],[R,O,A,D,S],[D,A,N,G,E,R]). ==>> C=9,R=6,O=2,S=3,A=5,D=1,N=8,G=7,E=4
*/
encrypt(W1, W2, W3) :-
    % Determine the length of the words
    length(W1, N),
    length(W2, N),
    length(W3, N1),

    % Ensure W3 length is N or N+1
    (N1 #= N; N1 #= N + 1),

    % Find all unique letters
    append([W1, W2, W3], AllLetters),
    list_to_set(AllLetters, Letters),

    % Assign digits to each letter
    Letters ins 0..9,

    % Check that there are no leading zeros
    [LeadLetter1|_] = W1,
    [LeadLetter2|_] = W2,
    [LeadLetter3|_] = W3,
    LeadLetter1 #\= 0,
    LeadLetter2 #\= 0,
    LeadLetter3 #\= 0,

    % Ensure all letters are distinct
    all_distinct(Letters),

    % Convert words to numbers
    wordToNumber(W1, Num1),
    wordToNumber(W2, Num2),
    wordToNumber(W3, Sum),

    % Num1 + Num2 = Sum
    Sum #= Num1 + Num2,
    
    % Label the Letters to find a solution
    label(Letters).


wordToNumber(L, N) :-
    wordToNumber(L, N, 0).

wordToNumber([], N, N).
wordToNumber([L|R], N, Acc) :-
    N1 #= Acc * 10 + L,
    wordToNumber(R, N, N1).

/* -------------------------------------------------------------------------------- */
/*
Question 3.
*/
/* 
The goal of Sudoku is to fill in a 9 by 9 grid with digits 
so that each column, row, and 3 by 3 section contain the 
numbers between 1 to 9. At the beginning of the game, 
the 9 by 9 grid will have some of the squares filled in. 
Your job is to fill in the missing digits and complete the grid. 

*/

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall-distinct(Rows),
        % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall-distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

grid(Size, Grid) :-
    length(Grid, Size),
    maplist(same_length(Grid), Grid).

xtranspose([[]|_], []).
xtranspose(Matrix, [Row|Rows]) :-
    transpose_1st_col(Matrix, Row, RestMatrix),
    xtranspose(RestMatrix, Rows).
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_1st_col(Rows, Hs, Ts).

xall-distinct([]).
xall-distinct([Row|Rows]) :-
    distinct_row(Row),
    xall-distinct(Rows).
distinct_row([]).
distinct_row([A|L]) :-
    diff(A,L),
    distinct_row(L).
diff(_,[]).
diff(A,[B|L]) :-
    A #\= B,
    diff(A,L).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
	 [_,2,_,_,_,_,4,5,6],
	 [_,_,3,2,_,5,_,_,_],
	 [_,_,_,4,_,_,8,_,5],
	 [7,8,9,_,5,_,_,_,_],
	 [_,_,_,_,_,6,2,_,3],
	 [8,_,1,_,_,_,7,_,_],
	 [_,_,_,1,2,3,_,8,_],
	 [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example:
   ?- t(Rows).
   [1,5,6,8,9,4,3,2,7]
   [9,2,8,7,3,1,4,5,6]
   [4,7,3,2,6,5,9,1,8]
   [3,6,2,4,1,7,8,9,5]
   [7,8,9,3,5,2,6,4,1]
   [5,1,4,9,8,6,2,7,3]
   [8,3,1,5,4,9,7,6,2]
   [6,9,7,1,2,3,5,8,4]
   [2,4,5,6,7,8,1,3,9]
   Rows = [[1, 5, 6, 8, 9, 4, 3, 2|...], [9, 2, 8, 7, 3, 1, 4|...], [4, 7, 3, 2, 6, 5|...], [3, 6, 2, 4, 1|...], [7, 8, 9, 3|...], [5, 1, 4|...], [8, 3|...], [6|...], [...|...]].

    [[1,_,_,8,_,4,_,_,_],[_,2,_,_,_,_,4,5,6],[_,_,3,2,_,5,_,_,_],[_,_,_,4,_,_,8,_,5],[7,8,9,_,5,_,_,_,_],[_,_,_,_,_,6,2,_,3],[8,_,1,_,_,_,7,_,_],[_,_,_,1,2,3,_,8,_],[2,_,5,_,_,_,_,_,9]]
*/

/* -------------------------------------------------------------------------------- */
/*
Question 4.
*/
% Example data
paper(1, lily, xxx, ai).
paper(2, peter, john, database).
paper(3, ann, xxx, theory).
paper(4, ken, lily, network).
paper(5, kris, xxx, games).

reviewer(lily, theory, network).
reviewer(john, ai, theory).
reviewer(peter, database, network).
reviewer(ann, theory, network).
reviewer(kris, theory, games).
reviewer(ken, database, games).
reviewer(bill, database, ai).
reviewer(jim, theory, games).

workLoadAtMost(2).

assign(W1, W2) :-
    assignHelper(W1_int, W2_int),
    findall(Name, reviewer(Name, _, _), Reviewers),  % Gets all the names of the reviewers
    maplist(idToNameWrapper(Reviewers), W1_int, W1),
    maplist(idToNameWrapper(Reviewers), W2_int, W2).

% Assign papers to reviewers
assignHelper(W1_int, W2_int) :-
    findall(ID, paper(ID, _, _, _), Papers),  % Gets all the possible paper IDs
    findall(Name, reviewer(Name, _, _), Reviewers),  % Gets all the names of the reviewers
    allExpertises(Expertises),  % Gets all the expertises
    length(Papers, NumPapers),  % Gets the number of papers
    length(W1_int, NumPapers),  % Each paper has at least one reviewer
    length(W2_int, NumPapers),  % Each paper has at least one more reviewer. Therefore, each paper has 2 reviewers.
    length(Reviewers, NumReviewers),
    W1_int ins 1..NumReviewers,
    W2_int ins 1..NumReviewers,
    assign_papers(Papers, W1_int, W2_int, Reviewers, Expertises),
    flatten([W1_int, W2_int], AllAssignments),
    workLoadAtMost(K),
    enforce_workload(AllAssignments, K),
    label(W1_int),
    label(W2_int).

% Assign each paper to two reviewers
assign_papers([], [], [], _, _).
assign_papers([P|Ps], [ID1|R1s], [ID2|R2s], Reviewers, Expertises) :-
    paper(P, CoAuthor1, CoAuthor2, Subject),
    reviewer(R1, Expertise1R1, Expertise2R1),
    reviewer(R2, Expertise1R2, Expertise2R2),
    nameToID(CoAuthor1, Reviewers, CA1),
    nameToID(CoAuthor2, Reviewers, CA2),
    nameToID(R1, Reviewers, ID1),
    nameToID(R2, Reviewers, ID2),
    nameToID(Expertise1R1, Expertises, E1R1),
    nameToID(Expertise2R1, Expertises, E2R1),
    nameToID(Expertise1R2, Expertises, E1R2),
    nameToID(Expertise2R2, Expertises, E2R2),
    nameToID(Subject, Expertises, Sub),
    ID1 #\= ID2,
    ID1 #\= CA1, ID1 #\= CA2,
    ID2 #\= CA1, ID2 #\= CA2,
    (Sub #= E1R1; Sub #= E2R1),
    (Sub #= E1R2; Sub #= E2R2),
    assign_papers(Ps, R1s, R2s, Reviewers, Expertises).

% Ensure no reviewer is assigned more than K papers
enforce_workload(AllAssignments, K) :-
    findall(R, reviewer(R, _, _), Reviewers),
    maplist(enforce_individual_workload(K, AllAssignments), Reviewers).

enforce_individual_workload(K, AllAssignments, Reviewer) :-
    include(==(Reviewer), AllAssignments, Assignments),  % Filters the AllAssignments list so that it only contains the ID of a specific reviewer
    length(Assignments, L),
    L #=< K.

% Assign IDs to each reviewer
assignIDs(Names, IDs) :-
    assignIDsHelper(Names, IDs, 1).

assignIDsHelper([], [], _).
assignIDsHelper([_|Names], [ID|IDs], ID) :-
    NextID is ID + 1, % Prepare the next ID
    assignIDsHelper(Names, IDs, NextID).

% Name to ID converstion
nameToID(Name, List, ID) :-
    nameToIDHelper(Name, List, 1, ID).
nameToIDHelper(_, [], _, -1).
nameToIDHelper(Name, [Name1|_], CurrID, CurrID) :-
    Name = Name1.
nameToIDHelper(Name, [Name1|Names], CurrID, ID) :-
    Name \= Name1,
    NextID is CurrID + 1,
    nameToIDHelper(Name, Names, NextID, ID).

% ID to Name conversion
% idToName(ID, List, Name) :-
%     idToNameHelper(ID, List, 1, Name).
% idToNameHelper(_, [], _, -1).
% idToNameHelper(ID, [Name|_], CurrID, Name) :-
%     ID = CurrID.
% idToNameHelper(ID, [_|Names], CurrID, Name) :-
%     ID \= CurrID,
%     NextID is CurrID + 1,
%     idToNameHelper(ID, Names, NextID, Name).
idToName(ID, List, Name) :-
    nth1(ID, List, Name).

allExpertises(Expertises) :-
    findall(Expertise, (reviewer(_, X, Y), Expertise = X; reviewer(_, X, Y), Expertise = Y), ExpertisesList),
    list_to_set(ExpertisesList, Expertises).

/*
This wrapper function is used to map the IDs of the reviewers to their corresponding names. The reason I
have this function is because the idToName predicate defined above doesn't work when invoked in the
maplist predicate.
*/
idToNameWrapper(Reviewers, ID, Name) :-
    idToName(ID, Reviewers, Name).
