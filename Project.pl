% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).



subSet([], []).
subSet([_|T], L):-
   subSet(T, L).
subSet([H|T], [H|L]):-
   subSet(T, L).

possibleSubset(L, R):-
   subSet(L, R1),
   permutation(R1, R).

possibleActivity([H|T], Acc, R):-
	H \== activity(X),
	possibleActivity(T, [H|Acc] , R).

possibleActivity([activity(X)|T], Acc, R):-
	possibleSubset(X, R1),
	reverse(Acc, RAcc),
	append(RAcc, [activity(R1)], L1),
	append(L1, T, R).

choosePreferences(Prefs , ChosenPreferences):-
    possibleSubset(Prefs , ChosenPreferences),
	\+member(activity(X), ChosenPreferences).

choosePreferences(Prefs , ChosenPreferences):-
    possibleSubset(Prefs , ChosenPreferences1),
	member(activity(X), ChosenPreferences1),
	possibleActivity(ChosenPreferences1, [], ChosenPreferences).

activitiesRate([],_,0).
activitiesRate([H|T],C,S):-
	customerPreferredActivity(C,H,R),
	activitiesRate(T,C,R1),
	S is R+R1.

preferenceSatisfaction(O, Customer, ChosenPrefs, S):-
	O=offer(Destination, Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests),

	(member(dest(Destination),ChosenPrefs);
	\+member(dest(X), ChosenPrefs)),

	((member(activity(Activities),ChosenPrefs)
	 ,activitiesRate(Activities,Customer,S1));
	(\+member(activity(Activities),ChosenPrefs),
	 S1 is 0)),

	((member(budget(X),ChosenPrefs), X>=Cost );
	(\+member(budget(X),ChosenPrefs))),

	((member(means(M),ChosenPrefs),
	offerMean(O,M),
	customerPreferredMean(Customer,M,S2)));
	(\+member(means(M),ChosenPrefs),
	S2 is 0),

	((member(accommodation(A),ChosenPrefs),
	offerAccommodation(O,A),
	customerPreferredAccommodation(Customer,A,S3)));
	(\+member(accommodation(A),ChosenPrefs),
	S3 is 0),

	S is S1+S2+S3.

before(Y1-M1-D1 , Y2-M2-D2):-
	(Y1 =< Y2);(Y1 = Y2 , M2 >= M1);(Y1 = Y2 , M2 = M1 ,D2 >= D1).

overlapPeriod(period(Y11-M11-D11 ,Y12-M12-D12) , period(Y21-M21-D21 , Y22-M22-D22)):-
    (before(Y11-M11-D11,Y21-M21-D21) , before(Y21-M21-D21 , Y12-M12-D12));
    (before(Y11-M11-D11 , Y22-M22-D22) , before(Y22-M22-D22 , Y12-M12-D12)).

