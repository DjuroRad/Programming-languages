path(istanbul, rize).
path(istanbul, ankara).
path(istanbul, izmir).

path(rize, istanbul).
path(rize, ankara).

path(van, ankara).
path(van, gaziantep).

path(ankara, istanbul).
path(ankara, rize).
path(ankara, izmir).
path(ankara, van).
path(ankara, diyarbakir).

path(gaziantep, van).

path(antalya, izmir).
path(antalya, diyarbakir).
path(antalya, erzincan).

path(izmir, istanbul).
path(izmir, ankara).
path(izmir, antalya).

path(erzincan, antalya).
path(erzincan, canakkale).

path(diyarbakir, ankara).
path(diyarbakir, antalya).

path(canakkale, erzincan).



flight(istanbul, rize, 4).
flight(istanbul, ankara, 1).
flight(istanbul, izmir, 2).

flight(rize, istanbul, 4).
flight(rize, ankara, 5).

flight(van, ankara, 4).
flight(van, gaziantep, 3).

flight(ankara, istanbul, 1).
flight(ankara, rize, 5).
flight(ankara, izmir, 6).
flight(ankara, van, 4).
flight(ankara, diyarbakir, 8).

flight(gaziantep, van, 3).

flight(antalya, izmir, 2).
flight(antalya, diyarbakir, 4).
flight(antalya, erzincan, 3).

flight(izmir, istanbul, 2).
flight(izmir, ankara, 6).
flight(izmir, antalya, 2).

flight(erzincan, antalya, 3).
flight(erzincan, canakkale, 6).

flight(diyarbakir, ankara, 8).
flight(diyarbakir, antalya, 4).

flight(canakkale, erzincan, 6).



finish_flight(Dist1, Dist2) :- Dist1 == Dist2.
new_dist(Dist1, Dist2, Res) :- Res is Dist1 - Dist2.
new_dist_add(Dist1, Dist2, Res) :- Res is Dist1 + Dist2.
				 
%route(X,Y,Z) :- flight(X,Y,Z).

%route(X,Y,Z) :- flight(X,To,_),   
	%							 route(To,Y, _), flight(X, To, DistNew), flight(To, Y, DistNew2), write('C = '), Total is DistNew+DistNew2, write(Total).
								 

route(X,Y,Z) :-  flight(X,Y,Z).
route(X,Y,Z) :-  route(X,Y,Z,[]).%[] keeps track of the traversed destinations!
route(X,Y,Z,ListTraveresed) :- flight(X,Y,Z) ; 
												not(member(X,ListTraveresed)), flight(X,To,Dist1), route(To, Y, Dist2, [X|ListTraveresed]), not(member(To,ListTraveresed)), Z is Dist1 + Dist2.