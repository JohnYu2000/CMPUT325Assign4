#const steps=16.

car(car1).
car(car2).
car(car3).
car(car4).
car(car5).

location(a).
location(b).
location(c).

at(car1, a, 0).
at(car2, a, 0).
at(car3, b, 0).
at(car4, b, 0).
at(car5, a, 0).

at(ferry, a, 0).

goal(T) :-
at(car1, b, T),
at(car2, c, T),
at(car3, a, T),
at(car4, c, T),
at(car5, b, T).