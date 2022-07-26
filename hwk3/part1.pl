%dynamic predicates defined here for students, rooms and courses!
:- dynamic(student/3).
:- dynamic(course/7).
:- dynamic(room/3).
%room(id,capacity,equipments)
room(m1,10,[smartboard,projector, handicapped]).
room(m2,10,[handicapped,projector]).
room(phy1,10,[smartboard,projector,handicapped]).
room(prog1,5,[handicapped,smartboard]).
room(prog2,5,[smartboard, projector]).
room(prog3,5,[handicapped,smartboard, projector]).
room(prog4,5,[handicapped,smartboard] ).

%Course(id,instr,capacity,starttime,finaltime,room)
course(math1, profmath,[12,13],m1, [handicapped]).
course(programming1,profprog,[10,11],m2,[projector]).
course(physics1,profphy,[15,16],phy1,[handicapped]).
course(math2,profmath,[12,13],prog1,[smartboard]).


%Instructor(id,[course1,course2],preferences)
instructor(profmath,[math1, math2],smartboard).
instructor(profphy,[physics1],projector).
instructor(profprog,[programming1],projector).
%student(id,[course1,course2],special)
student(sdnt12,[math1,programming1],f).
student(sdnt15,[programming1,physics1],f).
student(sdnt17,[programming1,math2,physics1],f).
student(sdnt9,[physics1,math2],f).
student(sdnt6,[programming1,physics1],f).
student(sdnt3,[physics1,math1],t).


new_student(ID, StudCourses, HANDICAPPED):- \+ student(ID,_,_), assertz( student(ID, StudCourses,HANDICAPPED)).
new_room(ID,Max,Equip) :- \+ room(ID,_,_),assertz(room(ID,Max,Equip)).
new_course(ID,Instructor,Capacity,T1,T2,Room1,Ekipman) :- \+course(ID,_,_,_,_,_,_), instructor(Instructor,_,_), assertz(course(ID,Instructor,Capacity,T1,T2,Room1,Ekipman)).
students(X):- student(X,_,_).
rooms(X):- room(X,_,_).
courses(X):- course(X,_,_,_,_,_).
%here each course has its start and end time
%when 2 courses start at the same time there will be a conflict here
compare_nums(Start1, Start2) :- Start1 == Start2.
sublist( [], _ ).
sublist( [X|XS], [X|XSS] ) :- sublist( XS, XSS ).
sublist( [X|XS], [_|XSS] ) :- sublist( [X|XS], XSS ).

same(T, Q) :- any(T, Q), !; any(Q, T), !.
any([X|_], [X,_]):- !.
any([X|T], Q) :- member(X, Q), !; any(T, Q), !.

check_conflicts(C1,C2, Time1, Time2):- course(C1,_,Time1,_,_),course(C2,_,Time2,_,_).
schedule_conflicts(C1,C2) :- check_conflicts(C1,C2, Start1, Start2), same(Start1, Start2),!.
enroll(Studentid,C) :- student(Studentid,_,Handicap), course(C,_,_,_,Equipment), (Handicap==f ; (Handicap == t ,member(handicapped,Equipment))).
assignroom(Course,Room):- course(Course,_,_,_,Needs),room(Room,_,Provides),sublist(Needs,Provides).
roomAssign(Room) :- room(Room,_,Provides), course(C,_,_,_,Needs), sublist(Needs, Provides), write(C).
studentAssign(Stndt):- student(Stndt,_,Handicap), course(C,_,_,_,Equipment),( Handicap == f, write(C) ; ( Handicap== t, sublist([handicapped], Equipment), write(C))).
