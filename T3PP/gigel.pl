:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.

match_rule([], _, rule(_, _, _, _, _)):- false, !.
match_rule(_, _, rule([], _, _, _, _)):- false, !.
match_rule(Tokens, UserMemory, rule(List, _, _, _, _)):- get_emotion(UserMemory, Emotie), Emotie = neutru, Tokens = List, !.
match_rule(Tokens, UserMemory, rule(List, Rasp, _, _, _)):- get_emotion(UserMemory, Emotie), Emotie \= gol, flatten(Rasp, Lst), member(Emotie, Lst), Tokens = List, !.
match_rule(Tokens, UserMemory, rule(List, _, _, _, _)):- get_emotion(UserMemory, Emotie), Emotie = gol, Tokens = List.



% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.

find_matching_rules([], _, _, []).
find_matching_rules(_Tokens, [], _, []).
find_matching_rules(Tokens, [HeadR | RestR], UserMemory, [HeadR | MatchingRules]):-match_rule(Tokens, UserMemory, HeadR), find_matching_rules(Tokens, RestR, UserMemory, MatchingRules), !.
find_matching_rules(Tokens, [_ | RestR], UserMemory, MatchingRules):- find_matching_rules(Tokens, RestR, UserMemory, MatchingRules).




% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

rules_to_list([], []).
rules_to_list([rule(_, ListaRasp, _, _, _) | Rest], [ListaRasp | Raspuns]):-rules_to_list(Rest, Raspuns).

select_answer(Tokens, UserMemory, BotMemory, Raspuns, []) :- 
	rules(Keywords, Rules),
	ord_subset(Keywords, Tokens), 
	find_matching_rules(Tokens, Rules, UserMemory, Matches),
	rules_to_list(Matches, List),
	find_min_all(List, BotMemory, ListF),
	find_min(ListF, ListF,BotMemory, Raspuns),
	(\+member(Tokens, [[pa], [bye], [la, revedere]])), !.


select_answer(Tokens, UserMemory, BotMemory, Raspuns, [exit]) :- 
	rules(Keywords, Rules),
	ord_subset(Keywords, Tokens), 
	find_matching_rules(Tokens, Rules, UserMemory, Matches),
	rules_to_list(Matches, List),
	find_min_all(List, BotMemory, ListF),
	find_min(ListF, ListF,BotMemory, Raspuns).


find_min_all([], _, []).
find_min_all([Head | Rest], Memory, [Rasp | Lista]):-find_min(Head, Head,Memory, Rasp), find_min_all(Rest, Memory, Lista).


find_min([], [HeadF | _], _, HeadF).
find_min([Head | _], _ListF, memory{}, Head).
find_min([Head | _], _ListF, Memory, Head):- get_answer(Head, Memory, Val), Val =:= 0, !.
find_min([_ | Rest], ListF, Memory, Answer):-find_min(Rest, ListF, Memory, Answer).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
handle_actions(Actions) :- \+memberchk(exit, Actions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.

find_occurrences(Memory, Result):- dict_keys(Memory, Keys), findall(Answer, (member(Key, Keys), get_value(Memory, Key, Val), words(Key, Propozitie), add_to_list(Propozitie, Val, Answer)), Ans), flatten(Ans, ResultAux), sort(0, @=<, ResultAux, Resul1), unifyValues(Resul1, Resul2), remove_duplicates(Resul2, Result).


remove_duplicates([], []).
remove_duplicates([(Key, Val) | []], [(Key, Val)]).
remove_duplicates([(Key, Val) , (Key, Val) | Rest], [(Key, Val) | Ans]):- remove_duplicates(Rest, Ans), !.
remove_duplicates([(Key, Val), (Key1, Val1) | Rest], [(Key, Val) | Ans]):- remove_duplicates([(Key1, Val1) | Rest], Ans).

add_to_list([], _, []).
add_to_list([Head | Rest], Val, [(Head, Val) | Ans]):-add_to_list(Rest, Val, Ans).

unifyValues([(Key, Value) | []], [(Key, Value)]).
unifyValues([(Key, Val) , (Key, Val1) | Rest], [(Key, Val2) | Ans]):-Val2 is Val + Val1, unifyValues([(Key, Val2) | Rest], Ans), !.
unifyValues([(Key, Val) , (Key, Val) | Rest], [(Key, Val2) | Ans]):-Val2 is Val + Val, unifyValues([(Key, Val2) | Rest], Ans), !.
unifyValues([(K, V), (Key, Val) | Rest], [(K, V) | Ans]):-unifyValues([(Key, Val) | Rest], Ans).  

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.

get_happy_score(UserMemory, Score):-find_occurrences(UserMemory, Lista), find_happy(Lista, Score).

find_happy([], 0).
find_happy([(Key, Val) | Rest], Score):-happy(Key), find_happy(Rest, Score2), Score is Val + Score2, !.
find_happy([_ | Rest], Score):-find_happy(Rest, Score).


% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
get_sad_score(UserMemory, Score) :- find_occurrences(UserMemory, Lista), find_sad(Lista, Score).

find_sad([], 0).
find_sad([(Key, Val) | Rest], Score):-sad(Key), find_sad(Rest, Score2), Score is Val + Score2, !.
find_sad([_ | Rest], Score):-find_sad(Rest, Score).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
get_emotion(UserMemory, fericit) :- get_happy_score(UserMemory, Happy), get_sad_score(UserMemory, Sad), Happy > Sad, !.
get_emotion(UserMemory, trist) :- get_happy_score(UserMemory, Happy), get_sad_score(UserMemory, Sad), Happy < Sad, !.
get_emotion(memory{}, gol).
get_emotion(_, neutru).


% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
get_tag_score(Tag, UserMemory, Score) :- tag(Tag, Lista), find_occurrences(UserMemory, Raspuns), find_tag(Lista, Raspuns, Score).

find_tag(_, [], 0).
find_tag(Lista, [(Key, Val) | Rest], Score):-memberchk(Key, Lista), find_tag(Lista, Rest, Score2), Score is Val + Score2, !.
find_tag(Lista, [_ | Rest], Score):-find_tag(Lista, Rest, Score).

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_emotion(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.

get_tag(memory{}, none).
get_tag(UserMemory, Tag) :-get_tag_score(Tag, UserMemory, _).
