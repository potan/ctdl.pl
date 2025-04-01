
:- table ct_not/1.
:- table ct_triple/3.
:- table ct_and/2.
:- table ct_or/2.

:- table ct_disjunction/2.
:- table mk_disjunction_or/3.

:- table meta_triple/4.

:- table mod_not/2.
:- table optional_obligatory/1.
:- table not_not_false/1.
:- table not_optional/1.
:- table not_obligatory/1.

:- discontiguous meta_triple/4.
:- discontiguous ct_triple/3.

:- multifile ct_triple/3.
:- multifile meta_triple/4.
:- multifile ct_and/2.
:- multifile ct_or/2.
:- multifile ct_disjunction/2.
:- multifile ct_ThematicRole/1.
:- multifile ct_Eventuality/1.

meta_triple(Id, S, P, O) :-
  ct_triple(Id, object, O),
  ct_triple(Id, predicate, P),
  ct_triple(Id, subject, S).

all_is([], _).
all_is([E | L], Type) :- ct_type(E, Type), all_is(L, Type).

is_all(_, []).
is_all(I, [Type|Types]) :- ct_type(I, Type), is_all(I, Types).
 
meta_types(S, P, O, Types) :- meta_triple(Id, S, P, O), is_all(Id, Types).

ct_not([E1, E2]) :- ct_triple(E1, not, E2).
ct_not([E1, E2]) :- ct_triple(E2, not, E1).

ct_modality(R) :- member(R, [rexist, obligatory, permitted, optional]).

ct_type(S, O) :- ct_triple(S, type, O).
ct_types(S, L) :- is_all(S, L).



%CONSTRUCT{?e1 :not ?e2}
%  WHERE{?e1 rdf:type ?c. ?e2 rdf:type ?c. ?c rdf:type :Eventuality. FILTER(?e1!=?e2)
%        ?trn rdf:type :ThematicRole. ?e1 ?trn ?vn. ?r rdf:type :false,:hold; rdf:subject ?e2; rdf:predicate ?trn; rdf:object ?vn.
%  NOT EXISTS{?tr rdf:type :ThematicRole. FILTER(?tr!=?trn) ?e1 ?tr ?tv1. NOT EXISTS{?e2 ?tr ?tv2}}
%  NOT EXISTS{?tr rdf:type :ThematicRole. FILTER(?tr!=?trn) ?e2 ?tr ?tv2. NOT EXISTS{?e1 ?tr ?tv1}}
%  NOT EXISTS{?tr rdf:type :ThematicRole. FILTER(?tr!=?trn) ?e1 ?tr ?tv1. ?e2 ?tr ?tv2. FILTER(?tv1!=?tv2)}}"""
ct_triple(E1, not, E2) :-
  ct_type(E1, C), ct_type(E2, C), ct_Eventuality(C),
  ct_ThematicRole(TRN),
  ct_triple(E1, TRN, VN),
  meta_types(E2, TRN, VN, [false, hold]),
  E1 \= E2,
  \+((ct_ThematicRole(TR1), ct_triple(E1, TR1, _), TR1 \= TRN, \+(ct_ThematicRole(E2, TR1, _)))),
  \+((ct_ThematicRole(TR2), ct_trimple(E2, TR2, _), TR2 \= TRN, \+(ct_ThematicRole(E1, TR2, _)))),
  \+((ct_ThematicRole(TR3), ct_triple(E1, TR3, TV1), ct_ThematicRole(TR3), ct_triple(E2, TR3, TV2), \+(TR1 = TR2, TV1 = TV2))).

%CONSTRUCT{?e1 rdf:type ?ropom. ?e2 rdf:type ?ropom}
%  WHERE{?ea :and1 ?e1. ?ea :and2 ?e2. ?ea rdf:type ?ropom. VALUES ?ropom {:Rexist :Obligatory :Permitted :Optional}}
ct_triple(E1, type, Ropom) :- ct_and(EA, L), member(E1, L), ct_type(EA, Ropom),
                      ct_modality(Ropom).

%CONSTRUCT{?ea rdf:type ?ropom}
%  WHERE{?ea :and1 ?e1. ?ea :and2 ?e2. ?e1 rdf:type ?ropom. ?e2 rdf:type ?ropom. VALUES ?ropom {:Rexist :Obligatory :Permitted :Optional}}
ct_triple(EA, type, Ropom) :-
  ct_modality(Ropom),
  ct_and(EA, L), all_is(L, Ropom).

%CONSTRUCT{?e2 rdf:type ?rom}
%  WHERE{VALUES ?rom {:Rexist :Obligatory}
%        ?eo rdf:type ?rom. {?eo :or1 ?e1; :or2 ?e2}UNION{?eo :or1 ?e2; :or2 ?e1}
%        ?en1 rdf:type ?rom. {?e1 :not ?en1}UNION{?en1 :not ?e1}}
ct_triple(E2, type, Rom) :- member(Rom, [rexist, obligatory]),
                    ct_or(EO, L), member(E2, L), member(E1, L), E2 \= E1,
                    ct_type(EO, Rom), ct_type(E1, Rom), 
                    ct_not(NL), member(E1, NL), member(E2, NL).


%CONSTRUCT{?r2 rdf:type :hold}
%  WHERE{{?r1 :disjunction ?r2}UNION{?r2 :disjunction ?r1}
%        ?r1 rdf:type ?tvr1; rdf:subject ?s; rdf:predicate ?p; rdf:object ?o.
%        ?rn1 rdf:type :hold,?tvrn1; rdf:subject ?s; rdf:predicate ?p; rdf:object ?o.
%        FILTER(((?tvr1=:true)&&(?tvrn1=:false))||((?tvr1=:false)&&(?tvrn1=:true)))}
ct_triple(R2, type, hold) :-
  ct_disjunction(R1, R2),
  member((TVR1, TVRN1), [(true, false), (false, true)]),
  meta_triple(R1, S, P, O), ct_type(R1, TVR1),
  meta_triple(RN1, S, P, O), ct_types(RN1, [hold, TVRN1]).


%CONSTRUCT{?s ?p ?o}WHERE{?r a :necessary, :hold; rdf:subject ?s; rdf:predicate ?p; rdf:object ?o}
%CONSTRUCT{?s ?p ?o}WHERE{?r a :true,:hold. ?r rdf:subject ?s. ?r rdf:predicate ?p. ?r rdf:object ?o}
ct_triple(S, P, O) :- member(M, [true, necessary]), meta_types(S, P, O, [M, hold]).


%CONSTRUCT{[rdf:type :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Rexist]}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e.} ?e rdf:type :Rexist.
%        NOT EXISTS{?f rdf:type :false,:hold; 
%        rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Rexist; }}
ct_triple(not_not_false(NE), type, false) :- not_not_false(NE).
ct_triple(not_not_false(NE), type, hold) :- not_not_false(NE).
meta_triple(not_not_false(NE), NE, type, rexist) :- not_not_false(NE).

not_not_false(NE) :-
  ct_not([E, NE]), ct_type(E, rexist),
  \+((meta_triple(F, NE, type, rexist), ct_types(F, [false, hold]))).


%CONSTRUCT{?ne rdf:type :Rexist}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e.}
%        ?r rdf:type :false,:hold;
%           rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist.}
ct_triple(NE, type, rexist) :-
  ct_not([NE, E]),
  meta_types(E, type, rexist, [false, hold]).


%CONSTRUCT{[rdf:type :true; rdf:subject ?e1; rdf:predicate rdf:type; rdf:object ?rpom] 
%                 :disjunction 
%                 [rdf:type :true; rdf:subject ?e2; rdf:predicate rdf:type; rdf:object ?rpom]}
% WHERE{?eo :or1 ?e1. ?eo :or2 ?e2. ?eo a ?rpom. VALUES ?rpom {:Rexist :Permitted :Optional}
%       NOT EXISTS{?r1 rdf:type :true; rdf:predicate rdf:type; rdf:object ?rpom. 
%                  ?r2 rdf:type :true; rdf:predicate rdf:type; rdf:object ?rpom.
%                  {?r1 rdf:subject ?e1. ?r2 rdf:subject ?e2.}UNION{?r1 rdf:subject ?e2. ?r2 rdf:subject ?e1.}}}
mk_disjunction_or(E1, E2, RPOM) :-
  ct_or(EO, [E1, E2]),
  ct_type(EO, RPOM),
  member(RPOM, [rexist, permitted, optional]).
meta_triple(disjunction_arg(E1, RPOM), E1, type, RPOM) :- mk_disjunction_or(E1, _, RPOM).
meta_triple(disjunction_arg(E2, RPOM), E2, type, RPOM) :- mk_disjunction_or(_, E2, RPOM).
ct_disjunction(disjunction_arg(E1, RPOM), disjunction_arg(E2, RPOM)) :- mk_disjunction_or(E1, E2, RPOM).

%CONSTRUCT{?eo a ?rpom}
%  WHERE{{?eo :or1 ?e1. ?eo :or2 ?e2.}UNION{?eo :or1 ?e2. ?eo :or2 ?e1.}
%        {?e1 rdf:type ?rpom}UNION{?e2 rdf:type ?rpom} VALUES ?rpom {:Rexist :Permitted :Optional}}
ct_triple(EO, type, rpom) :-
   member(T, [rexist, permitted, optional]),
   ct_or(EO, L),
   member(E, L),
   ct_type(E, T).



%CONSTRUCT{[rdf:type :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist]
%                :is-in-contradiction-with ?r.}
%   WHERE{?e rdf:type :Rexist.
%         ?r rdf:type :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist.
%         NOT EXISTS{{?t :is-in-contradiction-with ?r}UNION{?r :is-in-contradiction-with ?t}
%                    ?t rdf:type :true,:hold;
%                    rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist}}
is_in_contradiction_with(meta(E, type, rexist, [true, hold]), R) :-
   ct_type(E, rexist),
   meta_triple(R, E, type, rexist),
   ct_types(R, [false, hold]).


%CONSTRUCT{[rdf:type :true,:hold; rdf:subject ?e; rdf:predicate ?tr; rdf:object ?tv]:is-in-contradiction-with ?r}
%  WHERE{?tr rdf:type :ThematicRole. ?e ?tr ?tv. ?r rdf:type :false,:hold; rdf:subject ?e; rdf:predicate ?tr; rdf:object ?tv.
%        NOT EXISTS{{?t :is-in-contradiction-with ?r} UNION {?r :is-in-contradiction-with ?t} 
%                   ?t rdf:type :true,:hold; rdf:subject ?e; rdf:predicate ?tr; rdf:object ?tv}} 
is_in_contradiction_with(meta(E, TR, TV, [true, hold]), R) :-
  ct_ThematicRole(TR), ct_triple(E, TR, TV),
  meta_triple(R, E, TR, TV),
  ct_type(R, [false, hold]).


%CONSTRUCT{[rdf:type :true,:hold; rdf:subject ?eo; rdf:predicate rdf:type; rdf:object :Obligatory]
%                 :is-complied-with-by
%                    [rdf:type :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist]}
%  WHERE{?eo rdf:type :Obligatory, ?c. ?e rdf:type :Rexist, ?c. ?c rdf:type :Eventuality.
%        NOT EXISTS{?tr rdf:type :ThematicRole. ?eo ?tr ?vo. NOT EXISTS{?e ?tr ?ve}}
%        NOT EXISTS{?tr rdf:type :ThematicRole. ?eo ?tr ?vo. ?e ?tr ?ve. FILTER(?vo!=?ve)}
%        NOT EXISTS{?eor :is-complied-with-by ?er.
%                   ?eor rdf:type :true,:hold; rdf:subject ?eo; rdf:predicate rdf:type; rdf:object :Obligatory.
%                   ?er rdf:type :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist}}
is_complied_with_by(meta(EO, type, obligatory), E) :-
   ct_types(EO, [obligatory, C]), C \= obligatory, ct_Eventuality(C),
   ct_types(E, [rexist, C]), 
   \+((ct_ThematicRole(TR1), ct_triple(EO, TR1, _), \+(ct_triple(E, TR1, _)))),
   \+((ct_ThematicRole(TR2), ct_triple(EO, TR2, VO), ct_triple(E, TR2, VE), VO \= VE)).


%CONSTRUCT{?epr :is-violated-by [a :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist]}
%  WHERE{?epr rdf:type :false,:hold; rdf:subject ?ep; rdf:predicate rdf:type; rdf:object :Permitted.
%        ?ep rdf:type ?c. ?e rdf:type :Rexist, ?c. ?c rdf:type :Eventuality.
%        NOT EXISTS{?tr rdf:type :ThematicRole. ?ep ?tr ?vp. NOT EXISTS{?e ?tr ?ve}}
%        NOT EXISTS{?tr rdf:type :ThematicRole. ?ep ?tr ?vp. ?e ?tr ?ve. FILTER(?vp!=?ve)}
%        NOT EXISTS{?epr :is-violated-by ?te. 
%                   ?te rdf:type :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Rexist}}
is_violated_by(EPR, meta(E, type, rexist, [true, hold])) :-
   meta_triple(EPR, EP, type, permitted), ct_types(EPR, [false, hold]),
   ct_rype(E, C), ct_Eventuality(C), ct_type(E, rexist),
   \+((ct_ThematicRole(TR1), ct_triple(EP, TR1, _), \+(ct_triple(E, TR1, _)))),
   \+((ct_ThematicRole(TR2), ct_triple(EP, TR2, VP), ct_triple(E, TR2, VE), VP \= VE)).
%% It is strange, E and EP meet in the negative conditions only.


%CONSTRUCT{?enr :is-in-conflict-with [a :true,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Permitted]}
%   WHERE{?enr a :false,:hold; rdf:subject ?en; rdf:predicate rdf:type; rdf:object :Permitted.
%         ?en a ?c. ?e a :Permitted, ?c. ?c a :Eventuality.
%         NOT EXISTS{?tr a :ThematicRole. ?en ?tr ?vn. NOT EXISTS{?e ?tr ?vp}}
%         NOT EXISTS{?tr a :ThematicRole. ?en ?tr ?vn. ?e ?tr ?vp. FILTER(?vn!=?vp)}
%         NOT EXISTS{?enr :is-in-conflict-with ?te. ?te rdf:type :true,:hold;
%                    rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Permitted}}
is_in_conflict_with(ENR, meta(E, type, permitted, [true, hold])) :-
  ct_types(ENR, [false, hold]), meta_triple(ENR, EN, type, permitted),
  ct_type(EN, C), ct_Eventuality(C), ct_type(E, permitted),
  \+((ct_ThematicRole(TR1), ct_triple(EN, TR1, _), \+(ct_triple(E, TR1, _)))),
  \+((ct_ThematicRole(TR2), ct_triple(EN, TR2, VN), ct_triple(E, TR2, VP), VN \= VP)).
%% It is strange, E and EN meet in the negative conditions only.


%CONSTRUCT{?rep :is-necessarily-violated-by ?ren}
%   WHERE{?trn rdf:type :ThematicRole. ?en ?trn ?vn. ?ren rdf:type :necessary,:hold; rdf:subject ?en; rdf:predicate ?trn; rdf:object ?vn.
%         ?rep rdf:type :false,:hold; rdf:subject ?ep; rdf:predicate rdf:type; rdf:object :Permitted.
%         ?en rdf:type ?c. ?ep rdf:type ?c. ?c rdf:type :Eventuality.
%         NOT EXISTS{?tr rdf:type :ThematicRole. ?en ?tr ?vn. NOT EXISTS{?ep ?tr ?vp}}
%         NOT EXISTS{?tr rdf:type :ThematicRole. ?en ?tr ?vn. ?ep ?tr ?vp. FILTER(?vn!=?vp)}}
is_necessarily_violated_by(REP, REN) :-
  ct_ThematicRole(TRN), ct_triple(EN, TRN, VN),
  meta_triple(REN, EN, TRN, VN), ct_types(REN, [necessary, hold]),
  meta_triple(REP, EP, type, permitted), ct_types(REP, [false, hold]),
  ct_Eventuality(C), ct_type(EP, C),
  \+((ct_ThematicRole(TR1), ct_triple(EN, TR1, _), \+(ct_triple(EP, TR1, _)))),
  \+((ct_ThematicRole(TR2), ct_triple(EN, TR2, VN), ct_triple(EP, TR2, VP), VN \= VP)).


%CONSTRUCT{[a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object ?ddm]}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e} 
%        {?e rdf:type :Obligatory. BIND(:Permitted AS ?ddm)}UNION{?e rdf:type :Permitted. BIND(:Obligatory AS ?ddm)}
%        NOT EXISTS{?f a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object ?ddm}}
mod_not(NE, DDM) :-
  ct_not([E, NE]),
  ct_type(E, M), member((M, DDM), [(obligatory, permitted), (permitted, obligatory)]).

ct_triple(mod_not(NE, DDM), type, false) :- mod_not(NE, DDM).
ct_triple(mod_not(NE, DDM), type, hold) :- mod_not(NE, DDM).
meta_triple(mod_not(NE, DDM), NE, type, DDM) :- mod_not(NE, DDM).


%CONSTRUCT{?ne rdf:type ?ddm}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e} 
%         ?r a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type.
%         {?r rdf:object :Obligatory. BIND(:Permitted AS ?ddm)}UNION
%          {?r rdf:object :Permitted. BIND(:Obligatory AS ?ddm)}}
ct_triple(NE, type, DDM) :-
  ct_not([E, NE]),
  meta_triple(R, E, type, M), ct_types(R, [false, hold]),
  member((M, DDM), [(obligatory, permitted), (permitted, obligatory)]).


%CONSTRUCT{?ne rdf:type ?ddm}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e} 
%        ?r a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type.
%        {?r rdf:object :Obligatory. BIND(:Permitted AS ?ddm)}UNION
%        {?r rdf:object :Permitted. BIND(:Obligatory AS ?ddm)}}
% XXX It is same as prevision!


%CONSTRUCT{[a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Obligatory]}
%  WHERE{?e rdf:type :Optional. {?e :not ?ne}UNION{?ne :not ?e}
%        NOT EXISTS{?f rdf:type :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Obligatory}} 
optional_obligatory(NE) :-
   ct_type(E, optional), ct_not([E, NE]),
   \+(meta_types(NE, type, obligatory, [false, hold])).


%CONSTRUCT{?e rdf:type :Optional. ?ne rdf:type :Optional}
%   WHERE{{?e :not ?ne}UNION{?ne :not ?e}
%         ?r1 a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Obligatory.
%         ?r2 a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Obligatory.}
ct_type(E, type, optional) :-
  ct_not([E, NE]),
  meta_types(E, type, obligatory, [false, hold]),
  meta_types(NE, type, obligatory, [false, hold]).


%CONSTRUCT{[a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Optional]}
%   WHERE{?e a :Obligatory. {?e :not ?ne}UNION{?ne :not ?e}
%         NOT EXISTS{?f a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Optional}}
%CONSTRUCT{[a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Optional]}
%   WHERE{?e a :Obligatory. {?e :not ?ne}UNION{?ne :not ?e}
%         NOT EXISTS{?f a :false,:hold; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Optional}}
not_optional(RE) :-
   member(RE, [E, NE]),
   ct_not([E, NE]),
   ct_type(E, obligatory),
   \+(meta_types(RE, type, optional, [false, hold])).
meta_triple(not_optional(E), E, type, optional) :- not_optional(E).  
ct_triple(not_optional(E), type, FH) :- member(FH, [false, hold]), not_optional(E).


%CONSTRUCT{[rdf:type :true; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Obligatory]
%                 :disjunction
%                  [rdf:type :true; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Obligatory]}
%  WHERE{{?e :not ?ne}UNION{?ne :not ?e} 
%        ?r rdf:type :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Optional.
%        NOT EXISTS{?e rdf:type :Obligatory} NOT EXISTS{?ne rdf:type :Obligatory}
%        NOT EXISTS{{?r1 :disjunction ?r2}UNION{?r2 :disjunction ?r1}
%                   ?r1 rdf:type :true; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Obligatory. 
%                   ?r2 rdf:type :true; rdf:subject ?ne; rdf:predicate rdf:type; rdf:object :Obligatory.}}
mk_disjunction_not(E, NE) :-
  ct_not([E, NE]),
  meta_types(E, type, optional, [false, hold]).
meta_triple(disjunction_arg(E, obligatory), E, type, obligatory) :- mk_disjunction_not(E, _).
meta_triple(disjunction_arg(NE, obligatory), NE, type, obligatory) :- mk_disjunction_not(_, NE).
ct_disjunction(disjunction_arg(E, obligatory), disjunction_arg(NE, obligatory)) :-
  mk_disjunction_not(E, NE).



%CONSTRUCT{?e a :Permitted} WHERE{?e a :Obligatory}
ct_triple(E, type, permitted) :- ct_triple(E, type, obligatory).


%CONSTRUCT{[a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Obligatory]} 
%  WHERE{?r a :false, :hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Permitted.
%        NOT EXISTS{?f a :false,:hold; rdf:subject ?e; rdf:predicate rdf:type; rdf:object :Obligatory}}
not_obligatory(E) :- meta_types(E, type, permitted, [false, hold]).
meta_triple(not_obligatory(E), E, type, obligatory) :- not_obligatory(E).
ct_triple(not_obligatory(E), type, FH) :- member(FH, [false, hold]), not_obligatory(E).
