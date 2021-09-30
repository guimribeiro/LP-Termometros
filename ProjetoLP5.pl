/*-------------------------------------------------------------*/
membro(X, [X | _]).
membro(X, [_| R]) :- membro(X, R).

remove_repetidos([], []).
remove_repetidos([P | R], L) :-
    membro(P, R),
	!,
	remove_repetidos(R, L).
remove_repetidos([P | R], [P | L]) :- remove_repetidos(R, L).

concatena(Lista1,Lista2,Total):-
    append(Lista1,Lista2,Total1),
    remove_repetidos(Total1,Total).

retira_lista(_,[],[]).
retira_lista([(L,C)|Lista1],[(L,C)|Lista2],Final):-
    retira_lista(Lista1,Lista2,Final).

retira_lista(Lista1,[(L,C1)|Lista2],[(L,C1)|Final]):-
    retira_lista(Lista1,Lista2,Final).

combinacao(0, _, []).
combinacao(N, L, [E | C_L_E]) :-
	N > 0,
	append(_, [E | L_apos_E], L),
	N_1 is N - 1,
	combinacao(N_1, L_apos_E, C_L_E).
/*------------------------------------------------------------*/

encontra([P|_],P,[P]).
encontra([P|R1],X,[P|Res]):-encontra(R1,X,Res).
propaga([P|_],Pos,Posicoes):-propaga_aux(P,Pos,Pos_desor),sort(Pos_desor,Posicoes).
propaga_aux([P|_],Pos,Posicoes):-encontra(P,Pos,Posicoes).
propaga_aux([_|R],Pos,Posicoes):-propaga_aux(R,Pos,Posicoes).
/*------------------------------------------------------------*/
nao_altera_aux(P,_,[P|_]).
nao_altera_aux(Posicao,_,[_|R]):-
    nao_altera_aux(Posicao,_,R).
nao_altera_linhas_anteriores([],_,_).
nao_altera_linhas_anteriores([(Linha,Coluna)|R],L,Ja_Preenchidas):-
    Linha<L,
    !,
    nao_altera_aux((Linha,Coluna),L,Ja_Preenchidas),
	nao_altera_linhas_anteriores(R,L,Ja_Preenchidas).
nao_altera_linhas_anteriores([_|Posicoes],L,Ja_Preenchidas):-
	nao_altera_linhas_anteriores(Posicoes,L,Ja_Preenchidas).
/*------------------------------------------------------------*/
conta(P,[],[],_,Seletor):-
    Seletor=:=0,
    !,
    P>=0.

conta(P,[],[],_,Seletor):-
    Seletor=:=1,
    !,
    P=:=0.

conta(P,[(_,X)|R],Ja_Preen2,X,Seletor):-
    P>=0,
    !,
    P2 is P-1,
    conta(P2,R,Ja_Preen2,X,Seletor).

conta(P,[(A,B)|R],[(A,B)|Ja_Preen2],X,Seletor):-
    P>=0,
    X=\=B,
    !,
    conta(P,R,Ja_Preen2,X,Seletor).

verifica_parcial_aux([P|R],Ja_Preenchidas,Coluna,Dim,Seletor):-
	Coluna=<Dim,
    Coluna2 is Coluna+1,
    conta(P,Ja_Preenchidas,Ja_Preen2,Coluna,Seletor),
    verifica_parcial_aux(R,Ja_Preen2,Coluna2,Dim,Seletor).

verifica_parcial_aux(_,_,Coluna,Dim,_):-
    Coluna>Dim.

verifica_parcial([_,_|[Colunas]],Ja_Preenchidas,Dim,Poss):-
    concatena(Ja_Preenchidas,Poss,Lista),
	verifica_parcial_aux(Colunas,Lista,1,Dim,0).
/*-----------------------------------------------------------*/
conta_l(P,[],_):-P>=0.

conta_l(P,[(X,_)|R],X):-
    P>=0,
    !,
    P2 is P-1,
    conta_l(P2,R,X).

conta_l(P,[(B,_)|R],X):-
    P>=0,
    X=\=B,
    !,
    conta_l(P,R,X).
/*-------------------------------------------------------------*/
encontra_l([],_,[]).
encontra_l([(L,C)|R],L,[(L,C)|Lista]):-
    encontra_l(R,L,Lista).
encontra_l([_|R],L,Lista):-
    encontra_l(R,L,Lista).

ciclo_propaga(_,[],[]).
ciclo_propaga(Puz,[P|Lista],Posicoes):-
    propaga(Puz,P,Posicoes1),
    ciclo_propaga(Puz,Lista,Posicoes2),
    append(Posicoes1,Posicoes2,Posicoes).

posicoes_consequentes(Puz,Ja_Preenchidas,L,Total,Num,Posicoes,Lista):-
    encontra_l(Ja_Preenchidas,L,Posicoes),
    !,
    conta_l(Total,Posicoes1,L),
    length(Posicoes1,Num),
    sort(Posicoes1,Posicoes),
    ciclo_propaga(Puz,Posicoes1,Lista).
/*------------------------------------------------------------*/
/*findall(Comb, combinacao(2, [a, b ,c], Comb), L_combs).*/

avalia_posicoes(_,_,[],_,_,[]).
avalia_posicoes(Puz,Dim,[(A,B)|Lista_Pos],Total,Ja_Preenchidas,[Prop|Lista_Prop]):-
    propaga(Puz,(A,B),Prop),
    nao_altera_linhas_anteriores(Prop,A,Ja_Preenchidas),
    conta_l(Total,Prop,A),
    verifica_parcial(Puz,Ja_Preenchidas,Dim,Prop),
    avalia_posicoes(Puz,Dim,Lista_Pos,Total,Ja_Preenchidas,Lista_Prop).

avalia_posicoes(Puz,Dim,[_|Lista_Pos],Total,Ja_Preenchidas,Lista_Prop):-
    avalia_posicoes(Puz,Dim,Lista_Pos,Total,Ja_Preenchidas,Lista_Prop).

junta_combs_aux([],[]).
junta_combs_aux([Pos|Comb_L],Comb):-
    junta_combs_aux(Comb_L,Comb1),
    concatena(Pos,Comb1,Comb).

junta_combs([],[]).
junta_combs([Comb_L|L_Combs],[Comb|Lista]):-
    junta_combs_aux(Comb_L,Comb),
    junta_combs(L_Combs,Lista).

junta_ja_preenchidas(_,[],[]).
junta_ja_preenchidas(Lista,[Comb|Lista_Combs],[Nova_Comb|Lista_Final]):-
    concatena(Lista,Comb,Nova_Comb),
    junta_ja_preenchidas(Lista,Lista_Combs,Lista_Final).

avalia_linha_combs(_,_,[],[]).
avalia_linha_combs(Total,L,[Comb|Lista_Combs],[Comb1|Lista_Final]):-
    conta_l(Total,Comb,L),
    sort(Comb,Comb1),
    avalia_linha_combs(Total,L,Lista_Combs,Lista_Final).

avalia_linha_combs(Total,L,[_|Lista_Combs],Lista_Final):-
	avalia_linha_combs(Total,L,Lista_Combs,Lista_Final).

possibilidades_linha([Term,Linhas|[Colunas]], [(_,_)|_],_, Ja_Preenchidas,Possibilidades_L):-
    length(Colunas,Dim),
    \+(verifica_parcial([Term,Linhas|[Colunas]],Ja_Preenchidas,Dim,[])),
    Possibilidades_L=[].

possibilidades_linha([Term,Linhas|[Colunas]], [(L,C)|Posicoes_linha], Total, Ja_Preenchidas,Possibilidades_L):-
    !,
    posicoes_consequentes([Term,Linhas|[Colunas]],Ja_Preenchidas,L,Total,Num,Posicoes,Lista),
    retira_lista(Posicoes,[(L,C)|Posicoes_linha],Posicoes_Lista),
    length(Colunas,Dim),
    avalia_posicoes([Term,Linhas|[Colunas]],Dim,Posicoes_Lista,Total,Ja_Preenchidas,Lista_Prop),
    !,
    Num_Comb is Total-Num,
    findall(Comb, combinacao(Num_Comb, Lista_Prop, Comb), L_Combs),
	junta_combs(L_Combs,Lista_Combs),
    junta_ja_preenchidas(Lista,Lista_Combs,Lista_Pre_Final),
    avalia_linha_combs(Total,L,Lista_Pre_Final,Lista_Final),
	sort(Lista_Final,Possibilidades_L).

cria_posicoes(L,Dim,Lista):-
    cria_posicoes_aux(L,1,Dim,Lista).
cria_posicoes_aux(L,Dim,Dim,[(L,Dim)]).
cria_posicoes_aux(L,C,Dim,[(L,C)|Lista]):-
    C1 is C+1,
    cria_posicoes_aux(L,C1,Dim,Lista).

verifica_colunas(Dim,Ja_Preenchidas,Colunas):-
    verifica_parcial_aux(Colunas,Ja_Preenchidas,1,Dim,1).

resolve([Term,Linhas|[Colunas]],Solucao):-
    length(Linhas,Dim),
    resolve_aux([Term,Linhas|[Colunas]],Dim,1,[],Solucao1),
    !,
    verifica_colunas(Dim,Solucao1,Colunas),
    sort(Solucao1,Solucao).

iterador([Term,[_|Totais]|Colunas],Dim,L,[Hipotese|_],Ja_Preenchidas,Solucoes):-
    concatena(Hipotese,Ja_Preenchidas,Ja_Preenchidas1),
    resolve_aux([Term,Totais|Colunas],Dim,L,Ja_Preenchidas1,Solucoes).

iterador(Puz,Dim,L,[_|Possibilidades_L],Ja_Preenchidas,Solucoes):-
    iterador(Puz,Dim,L,Possibilidades_L,Ja_Preenchidas,Solucoes).

resolve_aux([_,[]|[_]],_,_,Ja_Preenchidas,Ja_Preenchidas).
resolve_aux([Term,[Total|R]|Colunas],Dim,L,Ja_Preenchidas,Solucao):-
    cria_posicoes(L,Dim,Lista_Pos),
    !,
    possibilidades_linha([Term,[Total|R]|Colunas],Lista_Pos,Total,Ja_Preenchidas,Possibilidades_L),
    Possibilidades_L\=[],
    L1 is L+1,
    iterador([Term,[Total|R]|Colunas],Dim,L1,Possibilidades_L,Ja_Preenchidas,Solucao).

 