:- use_module(library(clpfd)).

% n_rainhas(N, Qs):
% Qs é a lista com as colunas de cada rainha.
% Exemplo para N = 4: Qs = [1, 3, 0, 2]
n_rainhas(N, Qs) :-
    length(Qs, N),
    N1 is N - 1,
    Qs ins 0..N1,
    all_different(Qs),
    gerar_diagonais(Qs, 0, Diag1, Diag2),
    all_different(Diag1),
    all_different(Diag2),
    label(Qs).

% Cria as listas de diagonais (Coluna + Linha) e (Coluna - Linha)
gerar_diagonais([], _, [], []).
gerar_diagonais([Q|Resto], Linha, [D1|RestoD1], [D2|RestoD2]) :-
    D1 #= Q + Linha,
    D2 #= Q - Linha,
    ProxLinha is Linha + 1,
    gerar_diagonais(Resto, ProxLinha, RestoD1, RestoD2).

% Imprime o tabuleiro na tela com 'R' e '.'
write_nrainhas(Qs) :-
    length(Qs, N),
    N1 is N - 1,
    forall(member(ColunaRainha, Qs), (
        forall(between(0, N1, ColunaAtual), (
            (ColunaAtual =:= ColunaRainha -> write(' R ') ; write(' . '))
        )),
        nl
    )).
