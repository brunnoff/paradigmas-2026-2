:- use_module(library(clpfd)).

% TO * TO = BOA: cada letra é um dígito (0-9) diferente, e as letras
% que começam um número não podem valer 0.
% Modificação da operação para multiplicação (quadrado): TO * TO = BOA.
% Esta versão possui uma ÚNICA solução válida: 28 * 28 = 784.
crypto([
    [T, O],
    [T, O],
    [B, O, A]
]) :-
    Vars = [T, O, B, A],
    Vars ins 0..9,
    all_different(Vars),
    T #\= 0, B #\= 0,
    
    (10 * T + O) * (10 * T + O) #= 100 * B + 10 * O + A.


:- forall((
        crypto([
            [T, O],
            [T, O],
            [B, O, A]
        ]),
        label([T, O, B, A])
    ),
    format("TO=~w~w TO=~w~w BOA=~w~w~w~n", [T, O, T, O, B, O, A])).
