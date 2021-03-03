:- use_module(library(plunit)).

doencas('Cefaleia', [
            'Sensibilidade à luz',
            'Sensibilidade aos sons',
            'Sesibilidade aos cheiros',
            'Irritabilidade',
            'Náuseas',
            'Vômitos',
            'Queda da pálpebra',
            'Sensação de latejamento',
            'Dor intensa ao movimentar o corpo'
]).

doencas('Acidente vascular encefálico', [
            'Dor de cabeça',
            'Falta de força em um lado do corpo',
            'Fala embolada',
            'Perda da semsibilidade de uma parte do corpo',
            'Dificuldade em permanecer em pé',
            'Sonolência',
            'Alteração da visão',
            'Dificuldade para levantar o braço',
            'Dificuldade para segurar objetos',
            'Movimentos incomuns e descontrolados',
            'Perda de memória',
            'Confusão mental',
            'Náuseas',
            'Vômitos'
]).

doencas('Dorsalgia', [
            'Dor nas costas acompanhadas de cólica',
            'Dor na parte superior da coluna',
            'Dor de cabeça',
            'Dificuldades para respirar',
            'Formigamento',
            'Calafrios',
            'Falta de ar',
            'Febre',
            'Fraqueza',
            'Rigidez',
            'Dificuldades na locomoção'
]).

doencas('Dor precordial', [
            'Dor no peito'
]).

doencas('Insuficiência cardíaca', [
            'Falta de ar',
            'Inchaço dos pés',
            'Inchaço das pernas',
            'Falta de energia',
            'Cansaço',
            'Dificuldade para dormir',
            'Abdômen inchado',
            'Perda de apetite',
            'Tosse com catarro',
            'Aumentao da micção',
            'Confusão',
            'Perda de memória'
]).

doencas('Hipertensão arterial', [
            'Dor de cabeça',
            'Tontura',
            'Cansaço',
            'Zumbido no ouvido',
            'Sangramento pelo nariz',
            'Falta de ar',
            'Visao borrada'
]).

doencas('Arritmia', [
            'Palpitações no coração',
            'Queda de pressão',
            'Fadiga',
            'Falta de ar',
            'Desmaios',
            'Náuseas',
            'Vômitos',
            'Tontura'
]).

doencas('Gripe', [
            'Febre',
            'Dor no corpo',
            'Dor de cabeça',
            'Nariz entupido',
            'Dor de garganta',
            'Cansaço extremo'
]).


doencas('Malária', [
            'Dor de cabeça',
            'Febre',            'Dores nas articulações',
            'Vômitos',
            'Convulsões'
        ]).


doencas('Febre amarela', [
            'Febre',
            'Dores musculares',
            'Dor de cabeça',
            'Perda de apetite',
            'Náuseas',
            'Vômitos',
            'Fotofobia',
            'Fraqueza'
        ]).

spaces(0):- !.

spaces(N):- write(' '), N1 is N-1, spaces(N1).

printa_paciente([]) :- nl.

printa_paciente([H|T]) :- write("Paciente: "), spaces(1), write(H), nl, printa_paciente(T).

printa_sintomas([]) :- nl.

printa_sintomas([H|T]) :-
    write(H), member(_, T) -> write(','), spaces(1), printa_sintomas(T);
    spaces(1).

:- begin_tests(contar_sintomas).

test(contar1) :- contar_sintomas(['Febre', 'Nausea'], ['Febre'], 1).
test(contar2) :- contar_sintomas(['Febre', 'Nausea'], ['Febre', 'Nausea'], 2).
test(contar3, Contador == 2) :- contar_sintomas(['Febre', 'Nausea'], ['Febre', 'Nausea', 'Vomito'], Contador).

:- end_tests(contar_sintomas).

contar_sintomas([], _, Contador) :- Contador is 0.

contar_sintomas([Sintoma| Sintomas], SintomasDoenca, Contador) :-
    contar_sintomas(Sintomas, SintomasDoenca, N),
    (
        member(Sintoma, SintomasDoenca) -> Contador is N + 1;
        Contador is N
    ).

printa_doencas([]).

printa_doencas([Quantidade | Resto]) :-
    printa_doencas(Resto),
    forall(quantidade_sintomas_paciente(Doenca, _, Quantidade),
           (
               Quantidade == 0, !;
               write(Doenca),
               nl
           )
          ).

printa_sintomas_doencas([]).

printa_sintomas_doencas([Quantidade | Resto]) :-
    printa_sintomas_doencas(Resto),
    forall(quantidade_sintomas_paciente(Doenca, Sintomas, Quantidade),
           (
               Quantidade == 0, !;
               write(Doenca),
               write(":"),
               nl,
               printa_sintomas(Sintomas),
               nl,
               nl,
               nl
           )
          ).

mostra_diagnostico :-
    retractall(quantidade_sintomas_paciente(_, _, _)),
    write('Entre com o nome do paciente que deseja mostrar o diagnóstico: '),
    read(Nome),
    findall(X, paciente_possui_sintoma(Nome, X), Resultado),
    forall(
        doencas(Doenca, SintomasDoenca),
        (
            contar_sintomas(Resultado, SintomasDoenca, Contador),
            assert(quantidade_sintomas_paciente(Doenca, SintomasDoenca, Contador))
        )
    ),
    findall(X, quantidade_sintomas_paciente(_, _, X), Contadores),
    sort(Contadores, ContadoresOrdenados),
    write("A seguir serão mostradas as possíveis doenças que o paciente possa ter, sendo listadas em ordem decrescente, ou seja, a primeira mostrada é a de maior probabilidade\n\n"),
    printa_doencas(ContadoresOrdenados),
    write("\nO resultado do protótipo é apenas informativo, o paciente deve consultar um médico para obter um diagnóstico correto e preciso.\n"),
    write("Para mais informações da(s) possível(is) doenças digite '1.'"),
    read(Info),
    Info == 1 ->
    write("A seguir serão mostradas todos os sintomas das doenças que o paciente possa ter.\n\n\n"),
    printa_sintomas_doencas(ContadoresOrdenados).



le_um_sintoma(Paciente) :-
    write('Digite 0 para sair\nEntre com um sintoma: '),
    read(Sintoma),
    Sintoma \= 0 ->
    assert(paciente_possui_sintoma(Paciente, Sintoma)),
    le_um_sintoma(Paciente);
    true.

ler_sintomas_paciente :-
    write('Entre com o nome do paciente: '),
    read(Nome),
    paciente(Nome) -> le_um_sintoma(Nome);
    write('O paciente: não foi encontrado!').

novo_paciente :-
    write('Entre com o nome do paciente'),
    read(Nome),
    \+ paciente(Nome) ->
    assert(paciente(Nome)),
    write("Paciente cadastrado!");
    write('Paciente já cadastrado!').

le_arquivo(Stream,[]) :-
    at_end_of_stream(Stream), !.

le_arquivo(Stream,[X|L]) :-
    !,
    read(Stream,X),
    le_arquivo(Stream,L).

consulta_paciente_arquivo :-
    write('Entre com o nome do paciente que deseja consultar: '),
    read(Nome),
    open('c:/users/otavio/documents/prolog/pacientes.txt', read, Stream),
    le_arquivo(Stream, PacientesComEOF),
    select('end_of_file', PacientesComEOF, Pacientes),
    close(Stream),
    member(Nome, Pacientes) ->
    write("Paciente encontrado!");
    write("Paciente não encontrado!").


:- begin_tests(troca_elemento).

test(troca1) :- troca_elemento(1, 2, [1,2,3], [2,2,3]).
test(troca2) :- troca_elemento(1, 3, [1,2,3], [3,2,3]).
test(troca3, Res == ['oi', 'adeus', 'bem vindo']) :- troca_elemento('tchau', 'adeus', ['oi','tchau','bem vindo'], Res).

:- end_tests(troca_elemento).

troca_elemento(_, _, [], []) :- !.

troca_elemento(O, R, [O|T], [R|T2]) :- troca_elemento(O, R, T, T2), !.

troca_elemento(O, R, [H|T], [H|T2]) :- H \= O, troca_elemento(O, R, T, T2).

escreve_todos_pacientes_arquivo(_, []).

escreve_todos_pacientes_arquivo(Stream,[Paciente | Pacientes]) :-
    string_concat(Paciente, '.\n', NovoPaciente),
    write(Stream, NovoPaciente),
    escreve_todos_pacientes_arquivo(Stream, Pacientes).


deleta_paciente_arquivo :-
    write('Entre com o nome do paciente que deseja excluir: '),
    read(Nome),

    open('c:/users/otavio/documents/prolog/pacientes.txt', read, StreamArquivo),
    le_arquivo(StreamArquivo, PacientesComEOF),
    select('end_of_file', PacientesComEOF, Pacientes),
    close(StreamArquivo),

    member(Nome, Pacientes) ->
    delete(Pacientes, Nome, NovaListaPacientes),
    open('c:/users/otavio/documents/prolog/pacientes.txt', write, StreamArquivo2),
    escreve_todos_pacientes_arquivo(StreamArquivo2, NovaListaPacientes),
    close(StreamArquivo2),
    write("Paciente deletado com sucesso!");
    write("Paciente não encontrado!").

altera_paciente_arquivo :-
    write('Entre com o nome do paciente que deseja alterar: '),
    read(Nome),

    open('c:/users/otavio/documents/prolog/pacientes.txt', read, StreamArquivo),
    le_arquivo(StreamArquivo, PacientesComEOF),
    select('end_of_file', PacientesComEOF, Pacientes),
    close(StreamArquivo),

    member(Nome, Pacientes) ->

    open('c:/users/otavio/documents/prolog/pacientes.txt', write, StreamArquivo2),
    write('Entre com o nome que deseja substituir o paciente selecionado: '),
    read(NovoNome),
    troca_elemento(Nome, NovoNome, Pacientes, NovaListaPacientes),

    escreve_todos_pacientes_arquivo(StreamArquivo2, NovaListaPacientes),
    write("Nome do paciente alterado com sucesso!"),
    close(StreamArquivo2);
    write("Paciente não encontrado!").

salvar_paciente_arquivo :-
    write('Entre com o nome do paciente que deseja salvar: '),
    read(Nome),
    paciente(Nome) ->
    open('c:/users/otavio/documents/prolog/pacientes.txt', append, StreamArquivo),
    current_output(Stream),
    set_output(StreamArquivo),
    write(Nome),
    write('.'),
    nl(StreamArquivo),
    close(StreamArquivo),
    set_output(Stream),
    write("Paciente salvo");
    write('Paciente não encontrado!').


mostra_todos_pacientes:-
    findall(X, paciente(X), Resultado),
    printa_paciente(Resultado).

mostra_sintomas:-
    write("Entre com o nome do paciente: "),
    read(Nome),
    paciente(Nome) -> findall(X, paciente_possui_sintoma(Nome, X), Resultado),
    write("Sintomas: "),
    printa_sintomas(Resultado);
    write('Paciente não encontrado!').

remove_sintoma_paciente :-
    write("Entre com o nome do paciente que deseja remover o sintoma: '"),
    read(Nome),
    paciente(Nome) -> findall(X, paciente_possui_sintoma(Nome, X), Resultado),
    write("Sintomas: "),
    printa_sintomas(Resultado),
    write("\nEntre com o sintoma que deseja remover: "),
    read(Sintoma),
    paciente_possui_sintoma(Nome, Sintoma),
    retract(paciente_possui_sintoma(Nome, Sintoma)),
    write("Sintoma removido!").

remove_paciente :-
    write("Entre com o nome do paciente que deseja remover: "),
    read(Nome),
    paciente(Nome) -> retract(paciente(Nome)),
    retractall(paciente_possui_sitoma(Nome, _)),
    write("Paciente deletado com sucesso!");
    write("Paciente não encontrado!").


menu :- write('\n\n\n--- Menu (Insira o número e o "." para selecionar a opção) --- \n\n'),
        write('1. - Inserir novo paciente\n'),
        write('2. - Adicionar Sintomas a um paciente\n'),
        write('3. - Mostrar sintomas de um paciente\n'),
        write('4. - Ver Diagnostico de um paciente\n'),
        write('5. - Mostrar pacientes cadastrados\n'),
        write('6. - Remover sintomas de um paciente\n'),
        write('7. - Remover um paciente\n'),
        write('8. - Salvar um paciente no arquivo txt\n'),
        write('9. - Ler um paciente do arquivo txt\n'),
        write('10. - Deleta um paciente do arquivo txt\n'),
        write('11. - Altera um paciente do arquivo txt\n'),
        write('30. - Sair\n'),
        read(Opcao),
        opcao_escolhida(Opcao).

opcao_escolhida(Opcao) :-
                     Opcao == 1, novo_paciente, menu;
                     Opcao == 2, ler_sintomas_paciente, menu;
                     Opcao == 3, mostra_sintomas, menu;
                     Opcao == 4, mostra_diagnostico, menu;
                     Opcao == 5, mostra_todos_pacientes, menu;
                     Opcao == 6, remove_sintoma_paciente, menu;
                     Opcao == 7, remove_paciente, menu;
                     Opcao == 8, salvar_paciente_arquivo, menu;
                     Opcao == 9, consulta_paciente_arquivo, menu;
                     Opcao == 10, deleta_paciente_arquivo, menu;
                     Opcao == 11, altera_paciente_arquivo, menu;
                     Opcao == 30, true.

:-dynamic paciente/1.
paciente().

:-dynamic paciente_possui_sintoma/1.
paciente_possui_sintoma(paciente).

:-dynamic quantidade_sintomas_paciente/1.
quantidade_sintomas_paciente().









































