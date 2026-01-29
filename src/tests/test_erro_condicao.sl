func main(): int {
    let x: int = 1;

    // Erro: x é int, mas IF exige bool (diferente de C/C++, SL é estrita)
    if (x) { 
        print("Isso nao deve compilar");
    }

    // Erro: while com string
    while ("texto") {
        print("Loop infinito errado");
    }

    return 0;
}