func caixa<U>(conteudo: U): U {
    return conteudo;
}

func main(): void {
    print("--- Teste: Inferencia (TyAuto) ---");

    // Note que não escrevemos "let x: int". 
    // Escrevemos apenas "let x".
    // O compilador precisa:
    // 1. Resolver caixa(55) -> descobre que U é int -> retorno é int.
    // 2. Atribuir o tipo int à variável x (TyAuto -> TyInt).
    
    let x = caixa(55);
    
    print("X foi inferido como Int:");
    print(x);

    let y = caixa("Texto Inferido");
    print(y);
    
    // Teste de consistência: Se x é int, posso somar?
    let z = x + 10;
    print(z);
}