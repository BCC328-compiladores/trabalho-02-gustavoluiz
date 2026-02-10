func main(): void {
    // Teste de declaração e inferência (let)
    let a: int = 10;
    let b = 20; // Inferência de tipo
    let soma = a + b;

    print(soma); // Deve imprimir 30

    // Teste de If-Else
    if (soma > 25) {
        print("Soma maior que 25");
    } else {
        print("Soma menor ou igual a 25");
    }

    // Teste de tipos primitivos
    let altura: float = 1.75;
    let ativo: bool = true;
    
    if (ativo) {
        print(altura);
    }
}