func main(): void {
    print("--- Teste de Inferencia ---");

    // O compilador deve inferir que x eh int
    let x = 10; 
    
    // O compilador deve inferir que y eh int
    let y = 20;

    // O compilador deve inferir que texto eh string
    let texto = "Soma:";

    // Deve permitir somar dois ints inferidos
    let soma = x + y; 

    print(texto);
    print(soma);

    // Teste com booleano inferido
    let condicao = true;
    if (condicao) {
        print("A condicao era verdadeira!");
    }
}