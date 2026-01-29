struct Ponto {
    x: int;
    y: int;
}

func main(): int {
    // Declaração de array de inteiros
    let numeros: int[] = new int[5];

    // ============================================================
    // CENÁRIO DE ERRO 1: Índice de Array inválido
    // Regra: "O índice de um array deve ser sempre int."
    // ============================================================
    
    // Aqui estamos usando um float (2.5) como índice.
    // O verificador deve acusar: "Indice do array deve ser do tipo Int"
    // numeros[2.5] = 10; 


    // ============================================================
    // CENÁRIO DE ERRO 2: Campo de Struct inexistente
    // Regra: "Acesso a campos deve verificar se existe na definição."
    // ============================================================
    
    let p: Ponto; 
    
    // A struct Ponto só tem 'x' e 'y'. O campo 'z' não existe.
    // O verificador deve acusar: "Campo 'z' nao existe na struct 'Ponto'"
    p.z = 30;

    return 0;
}