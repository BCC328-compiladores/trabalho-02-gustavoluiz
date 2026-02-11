// A função exige que 'a' e 'b' sejam do mesmo tipo T
func comparar<T>(a: T, b: T): bool {
    // A implementação não importa, o erro ocorre na chamada
    return true; 
}

func main(): void {
    print("Tentando quebrar o sistema de tipos...");

    // Isso funciona (Int e Int)
    let teste1 = comparar(10, 20);

    // ISSO DEVE FALHAR NO TYPECHECKER!
    // Argumento 1: Int (implica T = Int)
    // Argumento 2: Bool (implica T = Bool)
    // Erro esperado: "Conflito de inferencia para generic T"
    let teste2 = comparar(10, true); 
}