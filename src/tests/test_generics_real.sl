// Função genérica: T assume o tipo do argumento 'val'
func identidade<T>(val: T): T {
    return val;
}

// Função genérica com dois argumentos do mesmo tipo
func maximo<T>(a: T, b: T): T {
    // Nota: O interpretador não sabe comparar 'T' genericamente com '>', 
    // então vamos apenas retornar 'a' para testar a checagem de tipos.
    return a; 
}

func main(): void {
    print("--- Teste de Generics (Polimorfismo) ---");

    // CASO 1: T vira INT
    let num = 10;
    let resNum = identidade(num); // O TypeChecker deve inferir T = int
    print("Resultado Int:");
    print(resNum);

    // CASO 2: T vira BOOL
    let boolVal = true;
    let resBool = identidade(boolVal); // O TypeChecker deve inferir T = bool
    
    if (resBool) {
        print("Resultado Bool: Verdadeiro");
    }

    // CASO 3: Consistência de Tipos
    // Aqui passamos dois ints. Se passássemos (10, true), o TypeChecker deveria dar erro.
    let m = maximo(50, 20);
    print("Chamada com dois argumentos ok.");
}