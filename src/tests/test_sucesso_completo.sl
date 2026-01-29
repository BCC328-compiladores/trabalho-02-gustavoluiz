struct Person {
    name: string;
    age: int;
    height: float;
}

func factorial(n: int): int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main(): int {
    // Teste de Structs
    let p: Person = new Person; // Supondo construtor ou instanciação assim
    
    // Teste de Declaração e Atribuição
    let x: int = 10;
    let y: int = factorial(5);
    
    // Teste de Array
    let numeros: int[] = new int[5];
    numeros[0] = x;
    
    // Teste de Loop e Booleanos
    let i: int = 0;
    while (i < 5) {
        if (numeros[i] > 0 && true) {
            print(numeros[i]);
        }
        i = i + 1;
    }

    return 0;
}