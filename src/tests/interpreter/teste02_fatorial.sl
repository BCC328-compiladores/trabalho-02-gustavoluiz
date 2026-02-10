func factorial(n: int): int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main(): void {
    let resultado: int = factorial(5);
    print(resultado); // Deve imprimir 120
    
    // Teste extra com condicional
    if (resultado == 120) {
        print("Calculo correto");
    }
}