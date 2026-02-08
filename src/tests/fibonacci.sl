// Função recursiva clássica
func fib(n: int): int {
    if (n <= 1) {
        return n;
    }
    // Chamada recursiva dupla
    return fib(n - 1) + fib(n - 2);
}

func main(): void {
    let n int = 8;
    let resultado int = 0;
    
    print("Calculando Fibonacci de:");
    print(n);
    
    resultado = fib(n);
    
    print("Resultado:");
    print(resultado);
}