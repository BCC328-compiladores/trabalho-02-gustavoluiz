// A sintaxe <T> deve ser aceita pelo Parser agora.
// O TypeChecker atual ignora o T, mas valida o corpo normalmente.
func wrapper<T>(val: int): int {
    print("Dentro da funcao generica");
    return val;
}

func main(): void {
    print("--- Teste de Sintaxe Generics ---");
    
    let x = 100;
    
    // Chamada normal (ainda nao implementamos inferencia de T na chamada, 
    // entao chamamos como uma funcao comum)
    let res = wrapper(x);
    
    print(res);
}