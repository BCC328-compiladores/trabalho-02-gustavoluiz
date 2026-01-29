// test4_generics.sl
// Exemplo de função genérica (map)
forall a b func map(f: (a) -> b, v a[]) : b[] {
    let result b[] = new b[v.size];
    
    for (i = 0; i < v.size; i++) {
        result[i] = f(v[i]);
    }
    
    return result;
}

func isAdult(age int): bool {
    return age >= 18;
}

func main(): void {
    let ages int[3] = [15, 20, 30];
    let checks bool[] = map(isAdult, ages);
    
    if (checks[0] == true) {
        print("Erro: 15 nao e adulto");
    } else {
        print("Correto");
    }
}