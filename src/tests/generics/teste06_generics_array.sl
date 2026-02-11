// Função que recebe um array de T e retorna um elemento T
func pegarElemento<T>(lista: T[], i: int): T {
    return lista[i];
}

func main(): void {
    print("--- Teste: Arrays Genericos ---");

    // 1. Criar Array de Inteiros
    let numeros: int[] = new int[3];
    numeros[0] = 100;
    numeros[1] = 200;
    numeros[2] = 300;

    // Passamos 'numeros' (int[]). 
    // O TypeChecker deve casar T[] com int[] e deduzir que T = int.
    let val = pegarElemento(numeros, 1); 
    
    print("Valor recuperado (esperado 200):");
    print(val);

    // 2. Criar Array de Strings (se seu parser suportar array de string)
    // Se não, pode remover esta parte, mas a lógica de tipos é a mesma.
    /*
    let textos: string[] = new string[2];
    textos[0] = "Ola";
    textos[1] = "Mundo";
    let s = pegarElemento(textos, 0);
    print(s);
    */
}