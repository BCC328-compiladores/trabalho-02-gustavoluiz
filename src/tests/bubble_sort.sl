func main(): void {
    // 1. Criar e preencher o array desordenado
    let size int = 5;
    let arr int[] = new int[size];
    
    arr[0] = 64;
    arr[1] = 34;
    arr[2] = 25;
    arr[3] = 12;
    arr[4] = 22;

    print("--- Array Original ---");
    let k int = 0;
    while (k < size) {
        print(arr[k]);
        k = k + 1;
    }

    // 2. Algoritmo Bubble Sort
    let i int = 0;
    while (i < size - 1) {
        let j int = 0;
        while (j < size - i - 1) {
            // Se o elemento atual for maior que o proximo, troca
            if (arr[j] > arr[j + 1]) {
                let temp int = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
            j = j + 1;
        }
        i = i + 1;
    }

    // 3. Imprimir array ordenado
    print("--- Array Ordenado ---");
    k = 0;
    while (k < size) {
        print(arr[k]);
        k = k + 1;
    }
}