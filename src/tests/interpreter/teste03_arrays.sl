func main(): void {
   
    let numeros: int[] = new int[5]; 
    
    // Preenchendo o array manualmente
    numeros[0] = 10;
    numeros[1] = 20;
    numeros[2] = 30;
    numeros[3] = 40;
    numeros[4] = 50;

    let i: int = 0;
    let total: int = 0;

    // Iteração com While
    while (i < 5) {
        // Acesso a array e soma
        let valor = numeros[i];
        print(valor);
        
        total = total + valor;
        i = i + 1;
    }

    print("Total:");
    print(total); // Deve imprimir 150
}