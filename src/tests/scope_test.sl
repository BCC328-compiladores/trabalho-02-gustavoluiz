func main(): void {
    let x int = 10;
    let y int = 50;

    print("Inicio (Global x):");
    print(x); // Esperado: 10

    if (x == 10) {
        // Shadowing: Declarando um novo x dentro do escopo do IF
        let x int = 20; 
        print("Dentro do IF (Local x):");
        print(x); // Esperado: 20
        
        // Modificando y (que eh do escopo pai)
        y = 99;
    }

    print("Fim (Global x restaurado?):");
    print(x); // Esperado: 10 (se suportar shadowing) ou 20 (se for escopo único)
              // O ideal em linguagens modernas é 10.

    print("Valor de y modificado:");
    print(y); // Esperado: 99
}