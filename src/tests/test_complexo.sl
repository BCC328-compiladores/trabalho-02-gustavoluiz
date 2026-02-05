struct Ponto {
    x: int;
    y: int;
}

func main(): int {
    print("--- Teste de Arrays ---");
    let arr: int[] = new int[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = arr[0] + arr[1];
    
    print(arr[0]);
    print(arr[1]);
    print("Soma (30):");
    print(arr[2]);

    print("--- Teste de Structs ---");
    let p: Ponto = new Ponto;
    p.x = 100;
    p.y = 200;
    
    print("Ponto X:");
    print(p.x);
    
    if (p.y > p.x) {
        print("Y eh maior que X");
    }

    return 0;
}