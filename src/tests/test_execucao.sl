func main(): int {
    let x: int = 5;
    let y: int = 10;
    
    print("Contando ate 5:");
    
    let i: int = 0;
    while (i < 5) {
        print(i);
        i = i + 1;
    }

    if (x + y == 15) {
        print("Soma correta: 15");
    } else {
        print("Erro na soma");
    }

    return 0;
}