func main(): int {
    let x: int = 10;

    if (x > 5) {
        let z: int = 20;
        print(z); // Aqui ok
    }

    // Erro: 'z' só existe dentro do bloco do IF
    print(z); 

    return 0;
}