func soma(a: int, b: int): int {
    // Erro: Retornando bool quando prometeu int
    return true; 
}

func main(): int {
    // Erro: Chamando com número errado de argumentos
    let x: int = soma(10); 
    
    // Erro: Chamando com tipo errado
    let y: int = soma(10, 20.5);

    return 0;
}