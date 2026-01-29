// teste.sl
func main(): int {
    let x: int = 10;
    let y: bool = true;
    
    // Isso deve gerar erro, pois não pode somar int com bool
    let z: int = x + y; 
    
    return 0;
}