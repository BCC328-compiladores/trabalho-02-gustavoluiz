struct Ponto {
    x: int;
    y: int;
    nome: string;
}

func main(): void {
    // Instanciação de struct
    let p1 = new Ponto; 
    
    // Atribuição de campos
    p1.x = 10;
    p1.y = 20;
    p1.nome = "Origem";

    print(p1.nome);
    print(p1.x);
    print(p1.y);

}