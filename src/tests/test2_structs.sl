// test2_structs.sl

struct Person {
    name string;
    age: int;      // Agora suportado (com dois pontos)
    height float;  // Agora suportado (sem dois pontos)
}

func main(): void {
    // Agora suportado pela nova regra de Stmt que adicionamos
    // Equivalente interno a: let people Person[] = new Person[3];
    let people Person [3]; 
    
    // Instanciação (Sintaxe de chamada de função padrão)
    people[0] = Person("Alice", 25, 1.65);
    people[1] = Person("Bob", 30, 1.80);
    people[2] = Person("Charlie", 35, 1.75);

    let i int = 0;
    
    while (i < 3) {
        // Acesso a campos
        print(people[i].name);
        print(people[i].height);
        
        // Incremento manual
        i = i + 1; 
    }
}