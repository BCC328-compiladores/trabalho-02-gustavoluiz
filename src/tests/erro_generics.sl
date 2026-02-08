func mesmoTipo<T>(a: T, b: T): void {
    return;
}

func main(): void {
    // ERRO: O primeiro arg diz que T é int. O segundo diz que T é bool.
    // A função solveGenerics deve detectar o conflito.
    mesmoTipo(10, true); 
}