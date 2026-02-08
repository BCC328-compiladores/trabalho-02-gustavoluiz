func factorial(n: int): int {
    if (n <= 1) { 
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main(): void {
    let res = factorial(5);
    print(res); 
}