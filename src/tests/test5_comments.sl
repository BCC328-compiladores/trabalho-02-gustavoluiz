/* -------------------------------------------------
   TESTE DE BLOCO SIMPLES
   Este um comentrio que ignora quebras de linha.
   Nada aqui deve virar token.
   -------------------------------------------------
*/

func main(): void {
    let visible int = 1;

    /* -------------------------------------------------
       TESTE DE ANINHAMENTO (NESTED COMMENTS)
       O Alex deve contar a profundidade:
       Nvel 1...

       /* Nvel 2: Comentrio dentro de comentrio!
          Se a lgica estiver errada, o lexer vai parar 
          no prximo fechar-comenrio.
          let hidden int = 999; // Isso deve ser ignorado
       */

       
       Voltamos para o Nvel 1.
       O lexer s deve sair do modo comentrio agora.
       -------------------------------------------------
    */
    print(visible);
}