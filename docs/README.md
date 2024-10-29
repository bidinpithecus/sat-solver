# SAT Solver

Este projeto implementa um resolvedor de SAT (Satisfiability Problem) que utiliza o algoritmo DPLL (Davis-Putnam-Logemann-Loveland) para resolver fórmulas booleanas na forma normal conjuntiva (CNF).

O programa lê arquivos `.cnf` e determina se a fórmula é satisfatível (SAT) ou insatisfatível (UNSAT), além de fornecer um modelo de atribuições de variáveis quando a fórmula é SAT.

## Pré-requisitos

Antes de compilar e executar o programa, certifique-se de que seu ambiente possui:

- GHC (Glasgow Haskell Compiler)
- `make` instalado

## Execução

Você pode executar o resolvedor de SAT de duas formas:

1. Executar em um único arquivo `.cnf`:

   Forneça o caminho para um arquivo `.cnf` como argumento ao script de execução:
   ```bash
   ./run.sh <caminho-do-arquivo-cnf>
   ```
   Por exemplo:
   ```bash
   ./run.sh input/par8-1-c.cnf
   ```
2. Executar em todos os arquivos `.cnf` do diretório `input`:

   Caso nenhum argumento seja fornecido, o script irá buscar pelos arquivos `.cnf` no diretório `input` e executar o resolvedor em cada um deles:
   ```bash
   ./run.sh
   ```

## Saída

Para cada arquivo `.cnf` processado, o programa gera um arquivo `.res` no diretório `output` com o mesmo nome do arquivo de entrada. O conteúdo do arquivo de saída será:

- SAT: Se a fórmula é satisfatível, seguido do modelo (as atribuições de variáveis).
- UNSAT: Se a fórmula é insatisfatível.

Exemplo de saída para um arquivo satisfatível:
```res
SAT
1 -2 3 0
```
