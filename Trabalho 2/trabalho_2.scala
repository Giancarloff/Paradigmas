// Vergleichssudoku

// Tipos para modelagem do tabuleiro
sealed trait Ordem
case object Lt extends Ordem
case object Gt extends Ordem
case object Na extends Ordem

type Sudoku = Array[Array[Int]] // Células do sudoku
type OrdemLinhas = Array[Array[Ordem]] // Matriz de ordenação de células numa linha
type OrdemColunas = Array[Array[Ordem]] // Matriz de ordenação das colunas
// Nota: Obtém-se a ordem das colunas lendo de cima para baixo
case class Vergleichssudoku(s: Sudoku, ol: OrdemLinhas, oc: OrdemColunas, h: Int, w: Int) // Tabuleiro completo
// Nota:                       Largura da subregião ----------^   ^---------- Altura da subregião

// Acesso aos dados
def getSudoku(vs: Vergleichssudoku): Sudoku = vs.s
def getOrdemLinhas(vs: Vergleichssudoku): OrdemLinhas = vs.ol
def getOrdemColunas(vs: Vergleichssudoku): OrdemColunas = vs.oc
def getAlturaSub(vs: Vergleichssudoku): Int = vs.h
def getLarguraSub(vs: Vergleichssudoku): Int = vs.w

// Elementos do tabuleiro
val tamanhoSudoku: Int = 9 // Um sudoku NxN tem tamanho N
val larguraSub: Int = 3
val alturaSub: Int = 3
val sudoku: Sudoku = Array.fill(tamanhoSudoku, tamanhoSudoku)(0)

val ordemLinhas: OrdemLinhas = Array(
  Array(Lt, Gt, Na, Lt, Lt, Na, Gt, Gt),
  Array(Lt, Lt, Na, Lt, Gt, Na, Gt, Lt),
  Array(Lt, Lt, Na, Lt, Gt, Na, Gt, Lt),
  Array(Gt, Gt, Na, Gt, Lt, Na, Lt, Gt),
  Array(Gt, Lt, Na, Gt, Lt, Na, Gt, Gt),
  Array(Lt, Gt, Na, Lt, Gt, Na, Gt, Lt),
  Array(Lt, Gt, Na, Gt, Gt, Na, Lt, Gt),
  Array(Gt, Lt, Na, Gt, Lt, Na, Lt, Lt),
  Array(Lt, Gt, Na, Lt, Lt, Na, Lt, Gt)
)

val ordemColunas: OrdemColunas = Array(
  Array(Gt, Lt, Na, Gt, Gt, Na, Gt, Lt),
  Array(Lt, Lt, Na, Lt, Lt, Na, Gt, Lt),
  Array(Lt, Lt, Na, Lt, Lt, Na, Gt, Gt),
  Array(Gt, Lt, Na, Gt, Lt, Na, Lt, Gt),
  Array(Gt, Gt, Na, Gt, Lt, Na, Gt, Gt),
  Array(Gt, Gt, Na, Gt, Gt, Na, Gt, Lt),
  Array(Lt, Gt, Na, Lt, Gt, Na, Lt, Gt),
  Array(Gt, Gt, Na, Gt, Gt, Na, Lt, Lt),
  Array(Lt, Gt, Na, Lt, Gt, Na, Lt, Gt)
)

// Montando o Vergleichssudoku
def tabuleiroVS(s: Sudoku, ol: OrdemLinhas, oc: OrdemColunas, h: Int, w: Int): Vergleichssudoku = Vergleichssudoku(s, ol, oc, h, w)

// Métodos auxiliars
def isListOrdered(ints: List[Int], ops: List[Ordem]): Boolean = (ints, ops) match {
  case (_, Nil) => true
  case (x :: y :: intTail, op :: opTail) =>
    if (x == 0 || y == 0) isListOrdered(y :: intTail, opTail)
    else op match {
      case Gt => if (x > y) isListOrdered(y :: intTail, opTail) else false
      case Lt => if (x < y) isListOrdered(y :: intTail, opTail) else false
      case Na => isListOrdered(y :: intTail, opTail)
    }
  case _ => false
}

def hasNoRepeats(ints: List[Int]): Boolean = ints match {
  case Nil => true
  case a :: p => !p.contains(a) && hasNoRepeats(p)
}

// Extrai a subregião de um elemento
def extractSubregion(vs: Vergleichssudoku, row: Int, col: Int): List[Int] = {
  val subregionSizeX = getLarguraSub(vs)
  val subregionSizeY = getAlturaSub(vs)
  val subregionRow = row / subregionSizeX
  val subregionCol = col / subregionSizeY
  val subregionStartRow = subregionRow * subregionSizeX
  val subregionStartCol = subregionCol * subregionSizeY
  for {
    i <- (subregionStartRow until subregionStartRow + subregionSizeX).toList
    j <- subregionStartCol until subregionStartCol + subregionSizeY
  } yield getSudoku(vs)(i)(j)
}

// Verificar se uma tentativa específica é válida
def tentativaValida(vs: Vergleichssudoku, row: Int, col: Int, num: Int): Boolean = {
    getSudoku(vs)(row)(col) == 0 &&
    !getSudoku(vs)(row).contains(num) &&
    !getSudoku(vs).toList.transpose.contains(num) &&
    !extractSubregion(vs, row, col).contains(num) &&
    isListOrdered(getSudoku(atualizarTabuleiro(vs, row, col, num))(row).toList, ordemLinhas(row).toList) &&
    {
        val transposed = getSudoku(atualizarTabuleiro(vs, row, col, num)).transpose
        isListOrdered(transposed(col).toList, ordemColunas(col).toList)
    }
    // isListOrdered(getSudoku(atualizarTabuleiro(vs, row, col, num)).transpose, ordemColunas(col).toList)
}

// Resolução por backtracking
def resolver(vs: Vergleichssudoku): Vergleichssudoku = {
  def tentarNumeros(vs: Vergleichssudoku, row: Int, col: Int): Vergleichssudoku = {
    if (col >= tamanhoSudoku) tentarNumeros(vs, row + 1, 0)
    else if (row >= tamanhoSudoku) vs
    else {
      val newVs = tentarN(vs, row, col, 1)
      if (newVs == vs) vs
      else tentarNumeros(newVs, row, col + 1)
    }
  }

  def tentarN(vs: Vergleichssudoku, row: Int, col: Int, num: Int): Vergleichssudoku = {
    if (num > tamanhoSudoku) vs
    else if (tentativaValida(vs, row, col, num)) {
      val newVs = atualizarTabuleiro(vs, row, col, num)
      val nextVs = tentarNumeros(newVs, row, col + 1)
      if (completo(nextVs)) nextVs
      else tentarN(vs, row, col, num + 1)
    } else tentarN(vs, row, col, num + 1)
  }

  tentarNumeros(vs, 0, 0)
}

// Verifica se o tabuileiro está completo
def completo(vs: Vergleichssudoku): Boolean = !temZero(getSudoku(vs))

def temZero(s: Sudoku): Boolean = s.flatten.toList match {
  case Nil => false
  case x :: tail => x == 0 || tail.contains(0)
}

// Gera um novo tabuleiro com o número colocado nele
def atualizarTabuleiro(vs: Vergleichssudoku, row: Int, col: Int, num: Int): Vergleichssudoku = {
  val newRow = getSudoku(vs)(row).updated(col, num)
  val newSudoku = getSudoku(vs).updated(row, newRow)
  Vergleichssudoku(newSudoku, getOrdemLinhas(vs), getOrdemColunas(vs), getAlturaSub(vs), getLarguraSub(vs))
}

// Facilitador do print
def printSudoku(vs: Vergleichssudoku): Unit = getSudoku(vs).foreach(row => println(row.mkString(" ")))

// Execução principal
def main(args: Array[String]): Unit = {
  val vs = tabuleiroVS(sudoku, ordemLinhas, ordemColunas, larguraSub, alturaSub)
  val resolvido = resolver(vs)
  printSudoku(resolvido)
}
