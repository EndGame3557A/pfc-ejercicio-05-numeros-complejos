package taller

/** Punto 2. Una fecha del calendario gregoriano: día, mes y año. El
  * constructor rechaza las fechas que no existen, así que todo objeto Fecha
  * es una fecha real.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Fecha(val dia: Int, val mes: Int, val anio: Int) {

  // Precondiciones del constructor: el mes va de 1 a 12 y el día existe en
  // ese mes de ese año. Una fecha que no cumple lanza
  // IllegalArgumentException.
  // Completar

  require(mes >= 1 && mes <= 12 && dia >= 1 && dia <= diasDelMes(mes), "Fecha no válida")

  // Si el año es bisiesto en el calendario gregoriano.
  private def esBisiesto: Boolean = if (anio%4 == 0 && (anio%100 != 0 || anio%400 == 0)) true else false

  // Cuántos días tiene el mes m de este año.
  private def diasDelMes(m: Int): Int = {
    m match {
      case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
      case 4 | 6 | 9 | 11 => 30
      case 2 if esBisiesto => 29
      case 2 => 28
    }
  }

  // Qué número de día es esta fecha dentro de su año: el 1 de enero es 1.
  def diaDelAnio: Int =
    (1 to mes).map(diasDelMes).sum + dia



  // La fecha del día siguiente.
  def siguiente: Fecha = new Fecha(dia+1, mes, anio)

  // La fecha n días después de esta, con n mayor o igual que 0.
  def masDias(n: Int): Fecha =
    new Fecha(dia + n, mes, anio) // Completar

  // Si esta fecha es anterior a otra.
  def antesDe(otra: Fecha): Boolean =
    if (diaDelAnio < otra.diaDelAnio) true else false

  // La forma "29/2/2024": día, mes y año separados por barras.
  override def toString: String = dia + "/" + mes + "/" + anio // Completar
}
