
case class Anuncio(
id: String, 
gastoUsd: Double, 
conversaciones: Int, 
ventas: Int, 
ingresoBs: Double)
case class Metricas(
costoPorConversacion: Double, 
tasaCierre: Double, 
roas: Double, 
utilidadUsd: Double)
case class Configuracion(
minConversaciones: Int, 
roasEscalar: Double, 
cierreEscalar: Double, 
tipoCambio: Double)

sealed trait EstadoProducto
case object Candidato extends EstadoProducto
case object EnUso extends EstadoProducto
case object Revisar extends EstadoProducto
case object Descartado extends EstadoProducto

sealed trait Recomendacion
case object Escalar extends Recomendacion
case object Observar extends Recomendacion
case object Pausar extends Recomendacion
case object EsperarDatos extends Recomendacion

sealed trait Diagnostico
case object TodoBien extends Diagnostico
case object InteresBajo extends Diagnostico
case object CierreBajo extends Diagnostico

case class Producto(
  nombre: String, 
  precioBs: Double, 
  estado: EstadoProducto, 
  anuncios: List[Anuncio])

object Proyecto {

  def redondear(x: Double): Double = Math.round(x * 100) / 100.0

  def calcularMetricas(a: Anuncio, c: Configuracion): Metricas = {
    val ingresoUsd: Double = a.ingresoBs / c.tipoCambio
    val costo: Double = if a.conversaciones == 0 then 0.0 else a.gastoUsd / a.conversaciones
    val cierre: Double = if a.conversaciones == 0 then 0.0 else a.ventas * 1.0 / a.conversaciones
    val roas: Double = if a.gastoUsd == 0 then 0.0 else ingresoUsd / a.gastoUsd
    Metricas(costo, cierre, roas, ingresoUsd - a.gastoUsd)
  }

  def recomendar(a: Anuncio, m: Metricas, c: Configuracion): Recomendacion =
    a match
      case Anuncio(_, _, conv, _, _) if conv < c.minConversaciones => EsperarDatos
      case _ if m.roas < 1.0 => Pausar
      case _ if m.roas >= c.roasEscalar && m.tasaCierre >= c.cierreEscalar => Escalar
      case _ => Observar

  def diagnosticar(m: Metricas, c: Configuracion): Diagnostico =
    m match
      case Metricas(_, cierre, _, _) if cierre < c.cierreEscalar => CierreBajo
      case Metricas(_, _, roas, _) if roas < c.roasEscalar => InteresBajo
      case _ => TodoBien

  def describirRecomendacion(r: Recomendacion): String =
    r match
      case Escalar => "Escalar: subir presupuesto"
      case Observar => "Observar: seguir mirando"
      case Pausar => "Pausar: apagar el anuncio"
      case EsperarDatos => "Esperar datos: aun son pocas conversaciones"

  def describirDiagnostico(d: Diagnostico): String =
    d match
      case TodoBien => "Todo bien"
      case InteresBajo => "Interes bajo: cada conversacion es mas cara"
      case CierreBajo => "Cierre bajo: hablan pero no compran"

  def describirEstado(e: EstadoProducto): String =
    e match
      case Candidato => "Candidato"
      case EnUso => "En uso"
      case Revisar => "Revisar"
      case Descartado => "Descartado"

  def gastoTotal(anuncios: List[Anuncio]): Double =
    anuncios.foldLeft(0.0)((acum, a) => acum + a.gastoUsd)

  def conversacionesTotal(anuncios: List[Anuncio]): Int =
    anuncios.foldLeft(0)((acum, a) => acum + a.conversaciones)

  def ventasTotal(anuncios: List[Anuncio]): Int =
    anuncios.foldLeft(0)((acum, a) => acum + a.ventas)

  def ingresoTotalBs(anuncios: List[Anuncio]): Double =
    anuncios.foldLeft(0.0)((acum, a) => acum + a.ingresoBs)

  def roasGlobal(anuncios: List[Anuncio], c: Configuracion): Double =
    if gastoTotal(anuncios) == 0 then 0.0
    else (ingresoTotalBs(anuncios) / c.tipoCambio) / gastoTotal(anuncios)

  def utilidadGlobal(anuncios: List[Anuncio], c: Configuracion): Double =
    ingresoTotalBs(anuncios) / c.tipoCambio - gastoTotal(anuncios)

  def mejorPor[A](lista: List[A], criterio: A => Double): A =
    lista.reduce((a, b) => if criterio(a) >= criterio(b) then a else b)

  def peorPor[A](lista: List[A], criterio: A => Double): A =
    lista.reduce((a, b) => if criterio(a) <= criterio(b) then a else b)

  def insertar[A](elemento: A, ordenada: List[A], criterio: A => Double): List[A] =
    ordenada match
      case Nil => List(elemento)
      case cabeza :: cola =>
        if criterio(elemento) >= criterio(cabeza) then elemento :: ordenada
        else cabeza :: insertar(elemento, cola, criterio)

  def ordenarPor[A](lista: List[A], criterio: A => Double): List[A] =
    lista match
      case Nil => Nil
      case cabeza :: cola => insertar(cabeza, ordenarPor(cola, criterio), criterio)

  def imprimirLista[A](lista: List[A], texto: A => String): Unit =
    lista match
      case Nil => print("")
      case cabeza :: cola => {
        println(texto(cabeza))
        imprimirLista(cola, texto)
      }

  def filtrarPorRecomendacion(anuncios: List[Anuncio], c: Configuracion, r: Recomendacion): List[Anuncio] =
    anuncios.filter(a => recomendar(a, calcularMetricas(a, c), c) == r)

  def filtrarPorEstado(productos: List[Producto], estado: EstadoProducto): List[Producto] =
    productos.filter(p => p.estado == estado)

  def cambiarEstado(p: Producto, nuevo: EstadoProducto): Producto =
    p.copy(estado = nuevo)

  def registrarAnuncio(p: Producto, nuevo: Anuncio): Producto =
    p.copy(anuncios = nuevo :: p.anuncios)

  def todosLosAnuncios(productos: List[Producto]): List[Anuncio] =
    productos.flatMap(p => p.anuncios)

  def resumenProducto(p: Producto, c: Configuracion): String = {
    val gasto: Double = gastoTotal(p.anuncios)
    val roas: Double = roasGlobal(p.anuncios, c)
    val utilidad: Double = utilidadGlobal(p.anuncios, c)
    s"${p.nombre} (${describirEstado(p.estado)}): gasto ${redondear(gasto)} USD, ROAS ${redondear(roas)}, utilidad ${redondear(utilidad)} USD"
  }

  def mostrarAnuncio(a: Anuncio, c: Configuracion): String = {
    val m: Metricas = calcularMetricas(a, c)
    s"${a.id}: costo/conv ${redondear(m.costoPorConversacion)} USD, cierre ${redondear(m.tasaCierre * 100)}%, ROAS ${redondear(m.roas)}, utilidad ${redondear(m.utilidadUsd)} USD\n    ${describirRecomendacion(recomendar(a, m, c))}\n    ${describirDiagnostico(diagnosticar(m, c))}"
  }

  def main(args: Array[String]): Unit = {
    val config: Configuracion = Configuracion(20, 2.0, 0.15, 10.0)

    val v1: Anuncio = Anuncio("V1", 10.0, 40, 8, 200.0)
    val v2: Anuncio = Anuncio("V2", 8.0, 20, 2, 50.0)
    val v3: Anuncio = Anuncio("V3", 5.0, 25, 6, 150.0)

    val curso: Producto = Producto("Curso Excel", 49.0, EnUso, List(v1, v2))
    val plantillas: Producto = Producto("Plantillas", 29.0, Candidato, List(v3))
    val productos: List[Producto] = List(curso, plantillas)
    val anuncios: List[Anuncio] = todosLosAnuncios(productos)

    println("ANUNCIOS")
    imprimirLista(anuncios, a => mostrarAnuncio(a, config))

    println("TOTALES")
    println(s"Gasto: ${gastoTotal(anuncios)} USD")
    println(s"Conversaciones: ${conversacionesTotal(anuncios)}")
    println(s"Ventas: ${ventasTotal(anuncios)}")
    println(s"Ingreso: ${ingresoTotalBs(anuncios)} Bs")
    println(s"ROAS global: ${redondear(roasGlobal(anuncios, config))}")
    println(s"Utilidad: ${redondear(utilidadGlobal(anuncios, config))} USD")

    println("MEJOR Y PEOR")
    val tasa: Anuncio => Double = a => calcularMetricas(a, config).tasaCierre
    val roas: Anuncio => Double = a => calcularMetricas(a, config).roas
    val costo: Anuncio => Double = a => -calcularMetricas(a, config).costoPorConversacion
    println(s"Mejor tasa de cierre: ${mejorPor(anuncios, tasa).id}")
    println(s"Peor tasa de cierre: ${peorPor(anuncios, tasa).id}")
    println(s"Mejor ROAS: ${mejorPor(anuncios, roas).id}")
    println(s"Peor ROAS: ${peorPor(anuncios, roas).id}")

    println("RANKINGS")
    println(s"Por ROAS: ${ordenarPor(anuncios, roas).map(a => a.id)}")
    println(s"Por tasa de cierre: ${ordenarPor(anuncios, tasa).map(a => a.id)}")
    println(s"Por costo por conversacion (menor primero): ${ordenarPor(anuncios, costo).map(a => a.id)}")

    println("FILTRAR")
    println(s"Para escalar: ${filtrarPorRecomendacion(anuncios, config, Escalar).map(a => a.id)}")
    println(s"Para pausar: ${filtrarPorRecomendacion(anuncios, config, Pausar).map(a => a.id)}")

    println("PRODUCTOS POR ESTADO")
    println(s"En uso: ${filtrarPorEstado(productos, EnUso).map(p => p.nombre)}")
    println(s"Candidatos: ${filtrarPorEstado(productos, Candidato).map(p => p.nombre)}")

    println("PRODUCTOS")
    imprimirLista(productos, p => resumenProducto(p, config))

    println("CAMBIOS (copy)")
    val plantillasEnUso: Producto = cambiarEstado(plantillas, EnUso)
    println(s"Antes: ${describirEstado(plantillas.estado)} | Despues: ${describirEstado(plantillasEnUso.estado)}")
    val v4: Anuncio = Anuncio("V4", 6.0, 30, 3, 60.0)
    val cursoNuevo: Producto = registrarAnuncio(curso, v4)
    println(s"Anuncios antes: ${curso.anuncios.length} | despues: ${cursoNuevo.anuncios.length}")

    println("MEJOR ANUNCIO DE TEXTOS (generico)")
    println(mejorPor(List("sc", "scala", "fp"), t => t.length * 1.0))
  }
}