package interfaz
case class Anuncio(id: String, gastoUsd: Double, conversaciones: Int, ventas: Int, ingresoBs: Double)
case class Metricas(costoPorConversacion: Double, tasaCierre: Double, roas: Double, utilidadUsd: Double)
case class Configuracion(minConversaciones: Int, roasEscalar: Double, cierreEscalar: Double, tipoCambio: Double)

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

case class Producto(nombre: String, precioBs: Double, estado: EstadoProducto, anuncios: List[Anuncio])

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
      case InteresBajo => "Interes bajo: cada conversacion sale cara"
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

  def etiquetaRecomendacion(r: Recomendacion): String =
    r match
      case Escalar => "Escalar"
      case Observar => "Observar"
      case Pausar => "Pausar"
      case EsperarDatos => "Esperar datos"

  def claseRecomendacion(r: Recomendacion): String =
    r match
      case Escalar => "escalar"
      case Observar => "observar"
      case Pausar => "pausar"
      case EsperarDatos => "esperar"

  def claseEstado(e: EstadoProducto): String =
    e match
      case Candidato => "candidato"
      case EnUso => "enuso"
      case Revisar => "revisar"
      case Descartado => "descartado"

  def unir(textos: List[String]): String =
    textos.foldLeft("")((acum, t) => acum + t + "\n")

  def tarjeta(titulo: String, valor: String): String =
    s"""<div class="card"><span>$titulo</span><b>$valor</b></div>"""

  def claseSigno(x: Double): String =
    if x < 0 then "neg" else "pos"

  def filaAnuncio(a: Anuncio, c: Configuracion): String = {
    val m: Metricas = calcularMetricas(a, c)
    val r: Recomendacion = recomendar(a, m, c)
    val d: Diagnostico = diagnosticar(m, c)
    s"""<tr><td><b>${a.id}</b></td><td>${redondear(a.gastoUsd)}</td><td>${a.conversaciones}</td><td>${a.ventas}</td><td>${redondear(m.costoPorConversacion)}</td><td>${redondear(m.tasaCierre * 100)}%</td><td>${redondear(m.roas)}</td><td class="${claseSigno(m.utilidadUsd)}">${redondear(m.utilidadUsd)}</td><td><span class="badge ${claseRecomendacion(r)}">${etiquetaRecomendacion(r)}</span></td><td>${describirDiagnostico(d)}</td></tr>"""
  }

  def filaProducto(p: Producto, c: Configuracion): String = {
    val utilidad: Double = utilidadGlobal(p.anuncios, c)
    s"""<tr><td><b>${p.nombre}</b></td><td><span class="badge ${claseEstado(p.estado)}">${describirEstado(p.estado)}</span></td><td>${p.precioBs} Bs</td><td>${p.anuncios.length}</td><td>${redondear(gastoTotal(p.anuncios))}</td><td>${redondear(roasGlobal(p.anuncios, c))}</td><td class="${claseSigno(utilidad)}">${redondear(utilidad)}</td></tr>"""
  }

  def listaRanking(id: String, visible: Boolean, anuncios: List[Anuncio], criterio: Anuncio => Double, mostrar: Anuncio => String): String = {
    val estilo: String = if visible then "block" else "none"
    val items: String = unir(ordenarPor(anuncios, criterio).map(a => s"""<li><b>${a.id}</b><span>${mostrar(a)}</span></li>"""))
    s"""<ol id="rank-$id" style="display:$estilo">$items</ol>"""
  }

  def generarHtml(productos: List[Producto], c: Configuracion): String = {
    val anuncios: List[Anuncio] = todosLosAnuncios(productos)
    val m: Anuncio => Metricas = a => calcularMetricas(a, c)
    val porRoas: Anuncio => Double = a => m(a).roas
    val porCierre: Anuncio => Double = a => m(a).tasaCierre
    val porCosto: Anuncio => Double = a => -m(a).costoPorConversacion
    val utilidad: Double = utilidadGlobal(anuncios, c)

    val tarjetas: String = unir(List(
      tarjeta("Gasto total", s"${redondear(gastoTotal(anuncios))} USD"),
      tarjeta("Conversaciones", s"${conversacionesTotal(anuncios)}"),
      tarjeta("Ventas", s"${ventasTotal(anuncios)}"),
      tarjeta("Ingreso", s"${ingresoTotalBs(anuncios)} Bs"),
      tarjeta("ROAS global", s"${redondear(roasGlobal(anuncios, c))}"),
      tarjeta("Utilidad", s"${redondear(utilidad)} USD")
    ))
    val filas: String = unir(anuncios.map(a => filaAnuncio(a, c)))
    val filasProductos: String = unir(productos.map(p => filaProducto(p, c)))
    val rankings: String =
      listaRanking("roas", true, anuncios, porRoas, a => s"ROAS ${redondear(m(a).roas)}") +
      listaRanking("cierre", false, anuncios, porCierre, a => s"${redondear(m(a).tasaCierre * 100)}% de cierre") +
      listaRanking("costo", false, anuncios, porCosto, a => s"${redondear(m(a).costoPorConversacion)} USD por conversaci&oacute;n")

    s"""<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Panel de rendimiento de anuncios</title>
<style>
:root { --bg:#f4f6fb; --panel:#ffffff; --texto:#1c2333; --suave:#6b7488; --linea:#e3e7f0; --acento:#4f46e5; --verde:#0f9d58; --rojo:#d93025; --ambar:#e8a200; --gris:#8a93a6; }
@media (prefers-color-scheme: dark) { :root { --bg:#0f1320; --panel:#181d2e; --texto:#e8ecf7; --suave:#98a2bd; --linea:#2a3149; --acento:#8b85ff; } }
* { box-sizing:border-box; }
body { margin:0; background:var(--bg); color:var(--texto); font-family:system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; line-height:1.45; }
header { padding:32px 24px 8px; max-width:1100px; margin:0 auto; }
header h1 { margin:0; font-size:28px; letter-spacing:-0.5px; }
header p { margin:6px 0 0; color:var(--suave); }
main { max-width:1100px; margin:0 auto; padding:16px 24px 48px; }
h2 { font-size:15px; text-transform:uppercase; letter-spacing:1px; color:var(--suave); margin:32px 0 12px; }
.cards { display:grid; grid-template-columns:repeat(auto-fit, minmax(150px, 1fr)); gap:12px; }
.card { background:var(--panel); border:1px solid var(--linea); border-radius:14px; padding:16px; }
.card span { display:block; color:var(--suave); font-size:13px; }
.card b { display:block; font-size:24px; margin-top:4px; }
.tabla { background:var(--panel); border:1px solid var(--linea); border-radius:14px; overflow-x:auto; }
table { border-collapse:collapse; width:100%; font-size:14px; }
th, td { padding:12px 14px; text-align:left; border-bottom:1px solid var(--linea); white-space:nowrap; }
th { color:var(--suave); font-weight:600; font-size:12px; text-transform:uppercase; letter-spacing:0.5px; }
tr:last-child td { border-bottom:none; }
td:last-child { white-space:normal; }
.badge { display:inline-block; padding:4px 10px; border-radius:999px; font-size:12px; font-weight:700; color:#fff; }
.escalar { background:var(--verde); }
.pausar { background:var(--rojo); }
.observar { background:var(--ambar); }
.esperar, .candidato, .descartado { background:var(--gris); }
.enuso { background:var(--acento); }
.revisar { background:var(--ambar); }
.pos { color:var(--verde); font-weight:700; }
.neg { color:var(--rojo); font-weight:700; }
.ranking { background:var(--panel); border:1px solid var(--linea); border-radius:14px; padding:16px 20px; }
select { font:inherit; padding:8px 12px; border-radius:10px; border:1px solid var(--linea); background:var(--bg); color:var(--texto); margin-bottom:12px; }
ol { margin:0; padding-left:24px; }
li { padding:8px 0; border-bottom:1px solid var(--linea); }
li:last-child { border-bottom:none; }
li span { margin-left:12px; color:var(--suave); }
footer { text-align:center; color:var(--suave); font-size:13px; padding:0 24px 32px; }
</style>
</head>
<body>
<header>
<h1>Panel de rendimiento de anuncios</h1>
<p>Tipo de cambio: ${c.tipoCambio} Bs por USD &middot; M&iacute;nimo de conversaciones: ${c.minConversaciones} &middot; ROAS para escalar: ${c.roasEscalar} &middot; Cierre para escalar: ${redondear(c.cierreEscalar * 100)}%</p>
</header>
<main>
<h2>Totales</h2>
<div class="cards">
$tarjetas
</div>
<h2>Anuncios</h2>
<div class="tabla">
<table>
<tr><th>Anuncio</th><th>Gasto USD</th><th>Conv.</th><th>Ventas</th><th>Costo/conv. USD</th><th>Cierre</th><th>ROAS</th><th>Utilidad USD</th><th>Recomendaci&oacute;n</th><th>Diagn&oacute;stico</th></tr>
$filas
</table>
</div>
<h2>Ranking de anuncios</h2>
<div class="ranking">
<select id="criterio" onchange="cambiar()">
<option value="roas">Ordenar por ROAS</option>
<option value="cierre">Ordenar por tasa de cierre</option>
<option value="costo">Ordenar por costo por conversaci&oacute;n</option>
</select>
$rankings
</div>
<h2>Productos</h2>
<div class="tabla">
<table>
<tr><th>Producto</th><th>Estado</th><th>Precio</th><th>Anuncios</th><th>Gasto USD</th><th>ROAS</th><th>Utilidad USD</th></tr>
$filasProductos
</table>
</div>
</main>
<footer>Generado por el programa en Scala</footer>
<script>
function cambiar() {
  var elegido = document.getElementById('criterio').value;
  ['roas', 'cierre', 'costo'].forEach(function (k) {
    document.getElementById('rank-' + k).style.display = (k === elegido) ? 'block' : 'none';
  });
}
</script>
</body>
</html>"""
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

    println("=== ANUNCIOS ===")
    imprimirLista(anuncios, a => mostrarAnuncio(a, config))

    println("=== TOTALES ===")
    println(s"Gasto: ${gastoTotal(anuncios)} USD")
    println(s"Conversaciones: ${conversacionesTotal(anuncios)}")
    println(s"Ventas: ${ventasTotal(anuncios)}")
    println(s"Ingreso: ${ingresoTotalBs(anuncios)} Bs")
    println(s"ROAS global: ${redondear(roasGlobal(anuncios, config))}")
    println(s"Utilidad: ${redondear(utilidadGlobal(anuncios, config))} USD")

    println("=== MEJOR Y PEOR ===")
    val tasa: Anuncio => Double = a => calcularMetricas(a, config).tasaCierre
    val roas: Anuncio => Double = a => calcularMetricas(a, config).roas
    val costo: Anuncio => Double = a => -calcularMetricas(a, config).costoPorConversacion
    println(s"Mejor tasa de cierre: ${mejorPor(anuncios, tasa).id}")
    println(s"Peor tasa de cierre: ${peorPor(anuncios, tasa).id}")
    println(s"Mejor ROAS: ${mejorPor(anuncios, roas).id}")
    println(s"Peor ROAS: ${peorPor(anuncios, roas).id}")

    println("=== RANKINGS ===")
    println(s"Por ROAS: ${ordenarPor(anuncios, roas).map(a => a.id)}")
    println(s"Por tasa de cierre: ${ordenarPor(anuncios, tasa).map(a => a.id)}")
    println(s"Por costo por conversacion (menor primero): ${ordenarPor(anuncios, costo).map(a => a.id)}")

    println("=== FILTRAR ===")
    println(s"Para escalar: ${filtrarPorRecomendacion(anuncios, config, Escalar).map(a => a.id)}")
    println(s"Para pausar: ${filtrarPorRecomendacion(anuncios, config, Pausar).map(a => a.id)}")

    println("=== PRODUCTOS POR ESTADO ===")
    println(s"En uso: ${filtrarPorEstado(productos, EnUso).map(p => p.nombre)}")
    println(s"Candidatos: ${filtrarPorEstado(productos, Candidato).map(p => p.nombre)}")

    println("=== PRODUCTOS ===")
    imprimirLista(productos, p => resumenProducto(p, config))

    println("=== CAMBIOS (copy) ===")
    val plantillasEnUso: Producto = cambiarEstado(plantillas, EnUso)
    println(s"Antes: ${describirEstado(plantillas.estado)} | Despues: ${describirEstado(plantillasEnUso.estado)}")
    val v4: Anuncio = Anuncio("V4", 6.0, 30, 3, 60.0)
    val cursoNuevo: Producto = registrarAnuncio(curso, v4)
    println(s"Anuncios antes: ${curso.anuncios.length} | despues: ${cursoNuevo.anuncios.length}")

    println("=== MEJOR ANUNCIO DE TEXTOS (generico) ===")
    println(mejorPor(List("sc", "scala", "fp"), t => t.length * 1.0))

    println("=== INTERFAZ ===")
    java.nio.file.Files.writeString(java.nio.file.Path.of("panel.html"), generarHtml(productos, config))
    println("Panel generado: panel.html")
  }
}