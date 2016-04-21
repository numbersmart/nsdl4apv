#' Crear Tabla de Mortalidad
#'
#' A partir de edad, defunciones y raíz, crea una tabla de mortalidad hipotética.
#' @param edad Vector de edades
#' @param defs Vector de defunciones, en mismo orden que edad
#' @param raiz Raíz de análisis (número de personas iniciales)
#' @keywords metodo
#' @encoding UTF-8
#' @seealso m_Metodos
#' @author Eduardo Flores
#' @examples
#' tabla <- m_CrearTblMortalidad(c(10,20,50), c(10,35, 90), 100)
#' @export
m_CrearTblMortalidad <- function(edad, defs, raiz){

  # columna de tasa básica de muerte
  totd <- sum(defs)
  pd <- defs/totd

  # columna lx
  lx <- cumsum(pd)
  lx <- raiz-(lx*raiz)
  lxv <- as.vector(lx[,1])
  # Tasa mortalidad
  lg <- lag(lxv)[2:length(lxv)]

  qx <- cbind.data.frame(lxv, c(raiz,lg), c(raiz,lg)-lxv)
  qx$l <- qx[,3]/qx[,2]
  qx <- qx$l

  # defunciones
  dx <- c(raiz, lag(lxv)[2:length(lxv)])-lxv

  # tabla
  tbl <- data.frame("x" = edad,
                    "pd" = pd,
                    "dx" = dx,
                    "lx" = lx,
                    "qx" = qx)
  names(tbl) <- c("x", "pd", "dx", "lx", "qx")
  return(tbl)
}
#' Agregar Factor de Mortalidad a Tabla de Mortalidad
#'
#' Toma una tasa y distribuye de acuerdo a metodo a diferentes edades y generos
#' @param tbl Tabla de Mortalidad, de acuerdo con el método en m_CrearTblMortalidad
#' @param tasa Tasa de muertes por cada 100 mil
#' @param raiz Raíz de análisis (número de personas iniciales)
#' @param edad_baja Edad (excluyente) mínima para afectar con mortalidad
#' @param edad_alta Edad (excluyente) máxima para afectar con mortalidad
#' @param genero_split Objeto de dos números, en los que se divide la mortalidad (Hombre, Mujer)
#' @param metodo Metodo de división de mortalidad (Lineal, Normal, o pesos por cuartil).
#' @param redondeo Redondeo de mortalidad para Arriba o Abajo (default)
#' @details Para dividir por métodos de cuártil, en el parámetro se específica un objeto númerico que sume 100 entre todos. Tal
#' como c(10,40,50). En este caso, el total de las edades se dividirá en 3 (el largo del objeto) tomando el peso de 10%, 40% y 50%, cada tercio de rangos
#' de edad de acuerdo a mayor a menor. Si edad_baja = 10 y edad_alta = 40, de 10 a 20 se agregará el 10% de la mortalidad, de 20 a 30 el 40% y de 30 a 40 el 50%.
#' @keywords metodo
#' @encoding UTF-8
#' @seealso m_Metodos
#' @author Eduardo Flores
#' @examples
#' tabla_nueva <- m_AddMortalidad(tabla_origen, 0.01, 100000, 12, 65, c(40,60), c(10,10,80), "Abajo")
#' @export
m_AddMortalidad <- function(tbl,
                            tasa = 0.1,
                            raiz,
                            edad_baja,
                            edad_alta,
                            genero_split = c(50,50),
                            metodo = "Lineal",
                            redondeo = "Abajo" ){
  # ------------------------
  # Validaciones
  if(sum(genero_split)==100){}else{stop("La suma de ambos generos debe ser igual a 100")}
  if(edad_alta>=edad_baja){}else{stop("La edad baja debe ser menor a la alta")}

  # Personas que se va a sumar a la mortalidad
  p <- raiz*tasa

  # ----------------------------
  # 1. Distribución por Genéro
  h <- subset(tbl, sexo == "Hombres")
  m <- subset(tbl, sexo == "Mujeres")

  # personas para hombres
  h_m <- p*(genero_split[1]/100)
  # personas para mujeres
  m_m <- p*(genero_split[2]/100)

  # ----------------------------
  # 2. Distribución por Edad
  # Cortar ambos data sets por edad...
  h_2 <- subset(h, x > edad_baja & x < edad_alta)
  h_c <- subset(h, x < edad_baja | x >= edad_alta)
  m_2 <- subset(m, x >= edad_baja & x < edad_alta)
  m_c <- subset(m, x <= edad_baja | x >= edad_alta)

  # Distribuir a las muertes por edad que queda
  n <- length(unique(h_2$x))
  if(metodo == "Lineal"){
    e_h <- h_m/n
    e_m <- m_m/n

    h_2$NuevasMuertes <- e_h
    m_2$NuevasMuertes <- e_m

  }else{
    if(metodo == "Normal"){
      e_h <- h_m/n
      e_h <- rnorm(n, e_h, 0.5)

      e_m <- m_m/n
      e_m <- rnorm(n, e_m, 0.5)

      h_2$NuevasMuertes <- e_h
      m_2$NuevasMuertes <- e_m

    }else{
      # por cuartiles
      nc <- length(metodo)

      # Hombres...
      h_2 <- arrange(h_2, x)
      h_2$levels <- cut(x = h_2$x, nc)
      catalogo <- data.frame("levels" = unique(h_2$levels),
                             "peso" = metodo)
      h_2 <- h_2 %>% left_join(., catalogo)
      h_2$NuevasMuertes <- h_2$peso/sum(h_2$peso)*h_m
      # Mujeres...
      m_2 <- arrange(m_2, x)
      m_2$levels <- cut(x = m_2$x, nc)
      catalogo <- data.frame("levels" = unique(m_2$levels),
                             "peso" = metodo)
      m_2 <- m_2 %>% left_join(., catalogo)
      m_2$NuevasMuertes <- m_2$peso/sum(m_2$peso)*m_m

      m_2 <- subset(m_2, select = -c(levels, peso))
      h_2 <- subset(h_2, select = -c(levels, peso))

    }

  }
  # Ya distribui, ahora...
  # Redondeos...
  if(redondeo == "Abajo"){
    h_2$NuevasMuertes <- floor(h_2$NuevasMuertes)
    m_2$NuevasMuertes <- floor(m_2$NuevasMuertes)
  }else{
    h_2$NuevasMuertes <- ceiling(h_2$NuevasMuertes)
    m_2$NuevasMuertes <- ceiling(m_2$NuevasMuertes)
  }

  # Unir...
  tbl_m2 <- rbind.data.frame(h_2, m_2)
  tbl_mc <- rbind.data.frame(h_c, m_c)
  tbl_mc$NuevasMuertes <- 0

  tbl_new <- rbind.data.frame(tbl_mc, tbl_m2)
  tbl_new$defs <- tbl_new$pd*raiz+tbl_new$NuevasMuertes

  efecto <- sum(tbl_new$defs)-sum(tbl_new$dx)

  # Generar nueva tabla de mortalidad...
  tbl_new <- arrange_(tbl_new, c("x"))
  tbl_new <- arrange_(tbl_new, c("sexo"))

  tbl_new_h <- subset(tbl_new, sexo == "Hombres")
  tbl_new_m <- subset(tbl_new, sexo == "Mujeres")

  f_h <- m_CrearTblMortalidad(tbl_new_h['x'], tbl_new_h['defs'], raiz)
  f_h$sexo <- "Hombres"
  f_m <- m_CrearTblMortalidad(tbl_new_m['x'], tbl_new_m['defs'], raiz)
  f_m$sexo <- "Mujeres"

  f <- rbind.data.frame(f_h, f_m)
  return(f)
}
