#Codigo parauna grafica: linea recta.

m <- 1# pendiente
b <- 2 #intersección

# funcion de la linea recta
f <- function (m , b , x){
  return( m * x + b )
 }

x <- seq ( 3 , 6, 2)# vector de 3 a 6
y <- f(m , b , x) # evaluamos


plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que  pasa por el 0 en las y

m <- 2# pendiente
b <- -3 #intersección

# funcion de la linea recta
g <- function (m , b , x){
  return( m * x + b )
}

x <- seq ( -1 , 2, 0.1)# vector de -1 a 2
y <- g(m , b , x) # evaluamos


plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que  pasa por el 0 en las y


#Grafica de una parabola
g <- function (x){    
 return (x ^2 + x + 5)  
}

x <- seq ( -3 , 3, 0.1) # vector de -3 a 3
y <- g(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que
                        #pasa por el 0 en las y

t <- function ( x){     #grafica 2
  return (-x ^2 + x + 3)  
}

x <- seq ( -2 , 2, 0.01) # vector de -2 a 2
y <- t(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que
#pasa por el 0 en las y


# Circunferencia
circunferencia <- function (h , k , r) {
   if (r >= 0) { # r tiene que ser positivo
    if (r == 0) { # si es r = 0 , entonces es un punto
       plot (x = h , y = k , xlab = " Eje X", ylab = "Eje Y") # grafica del punto
    } else {
         x <- seq(h - r , h + r , 0.01) # ya que no podemos graficar en todo R^2
        ypositiva <- k + sqrt (r ^2 - (( x - h) ^2) ) # parte positiva de la circunferencia
         ynegativa <- k - sqrt (r ^2 - (( x - h) ^2) ) # parte negativa de la circunferencia
         # graficamos primero la parte positiva
         plot (x , ypositiva , type = "l", xlim = c(h - (r + 1) , h + (r + 1) ) , ylim = c(k - (r + 1) ,
                k + ( r + 1) ) ,
                 xlab = " Eje X", ylab = " Eje Y")
         lines (x , ynegativa , type = "l") # agregamos la parte negativa
         abline ( h = 0, v = 0) # agregamos los ejes
        points ( x = h , y = k , col = " red ") # dibujamos el centro
     }
  } else{
      return ( print ("El radio no es positivo ."))
   }
}

circunferencia (-4,1,6) # ejecutamos la funcion


#Circunferencia 2

circunferencia <- function (h , k , r) {
  if (r >= 0) { # r tiene que ser positivo
    if (r == 0) { # si es r = 0 , entonces es un punto
      plot (x = h , y = k , xlab = " Eje X", ylab = "Eje Y") # grafica del punto
    } else {
      x <- seq(h - r , h + r , 0.01) # ya que no podemos graficar en todo R^2
      ypositiva <- k + sqrt (r ^2 - (( x - h) ^2) ) # parte positiva de la circunferencia
      ynegativa <- k - sqrt (r ^2 - (( x - h) ^2) ) # parte negativa de la circunferencia
      # graficamos primero la parte positiva
      plot (x , ypositiva , type = "l", xlim = c(h - (r + 1) , h + (r + 1) ) , ylim = c(k - (r + 1) ,
                                                                                        k + ( r + 1) ) ,
            xlab = " Eje X", ylab = " Eje Y")
      lines (x , ynegativa , type = "l") # agregamos la parte negativa
      abline ( h = 0, v = 0) # agregamos los ejes
      points ( x = h , y = k , col = " red ") # dibujamos el centro
    }
  } else{
    return ( print ("El radio no es positivo ."))
  }
}

circunferencia (1,-4,5.5) # ejecutamos la funcion


# Elipse

elipse <- function (h , k , a , b , horizontal){
if (a > b){ # a tiene que ser mayor que b
  c <- sqrt ( a ^2 - b ^2) # calculamos c
  if ( horizontal ){ # si es una elipse horizontal
      x <- seq(h - a , h + a , 0.01) # definimos el dominio
      ypositiva <- k + sqrt (( b ^2 - (b ^2/a ^2) * (( x - h) ^2) )) # parte positiva
      ynegativa <- k - sqrt (( b ^2 - (b ^2/a ^2) * (( x - h) ^2) )) # parte negativa
      # graficamos primero la parte positiva
      plot (x , ypositiva , type = "l", xlim = c(h - (a + 1) , h + (a + 1) ) , ylim = c(k - (b + 1) ,
              k + ( b + 1) ), xlab= "Eje X", ylab= "Eje Y")
      lines (x , ynegativa , type = "l") # agregamos la parte negativa
      abline ( h = 0, v = 0) # ejes coordenados
      points ( x = c(h - c, h + c) , y = c(k , k ) , col = " red ") # focos
    } else {
      x <- seq(h - b , h + b , 0.01)
      ypositiva <- k + sqrt (( a ^2 - (a ^2/b ^2) * (( x - h) ^2) ))
      ynegativa <- k - sqrt (( a ^2 - (a ^2/b ^2) * (( x - h) ^2) ))
      plot (x , ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1) ) , ylim = c(k - (a + 1) ,
              k + ( a + 1) ), xlab= "Eje X", ylab= "Eje Y")
      lines (x , ynegativa , type = "l")
      abline ( h = 0, v = 0)
      points ( x = c(h , h ) , y = c(k - c, k + c) , col = " red ")
    }
  } else {
      return ( print ("No cumple las condiciones para ser una elipse . (a no es mayor que b)"))
  }
}

elipse (-2 , 3 , 64 , 25, F) #Ejecutamos

#Elipse 2
elipse <- function (h , k , a , b , horizontal){
  if (a > b){ # a tiene que ser mayor que b
    c <- sqrt ( a ^2 - b ^2) # calculamos c
    if ( horizontal ){ # si es una elipse horizontal
      x <- seq(h - a , h + a , 0.01) # definimos el dominio
      ypositiva <- k + sqrt (( b ^2 - (b ^2/a ^2) * (( x - h) ^2) )) # parte positiva
      ynegativa <- k - sqrt (( b ^2 - (b ^2/a ^2) * (( x - h) ^2) )) # parte negativa
      # graficamos primero la parte positiva
      plot (x , ypositiva , type = "l", xlim = c(h - (a + 1) , h + (a + 1) ) , ylim = c(k - (b + 1) ,k + ( b + 1) ), xlab= "Eje X", ylab= "Eje Y")
                                                                                        
      lines (x , ynegativa , type = "l") # agregamos la parte negativa
      abline ( h = 0, v = 0) # ejes coordenados
      points ( x = c(h - c, h + c) , y = c(k , k ) , col = " red ") # focos
    } else {
      x <- seq(h - b , h + b , 0.01)
      ypositiva <- k + sqrt (( a ^2 - (a ^2/b ^2) * (( x - h) ^2) ))
      ynegativa <- k - sqrt (( a ^2 - (a ^2/b ^2) * (( x - h) ^2) ))
      plot (x , ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1) ) , ylim = c(k - (a + 1) ,k + ( a + 1) ), xlab= "Eje X", ylab= "Eje Y")
                                                                                        
      lines (x , ynegativa , type = "l")
      abline ( h = 0, v = 0)
      points ( x = c(h , h ) , y = c(k - c, k + c) , col = " red ")
    }
  } else {
    return ( print ("No cumple las condiciones para ser una elipse . (a no es mayor que b)"))
  }
}

elipse (1 , -2 , 36 , 4, T) #Ejecutamos


# Hiperbola
hiperbola <- function (h , k , a , b , horizontal ){
  c <- sqrt (a ^2 + b ^2) # calculamos c
  if ( horizontal ){ # hiperbola sobre el eje x
    xizq <- seq (h - (a + 3) , h - a , 0.01) # dominio izquierdo
    xder <- seq (h + a , h + (a + 3) , 0.01) # dominio derecho
    yizqpositiva <- k + sqrt (( b ^2/a ^2) *(( xizq - h) ^2) - b ^2) # parte positiva del dominio
    #izquierdo
    yizqnegativa <- k - sqrt (( b ^2/a ^2) *(( xizq - h) ^2) - b ^2) # parte negativa del dominio
    #izquierdo
    yderpositiva <- k + sqrt (( b ^2/a ^2) *(( xder - h) ^2) - b ^2) # parte positiva del dominio
    #izquierdo
    ydernegativa <- k - sqrt (( b ^2/a ^2) *(( xder - h) ^2) - b ^2) # parte negativa del dominio
    #izquierdo
    
    # graficamos la parte positiva del dominio izquierdo
    plot ( xizq , yizqpositiva , type = "l", xlim = c(h - ( a + 4) , h + (a + 4) ) , ylim = c(k - (b +
                                                                                                     4) , k + (b + 4) ) ,
           xlab = " Eje X", ylab = " Eje Y")
    lines ( xizq , yizqnegativa , type = "l") # agregamos parte negativa del dominio izquierdo
    lines ( xder , ydernegativa , type = "l")
    lines ( xder , yderpositiva , type = "l")
    abline ( h = 0, v = 0) # ejes coordenados
    points ( x = c(h - (a + c)) , y = c(k) , col = " red ")# focos
    points ( x = c(h + (a + c)) , y = c(k) , col = " red ")
  } else { # hiperbola sobre el eje y
    yizq <- seq (k - (a + 3) , k - a , 0.01) # rango inferior
    yder <- seq (k + a , k + (a + 3) , 0.01) # rango superior
    xizqpositiva <- h + sqrt (( b ^2/a ^2) *(( yizq - k) ^2) - b ^2) # parte positiva del rango
    #inferior
    xizqnegativa <- h - sqrt (( b ^2/a ^2) *(( yizq - k) ^2) - b ^2) # parte negativa del rango
    #superior
    xderpositiva <- h + sqrt (( b ^2/a ^2) *(( yder - k) ^2) - b ^2) # parte positiva del rango
    #inferior
    xdernegativa <- h - sqrt (( b ^2/a ^2) *(( yder - k) ^2) - b ^2) # parte negativa del rango
    #superior
    # graficamos
    plot ( xizqpositiva , yizq , type = "l", xlim = c(h - ( b + 4) , h + (b + 4) ) , ylim = c(k - (a +
                                                                                                     4) , k + (a + 4) ) ,
           xlab = " Eje X", ylab = " Eje Y")
    lines ( xizqnegativa , yizq , type = "l")
    lines ( xdernegativa , yder , type = "l")
    lines ( xderpositiva , yder , type = "l")
    abline ( h = 0, v = 0)
    points ( x = c(h) , y = c(k - (a + c)) , col = " red ") # focos
    points ( x = c(h) , y = c(k + (a + c)) , col = " red ")
  }
}

hiperbola (1 , 1, 4, 3 , T )


# Hiperbola
hiperbola <- function (h , k , a , b , horizontal ){
  c <- sqrt (a ^2 + b ^2) # calculamos c
  if ( horizontal ){ # hiperbola sobre el eje x
    xizq <- seq (h - (a + 3) , h - a , 0.01) # dominio izquierdo
    xder <- seq (h + a , h + (a + 3) , 0.01) # dominio derecho
    yizqpositiva <- k + sqrt (( b ^2/a ^2) *(( xizq - h) ^2) - b ^2) # parte positiva del dominio
    #izquierdo
    yizqnegativa <- k - sqrt (( b ^2/a ^2) *(( xizq - h) ^2) - b ^2) # parte negativa del dominio
    #izquierdo
    yderpositiva <- k + sqrt (( b ^2/a ^2) *(( xder - h) ^2) - b ^2) # parte positiva del dominio
    #izquierdo
    ydernegativa <- k - sqrt (( b ^2/a ^2) *(( xder - h) ^2) - b ^2) # parte negativa del dominio
    #izquierdo
    
    # graficamos la parte positiva del dominio izquierdo
    plot ( xizq , yizqpositiva , type = "l", xlim = c(h - ( a + 4) , h + (a + 4) ) , ylim = c(k - (b +
                                                                                                     4) , k + (b + 4) ) ,
           xlab = " Eje X", ylab = " Eje Y")
    lines ( xizq , yizqnegativa , type = "l") # agregamos parte negativa del dominio izquierdo
    lines ( xder , ydernegativa , type = "l")
    lines ( xder , yderpositiva , type = "l")
    abline ( h = 0, v = 0) # ejes coordenados
    points ( x = c(h - (a + c)) , y = c(k) , col = " red ")# focos
    points ( x = c(h + (a + c)) , y = c(k) , col = " red ")
  } else { # hiperbola sobre el eje y
    yizq <- seq (k - (a + 3) , k - a , 0.01) # rango inferior
    yder <- seq (k + a , k + (a + 3) , 0.01) # rango superior
    xizqpositiva <- h + sqrt (( b ^2/a ^2) *(( yizq - k) ^2) - b ^2) # parte positiva del rango
    #inferior
    xizqnegativa <- h - sqrt (( b ^2/a ^2) *(( yizq - k) ^2) - b ^2) # parte negativa del rango
    #superior
    xderpositiva <- h + sqrt (( b ^2/a ^2) *(( yder - k) ^2) - b ^2) # parte positiva del rango
    #inferior
    xdernegativa <- h - sqrt (( b ^2/a ^2) *(( yder - k) ^2) - b ^2) # parte negativa del rango
    #superior
    # graficamos
    plot ( xizqpositiva , yizq , type = "l", xlim = c(h - ( b + 4) , h + (b + 4) ) , ylim = c(k - (a +
                                                                                                     4) , k + (a + 4) ) ,
           xlab = " Eje X", ylab = " Eje Y")
    lines ( xizqnegativa , yizq , type = "l")
    lines ( xdernegativa , yder , type = "l")
    lines ( xderpositiva , yder , type = "l")
    abline ( h = 0, v = 0)
    points ( x = c(h ) , y = c(k - (a + c)) , col = " red ") # focos
    points ( x = c(h ) , y = c(k + (a + c)) , col = " red ")
  }
}

hiperbola (3 , 2, 5, 4 , F )
