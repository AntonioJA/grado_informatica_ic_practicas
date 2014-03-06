(deftemplate Persona
    (field Nombre)
    (field ColorOjos)
    (field ColorPelo)
)

(defrule ParejaComplicada
    (Persona
        (Nombre ?N1)
        (ColorOjos ?Ojos1 & azul|verde) ; Almacenamos en Ojos1 el valor del color de ojos
        (ColorPelo ?Pelo1 & ~negro))
    (Persona
        (Nombre ?N2 & ~?N1) ; Para que no se compare con la misma persona N1 tiene que ser distinto de N2
        (ColorOjos ?Ojos2 & ~?Ojos1)
        (ColorPelo ?Pelo2 & rojo | ?Pelo1))
    =>
    (printout t ?N1 " tiene los ojos " ?Ojos1
        " y el pelo " ?Pelo1 crlf
        ?N2 " tiene los ojos " ?Ojos2
        " y el pelo " ?Pelo2 crlf))

(deffacts Personas
    (Persona
        (Nombre Alex)
        (ColorOjos azul)
        (ColorPelo rubio))
    (Persona
        (Nombre Guille)
        (ColorOjos verde)
        (ColorPelo Marron))
    (Persona
        (Nombre Maria)
        (ColorOjos negros)
        (ColorPelo rubio))
    (Persona
        (Nombre Eladio)
        (ColorOjos rojo)
        (ColorPelo rojo))
)


