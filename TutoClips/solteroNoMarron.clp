(deftemplate Persona
    (multifield Nombre)
    (field ColorPelo)
    (field Casado)
)

(defrule SolteroNoMarron
    (Persona 
        (Nombre $?N) ; Variable libre, con $ alacemanos el valor completo del multifield
        (ColorPelo ~Marron) ; No marron
        (Casado No))
    =>
    (printout t $?N " no tiene pelo marrón" crlf)) ; Aquí ?N ya tiene el nombre de arriba


; Personas con pelo de color marron o negro

; & precedido de un campo ?Campo sirve para guardar el valor en dicho campo, por ejemplo
(defrule PersonaMarronONegro
    (Persona
        (Nombre $?N)
        (ColorPelo ?Color & Marron|Negro))
    =>
    (printout t "El color de pelo de " $?N " es " ?Color crlf))

; Pelo ni marron ni negro

(defrule PersonaNiMarronNiNegro
    (Persona
        (Nombre $?N)
        (ColorPelo ~Marron & ~Negro))
    =>
    (printout t $?N " no tiene el pelo marrón ni negro" crlf)
)


; Datos

(deffacts solteros
    (Persona
        (Nombre Guille Alcalde)
        (ColorPelo Negro)
        (Casado No))
    (Persona
        (Nombre Alejandro Alcalde)
        (ColorPelo Marron)
        (Casado No))
    (Persona
        (Nombre Maria Navarro)
        (ColorPelo Rubio)
        (Casado No))
)
