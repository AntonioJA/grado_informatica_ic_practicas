(deftemplate Emergencia
    (field tipo)
    (field sector)
    (field campo)
)

(deftemplate SistemaExtincion
    (field tipo)
    (field status)
    (field UltimaRevision)
)

(defrule Emergencia-Fuego-ClaseB
    (Emergencia 
        (tipo ClaseB))
    (SistemaExtincion
        (tipo DioxidoCarbono)
        (status operativo))
    =>
        (printout t "Usar extintor CO2" crlf)
)
