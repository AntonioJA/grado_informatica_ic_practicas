(deffacts init
  (menu-level main)
)

(deftemplate relacion
  (slot sintoma (type STRING) (default ?NONE))
  (slot intensidad (type STRING) (default ?NONE))
  (slot duracion (type STRING) (default ?NONE))
)

;; Si elige 5, seguimos pasamos al siguiente modulo
(defrule siguiente-nivel
  ?pr <- (illness-response 5)
  ?ml <- (menu-level main)
=>
  (assert (menu-level duracion))
  (retract ?ml)
  (retract ?pr)
  (facts)
)

(defrule main-menu
  ?ml <- (menu-level main)
=>
  (retract ?ml)
  (printout t crlf crlf crlf)
  (printout t
 "     1) Fiebre " crlf
 "     2) Dolor cabeza " crlf
 "     3) Tos " crlf
 "     4) Mal estar general " crlf
 "     5) He introducido todos mis síntomas " crlf
 "        Respuesta: " )
  (bind ?response (read))
  (assert (illness-response ?response))
  (assert (menu-level main))
  (printout t crlf)
  (facts)
)

;; En base a lo introducido, asertamos varios posibles sintomas
(defrule con-fiebre
  ?pr <- (illness-response 1)
=>
  (retract ?pr)
  (assert (relacion (sintoma fiebre)))
  (facts)
)
(defrule con-dolor-cabeza
  ?pr <- (illness-response 2)
=>
  (retract ?pr)
  (assert (relacion (sintoma dolor-cabeza)))
  (facts)
)
(defrule tos
  ?pr <- (illness-response 3)
=>
  (retract ?pr)
  (assert (relacion (sintoma tos)))
  (facts)
)
(defrule mal-estar
  ?pr <- (illness-response 4)
=>
  (retract ?pr)
  (assert (relacion (sintoma mal-estar)))
  (facts)
)

(defrule duracion-sintomas
    ?ml <- (menu-level duracion)
    (relacion (sintoma ?s&fiebre|dolor-cabeza|tos|mal-estar))
    ?sin <- (relacion (sintoma ?s))
=>
    (printout t crlf crlf crlf)
    (printout t
    "¿Cuanto tiempo llevas teniendo " ?s crlf crlf
    "    1. Días. " crlf
    "    2. Semanas. " crlf
    "    3. Más. " crlf)
    (bind ?response (read))
    (assert (relacion (sintoma ?s) (duracion ?response)))
    (retract ?sin)
    (retract ?ml)
    (assert (menu-level intensidad))
    (printout t crlf)
    (facts)
)

(defrule intensidad
  ?ml <- (menu-level intensidad)
  ?sin <- (relacion (sintoma ?s) (duracion ?t))
=>
  (printout t crlf crlf crlf)
  (printout t
  "Llevas con " ?s " " ?t " tiempo, intensidad?" crlf crlf
  "    1. Normal. " crlf
  "    2. Media. " crlf
  "    3. Alta. " crlf)
  (bind ?response (read))
  (assert (relacion (sintoma ?s) (duracion ?t) (intensidad ?response)))
  (retract ?ml)
  (retract ?sin)
  (facts)
)

;(deffacts enfermedades
;  (relacion (enfermedad gripe) (sintoma fiebre) (intensidad normal))
;  (relacion (enfermedad gripe) (sintoma dolor-cabeza))
;  (relacion (enfermedad gripe) (sintoma tos))
;  (relacion (enfermedad gripe) (sintoma malestar-general))
;
;  (relacion (enfermedad h1n1) (sintoma fiebre) (intensidad alta))
;  (relacion (enfermedad h1n1) (sintoma dolor-cabeza))
;
;  (relacion (enfermedad meningitis) (sintoma fiebre) (intensidad alta))
;  (relacion (enfermedad meningitis) (sintoma dolor-cabeza))
;  (relacion (enfermedad meningitis) (sintoma rigidez-cuello))
;  (relacion (enfermedad meningitis) (sintoma tos) (intensidad media))
;
;  (relacion (enfermedad denge) (sintoma fiebre) (intensidad alta))
;  (relacion (enfermedad denge) (sintoma dolor-cabeza) (intensidad alta))
;  (relacion (enfermedad denge) (sintoma tos) (intensidad alta))
;  (relacion (enfermedad denge) (sintoma ronchas))
;)
;; Hipótesis principal

;- Si no se ha descartado meningitis, se establece como diagnóstico diferencias.
;- Si hay fiebre alta, y no hay epidemia de denge, dh1n1 coomo DD
;- Si hay epidemia de denge, DD denge.
;- Pero teniendo en cuenta que una regla haya descartado el diagnóstico.
;- Si la hipótesis es la misma que el consecuente que la regla, la regla no se aplica.
;- Caso particular, meningitis, si no se aplica la regla no hago DD.

;; Sintomas

;(deftemplate sintoma
;	(slot nombre
;		(default desconocido)
;	)
;	(slot presente
;		(type SYMBOL)
;		(default puede)
;    (allowed-symbols SI NO PUEDE)
;	)
;	(slot intensidad
;		(type SYMBOL)
;		(default desconocida)
;    (allowed-symbols desconocida alta media baja)
;	)
;  (slot duracion
;    (default desconocida)
;    (allowed-symbols desconocida horas dias semanas meses)
;  )
;)
