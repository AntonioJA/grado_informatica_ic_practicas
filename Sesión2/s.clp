;reglas

;;; Cargar al princiop los hechos con pares sintomas y enfermedad

;;; moid hip

;- si solo sintaomas gripales, gripe HIP
;- Si sintomas gripales y dolor nuca o rig hip meningitis
;- si sintomas gripales y ronnchas hip denge
;- Sintomas gripales y persintencia o fiebre alta n1h1

;;; mod dd

;peina en enfermedades que podrian confundirse, depende de la hipotesis

;- si no descartado meningitis, dd meningitis
;- Si hip N1H1 dd denge
;- si hip denge dd N1H1
;- si hip meningits directo meningitis


;esquema]

; dice sintoms, hago hip, pienso con cual enfermedad mas puede casar, preguntamos; para descartar con que enfermedades puede coincir..

; Dolor cabe, mal estar, hip gripe, dd meningitis, preguntamos por sintoma de dd,; en este caso rigidez nuca. Si le duele asertamos sintoma. Descartamos y; volvemos a hipotesis.

; Inferimos en dd y en hip.

; Faltan las reglas de que preguntar.

; Reglas para descartar enfermedades. en modulo pregunta.

; Descartar

; - Si no dolor nuca ni rigidez, desc meningitis.
; - Si no persistencia en sintomas ni fieb alta descar N1H1
; - Si no roncha ni epidemia desc denge.
; - Si ronchas y no epidemia, no se descarta denge

; No nos ha dicho, por ejemplo, que si desc una enfermedad, en la regla que toma; como hip no la aplica, en el antecedente tientie que poner no descartado.

; Lo mismo para el dd

; No puede coge mismo hip y dd


(deftemplate sin-enf
  (slot sintoma)
  (slot intensidad)
  (slot duracion)
)

(deffacts HECHOS
  (noDescartado GRIPE)
  (noDescartado MENINGITIS)
  (noDescartado N1H1)
  (noDescartado DENGUE)

  (sin-enf (sintoma CUELLO))
  (sin-enf (sintoma TOS))
  (sin-enf (sintoma RONCHAS))
  (sin-enf  (sintoma FIEBRE))
  (sin-enf  (sintoma MAL))
  (sin-enf  (sintoma DOLOR))

)




(deffunction ask-question (?qBEG ?qMID ?qEND $?allowed-values)

	(printout t ?qBEG crlf crlf)
  (printout t ?qMID crlf)
  (printout t "Introduce (" ?qEND"): ")
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer))
	)
	(while (not (member ?answer ?allowed-values)) do
    (printout t ?qBEG crlf crlf)
    (printout t ?qMID crlf)
    (printout t "Introduce (" ?qEND"): ")
  	(bind ?answer (read))
  	(if (lexemep ?answer)
  		then (bind ?answer (lowcase ?answer))
  	)
	)
?answer)

(defrule firstQuestion
  ?x <- (initial-fact)
	=>
  (retract ?x)
	(bind ?r (ask-question
    "¿Cuales son tus síntomas?"
    "Fiebre, Dolor de cabeza, mal estar general"
    "fiebre, dolor, mal, p"
    fiebre dolor mal p))
  (if (neq ?r p)
		then (assert (initial-fact))
         (assert (sin-enf (sintoma ?r)))
  else
    (assert (modulo-hipotesis))
    (retract ?x)
  )
  (watch facts)
)

;;
;; HIPOTESIS
;;

(defrule hip1
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma FIEBRE))
  (noDescartado GRIPE)
=>
  (assert (hipotesis GRIPE))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip2
  ?ml <- (modulo-hipotesis)
  (noDescartado MENINGITIS)
  (sin-enf (sintoma CUELLO))
  =>
  (assert (hipotesis MENINGITIS))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip3
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma RONCHAS))
  (noDescartado DENGUE)
  =>
  (assert (hipotesis DENGUE))
  (assert (modulo-dd))
  (retract ?ml)
)

(defrule hip4
  ?ml <- (modulo-hipotesis)
  (sin-enf (sintoma ?x) (duracion SEMANAS))
  (noDescartado N1H1)
  =>
  (assert (hipotesis N1H1))
  (assert (modulo-dd))
  (retract ?ml)
)


;;
;; Diagnóstico diferencial
;;

(defrule dd1
  ?ml <- (modulo-dd)
  (noDescartado MENINGITIS)
=>
  (assert (dd MENINGITIS))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd2
  ?ml <- (modulo-dd)
  (hipotesis N1H1)
=>
  (assert (dd DENGUE))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd3
  ?ml <- (modulo-dd)
  (hipotesis DENGUE)
=>
  (assert (dd N1H1))
  (assert (modulo-pregunta))
  (retract ?ml)
)

(defrule dd4
  ?ml <- (modulo-dd)
  (hipotesis MENINGITIS)
=>
  (assert (dd MENINGITIS))
  (assert (modulo-pregunta))
  (retract ?ml)
)

;;
;; MOD Preguntas
;;

(defrule pregunta1
  ?ml <- (modulo-pregunta)
  ?dd <- (dd MENINGITIS)
  ?e <- (noDescartado GRIPE)
  ?e1 <- (noDescartado MENINGITIS)
  =>
  (bind ?q (ask-question
    "¿Tienes rigidez en el cuello o dolor en la nuca?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q si)
      then
        (retract ?e)
      else
        (retract ?e1)
    )

    (retract ?ml)
    (assert (modulo-hipotesis))
)

(defrule pregunta2
  ?ml <- (modulo-pregunta)
  ?dd <- (dd DENGUE)
  ?e <- (noDescartado DENGUE)
  =>
  (bind ?q (ask-question
    "¿Tienes ronchas?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q no)
      then
        (retract ?e)
    )
    (retract ?ml)
    (assert (modulo-hipotesis))
)

(defrule pregunta3
  ?ml <- (modulo-pregunta)
  ?dd <- (dd N1H1)
  ?e <- (noDescartado GRIPE)
  ?e1 <-(noDescartado N1H1)
  ?si <- (sin-enf (sintoma ?x))
  =>
  (bind ?q (ask-question
    "¿Llevas más de una semana con los sintomas?"
    "si/no"
    "si/no"
    si no))

    (if (eq ?q si)
      then
        (retract ?e)
        (assert (sin-enf (sintoma ?x) (duracion SEMANAS)))
      else
        (retract ?e1)
    )

    (retract ?ml)
    (assert (modulo-hipotesis))
)
