(deftemplate relacion
  (slot sintoma)
  (slot presente (default no))
  (slot intensidad)
  (slot duracion)
)

(deftemplate epidemia
  (slot nombre)
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
    "Fiebre, Dolor de cabeza, tos, mal estar general"
    "fiebre, dolor, tos, mal, p"
    fiebre dolor tos mal p))
  (if (neq ?r p)
		then (assert (initial-fact))
         (assert (relacion (sintoma ?r) (presente si)))
  else
    (assert (modulo-hipotesis))
    (retract ?x)
  )
  (watch facts)
)

(defrule user-quits
  (salir)
=>
  (printout t "You have QUIT the program." crlf)
  (halt)
)

;;
;; HIPOTESIS
;;

(defrule hip-inicial
  ?ml <- (modulo-hipotesis)
  ?x <- (relacion (sintoma ?s))
  =>
  (if (or (eq ?s fiebre) (eq ?s mal))
    then
      (assert (hipotesis GRIPE))
    else
      (assert (quit))
  )
  (retract ?ml)
  (assert (modulo-dd))
)

;;
;; Diagnóstico diferencial
;;

(defrule dd
  ?ml <- (modulo-dd)
  ?hip <- (hipotesis ?enf)
  ?x <- (relacion (sintoma ?x1))
  ?y <- (relacion (sintoma ?y1))
  =>
  (assert (dd MENINGITIS))
  (retract ?ml)
  (assert (modulo-pregunta))
)


;;
;; Preguntamos para descartar o afirmar hipotesis
;;
;; Aqui solo se pregunta y se aserta la respuesta, y volvemos al modulo de hipotesis, se actualiza la hipotesis en el mod de hipotesis. De hip a dd, luego a pregunta, y al final diagnostico.

(defrule pregunta-Meningitis
  ?ml <- (modulo-pregunta)
  ?hip <- (hipotesis ?enf)
  ?dd <- (dd MENINGITIS)
  =>
  (bind ?q (ask-question
    "¿Tienes rigidez en el cuello o dolor en la nuca?"
    "si/no"
    "si/no"
    si no))

    (assert (relacion (sintoma rigidez) (presente ?q)))

    (if (eq ?q si)
      then
        (retract ?hip)
        (assert (hipotesis MENINGITIS))
      else
        (retract ?dd)
        (assert (dd DENGE))
    )
    (retract ?ml)
    (assert (modulo-diag))
    ;(assert (modulo-hipotesis))
)

(defrule pregunta-Dengue
  ?ml <- (modulo-pregunta)
	?dd <- (dd DENGE)
	?hip <- (hipotesis ?enf)
  ?epi <- (epidemia (nombre DENGE))
=>
  (bind ?q (ask-question
    "¿Tienes ronchas?"
    "si/no"
    "si/no"
    si no))

  (if ( eq ?q si)
    then
      (retract ?hip)
      (assert ( hipotesis DENGE ))
    else
      (if ?epi
        then
          (retract ?hip)
        	(assert (hipotesis DENGE ))
        else
          (retract ?dd)
        	(assert (dd N1H1))
      )
  )
  (retract ?ml)
  (assert (modulo-diag))
)

(defrule N1H1
  ?ml <- (modulo-pregunta)
	?dd <- (dd N1H1)
	?hip <- (hipotesis ?enfermedad)
=>
  (bind ?q (ask-question
    "¿Llevas más de una semana con dolor de cabeza y fiebre?"
    "si/no"
    "si/no"
    si no))
  (if (eq ?q si)
    then
      (retract ?hip)
      (assert (hipotesis H1N1))
    else
      (retract ?dd)
      (assert(dd GRIPE))
  )
  (retract ?ml)
  (assert (modulo-diag))
)


;;
;; DIAGNOSTICO
;;

(defrule diag
  ?ml <- (modulo-diag)
  ?dd <- (dd ?s)
  ?hip <- (hipotesis ?t)
  (test (eq ?s ?t))
  =>
  (printout t "Vamos a mandarte pruebas para " ?t crlf)
)
