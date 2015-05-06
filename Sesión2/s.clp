(deftemplate relacion
  (slot sintoma)
  (slot presente (default no))
  (slot intensidad)
  (slot duracion)
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
      (assert (dd MENINGITIS))
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
  ?dd <- (dd ?ddn)
  ?x <- (relacion (sintoma ?x1))
  ?y <- (relacion (sintoma ?y1))
  =>
  (if (eq ?ddn DENGE )
    then
      (retract ?dd)
      (assert (dd DENGE))
  )
  (if (eq ?ddn H1N1)
    then
      (retract ?dd)
      (assert (dd H1N1))
  )
  (if (eq ?ddn MENINGITIS)
    then
      (retract ?dd)
      (assert (dd MENINGITIS))
  )

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

    (if (eq ?q si)
      then
        (retract ?hip)
        (assert (hipotesis MENINGITIS))
        (assert (relacion (sintoma rigidez) (presente si)))
      else
        (retract ?dd)
        (assert (dd DENGE))
        (assert (relacion (sintoma rigidez) (presente no)))
    )
    (retract ?ml)
    (assert (modulo-diag))
)

(defrule pregunta-Dengue
  ?ml <- (modulo-pregunta)
	?dd <- (dd DENGE)
	?hip <- (hipotesis ?enf)
=>

  (bind ?q (ask-question
    "¿Hay epidemia de DENGE?"
    "si/no"
    "si/no"
    si no))

  (assert (epidemia ?q))

  (if (eq ?q si)
    then
      (assert (hipotesis DENGE))
      (retract ?hip)
    else
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
            (retract ?dd)
            (assert (dd H1N1))
      )
  )
  (retract ?ml)
  (assert (modulo-diag))
)

(defrule H1N1
  ?ml <- (modulo-pregunta)
	?dd <- (dd H1N1)
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

(defrule diag-no
  ?ml <- (modulo-diag)
  ?dd <- (dd ?s)
  ?hip <- (hipotesis ?t)
  (test (neq ?s ?t))
  =>
  (assert (modulo-dd))
)
