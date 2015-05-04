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

;;
;; HIPOTESIS
;;

(defrule hip-gripe
  ?ml <- (modulo-hipotesis)
  (relacion (sintoma dolor))
  (relacion (sintoma mal))
  =>
  (assert (hipotesis GRIPE))
  (retract ?ml)
  (assert (modulo-dd))
  (printout t "rule-hipotesis" crlf)
)

(defrule hip-gripe2
  ?ml <- (modulo-hipotesis)
  ?dd <- (dd MENINGITIS)
  (relacion (sintoma dolor))
  (relacion (sintoma mal))
  (relacion (sintoma rigidez) (presente no))
  (relacion (sintoma nuca) (presente no))
  =>
  (assert (hipotesis GRIPE))
  (retract ?ml)
  (retract ?dd)
  (assert (modulo-dd))
  (printout t "rule-hipotesis2" crlf)
)

;;
;; Diagnóstico diferencial
;;

(defrule dd-meningitis
  ?ml <- (modulo-dd)
  (hipotesis GRIPE)
  (not (relacion (sintoma rigidez)))
  (not (relacion (sintoma nuca)))
  =>
  (assert (dd MENINGITIS))
  (retract ?ml)
  (assert (modulo-pregunta))
  (printout t "rule-dd-meningitis" crlf)
)

(defrule dd-meningitis2
  ?ml <- (modulo-dd)
  (relacion (sintoma rigidez) (presente si))
  (not (relacion (sintoma nuca)))
  =>
  (retract ?ml)
  (assert (dd MENINGITIS))
  (assert (modulo-pregunta))
  (printout t "dd-meningitis2" crlf)
)

;;
;; Preguntamos para descartar o afirmar hipotesis
;;
;; Aqui solo se pregunta y se aserta la respuesta, y volvemos al modulo de hipotesis, se actualiza la hipotesis en el mod de hipotesis. De hip a dd, luego a pregunta, y al final diagnostico.

(defrule preguntas
  ?ml <- (modulo-pregunta)
  (hipotesis GRIPE)
  (dd MENINGITIS)
  =>
  (bind ?q (ask-question
    "¿Tienes rigidez en el cuello?"
    "si/no"
    "si/no"
    si no))
    (assert (relacion (sintoma rigidez) (presente ?q)))
        ; Vamos a hip, dispara gripe, dd, meningitis, pregunta otra vez por molestias nuca, meningitis, y sale.
        ; EN hip, si no tiene dolor nuca ni rigidez, descaratar menigits
    (assert (modulo-hipotesis))
    (retract ?ml)
)

(defrule preguntas2
  ?ml <- (modulo-pregunta)
  ?h <- (hipotesis GRIPE)
  (dd MENINGITIS)
  (relacion (sintoma rigidez) (presente si))
  =>
  (bind ?q (ask-question
    "¿Tienes molestias en la nuca?"
    "si/no"
    "si/no"
    si no))

    (assert (relacion (sintoma nuca) (presente ?q)))
    (retract ?h)
    (assert (modulo-hipotesis))
    (retract ?ml)
)


;;
;; DIAGNOSTICO
;;

(defrule diag
  ?dd <- (dd ?s)
  ?hip <- (hipotesis ?t)
  (test (eq ?s ?t))
  =>
  (printout t "Vamos a mandarte pruebas para " ?t crlf)
)
