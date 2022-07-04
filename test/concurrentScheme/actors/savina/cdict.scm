(define NumWorkers (int-top))
(define NumMsgsPerWorker (int-top))
(define WritePercent (modulo (int-top) 100))

(define (build-vector n f)
  (letrec ((v (make-vector n #f))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))
(define (vector-foreach f v)
  (letrec ((loop (lambda (i)
                   (if (< i (vector-length v))
                       (begin
                         (f (vector-ref v i))
                         (loop (+ i 1)))
                       'done))))
    (loop 0)))

(define master (actor "master" (workers dictionary terminated)
                        (create-workers ()
                                        (let ((workers (build-vector NumWorkers (lambda (i) (create worker a/self dictionary i 0)))))
                                          (vector-foreach (lambda (w) (send w do-work)) workers)
                                          (become master workers dictionary terminated)))
                        (end-work ()
                                  (if (= (+ terminated 1) NumWorkers)
                                      (begin
                                        (send dictionary end-work)
                                        (terminate))
                                      (become master workers dictionary (+ terminated 1))))))
(define worker (actor "worker" (master dictionary id message-count)
                        (do-work ()
                                 (let ((an-int (random 100)))
                                   (if (< an-int WritePercent)
                                       (send dictionary write a/self (random 100) (random 100))
                                       (send dictionary read a/self (random 100)))
                                   (become worker master dictionary id (+ message-count 1))))
                        (result (value)
                             (if (<= (+ message-count 1) NumMsgsPerWorker)
                                 (let ((an-int (random 100)))
                                   (if (< an-int WritePercent)
                                       (send dictionary write a/self (random 100) (random 100))
                                       (send dictionary read a/self (random 100)))
                                   (become worker master dictionary id (+ message-count 1)))
                                 (begin
                                   (send master end-work)
                                   (terminate))))))
(define dictionary (actor "dictionary" (state)
                            (write (sender key value)
                                   (send sender result value)
                                   (become dictionary (cons (cons key value) state)))
                            (read (sender key)
                                  (let ((res (assoc key state)))
                                    (if (pair? res)
                                        (send sender result (cdr res))
                                        (send sender result 0)))
                                  (become dictionary state))
                            (end-work () (terminate))))
(define dictionary-initial-state '())
(define (start-master)
  (let* ((dictionary (create dictionary dictionary-initial-state))
         (master (create master #f dictionary 0)))
    (send master create-workers)))
(start-master)
