;; Benchmark that compare recursive concurrent matrix multiplication with naive sequential matrix multiplication
(define N (expt 2 (int-top)))
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))
(define (random-matrix w h)
  (build-vector N (vector)
                (lambda (i)
                  (build-vector N 0 (lambda (j) (random 100))))))

(define (extract-matrix M size fromx fromy)
  (build-vector size (vector)
                (lambda (i)
                  (build-vector size 0 (lambda (j)
                                         (vector-ref (vector-ref M (+ fromx i)) (+ fromy j)))))))
(define (split-matrix M)
  (let ((half (/ (vector-length M) 2)))
    (let ((M11 (extract-matrix M half 0 0))
          (M12 (extract-matrix M half half 0))
          (M21 (extract-matrix M half 0 half))
          (M22 (extract-matrix M half half half)))
      (list M11 M12 M21 M22))))

(define (combine-matrices size M11 M12 M21 M22)
  (let ((half (vector-length M11)))
    (build-vector size (vector)
                  (lambda (i)
                    (build-vector size 0
                                  (lambda (j)
                                    (if (and (< i half) (< j half))
                                        (vector-ref (vector-ref M11 i) j)
                                        (if (and (>= i half) (< j half))
                                            (vector-ref (vector-ref M12 (- i half)) j)
                                            (if (and (< i half) (>= j half))
                                                (vector-ref (vector-ref M21 i) (- j half))
                                                (vector-ref (vector-ref M22 (- i half)) (- j half)))))))))))

(define (matrix+ A B)
  (build-vector (vector-length A) (vector)
                (lambda (i)
                  (build-vector (vector-length (vector-ref A i)) 0
                                (lambda (j)
                                  (+ (vector-ref (vector-ref A i) j)
                                     (vector-ref (vector-ref B i) j)))))))

(define (matrix-multiply B A)
  (if (= (vector-length A) 1)
      (vector (vector (* (vector-ref (vector-ref A 0) 0) (vector-ref (vector-ref B 0) 0))))
      (let* ((A-sub (split-matrix A))
             (A11 (car A-sub))
             (A12 (cadr A-sub))
             (A21 (caddr A-sub))
             (A22 (cadddr A-sub))
             (B-sub (split-matrix B))
             (B11 (car B-sub))
             (B12 (cadr B-sub))
             (B21 (caddr B-sub))
             (B22 (cadddr B-sub))
             (C11t (fork (matrix+ (matrix-multiply A11 B11) (matrix-multiply A12 B21))))
             (C12t (fork (matrix+ (matrix-multiply A11 B12) (matrix-multiply A12 B22))))
             (C21t (fork (matrix+ (matrix-multiply A21 B11) (matrix-multiply A22 B21))))
             (C22t (fork (matrix+ (matrix-multiply A21 B12) (matrix-multiply A22 B22)))))
        (combine-matrices (vector-length A) (join C11t) (join C12t) (join C21t) (join C22t)))))

(define (matrix-multiply-seq A B)
  (let* ((n (vector-length A))
         (C (build-vector n (vector) (lambda (i) (make-vector n 0)))))
    (letrec ((loopi
              (lambda (i)
                (if (= i n)
                    C
                    (letrec ((loopj
                              (lambda (j)
                                (if (= j n)
                                    'done
                                    (letrec ((loopk
                                              (lambda (k)
                                                (if (= k n)
                                                    'done
                                                    (begin
                                                      (vector-set! (vector-ref C i) j
                                                                   (+ (vector-ref (vector-ref C i) j)
                                                                      (* (vector-ref (vector-ref A i) k)
                                                                         (vector-ref (vector-ref B k) j))))
                                                      (loopk (+ k 1)))))))
                                      (loopk 0)
                                      (loopj (+ j 1)))))))
                      (loopj 0)
                      (loopi (+ i 1)))))))
      (loopi 0))))

(define (check-equality M1 M2)
  (and (= (vector-length M1) (vector-length M2))
       (letrec ((loop-elements (lambda (i j)
                                 (if (= j (vector-length (vector-ref M1 i)))
                                     #t
                                     (if (= (vector-ref (vector-ref M1 i) j) (vector-ref (vector-ref M2 i) j))
                                         (loop-elements i (+ j 1))
                                         #f))))
                (loop-line (lambda (i)
                             (if (= i (vector-length M1))
                                 #t
                                 (if (and (= (vector-length (vector-ref M1 i)) (vector-length (vector-ref M2 i)))
                                          (loop-elements i 0))
                                     (loop-line (+ i 1))
                                     #f)))))
         (loop-line 0))))

(define A (random-matrix N N))
(define B (random-matrix N N))
(define C (matrix-multiply A B))
(define C2 (matrix-multiply-seq A B))
(check-equality C C2)
