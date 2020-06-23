(letrec ((counter 0)
         (thread (lambda (n)
                   (letrec ((old counter)
                            (new (+ old 1)))
                     (if (cas counter old new)
                         #t
                         (thread n)))))
(t1 (fork (thread 1)))
(t2 (fork (thread 2)))
(t3 (fork (thread 3)))
(t4 (fork (thread 4)))
(t5 (fork (thread 5)))
(t6 (fork (thread 6)))
(t7 (fork (thread 7)))
(t8 (fork (thread 8)))
(t9 (fork (thread 9)))
(t10 (fork (thread 10)))
(t11 (fork (thread 11)))
(t12 (fork (thread 12)))
(t13 (fork (thread 13))))
(join t1)
(join t2)
(join t3)
(join t4)
(join t5)
(join t6)
(join t7)
(join t8)
(join t9)
(join t10)
(join t11)
(join t12)
(join t13))