;Andrés Campos
;Eric Parton

#lang racket
(require plot)

;--------------------------Univariable functions---------------------------------------------------------

;Function with friendlier input syntax since it initialises some variables
(define (runGradientDescent xi fx alpha iterations)
  ;Get the derivative from the function
  (let ([dx (getDeriv fx)]
        [a (list-ref fx 0)]
        [b (list-ref fx 1)]
        [c (list-ref fx 2)]
        [d (list-ref fx 3)]
        [e (list-ref fx 4)]
        [f (list-ref fx 5)]
        [g (list-ref fx 6)]
        [h (list-ref fx 7)]
        [i (list-ref fx 8)]
        [j (list-ref fx 9)])
    ;Call gradient descent with the inital values for x and y passed as lists
    (gradientDescent xi fx dx alpha (list xi) (list (calculateFunctionVal xi a b c d e f g h i j)) 0 iterations)
    )
  )

;Returns the parameters for the derivative of f(x)
(define (getDeriv fx)
  (let ([a (list-ref fx 0)]
        [b (list-ref fx 1)]
        [c (list-ref fx 2)]
        [d (list-ref fx 3)]
        [e (list-ref fx 4)]
        [f (list-ref fx 5)]
        [g (list-ref fx 6)]
        [h (list-ref fx 7)]
        [i (list-ref fx 8)]
        [j (list-ref fx 9)])
    (list
     (* 1  b)
     (* 2  c)
     (* 3  d)
     (* 4  e)
     (* 5  f)
     (* 6  g)
     (* 7  h)
     (* 8  i)
     (* 9  j)
     (* 10 0)
     )
    )
  )

;Main function: initial x, function parameters, derivative parameters, learning rate
(define (gradientDescent xi fx dx alpha xpoints ypoints iterations iterationsLimit)
  ;Declare some useful 'variables'
  (let* (
        ;Function parameters
        [a (list-ref fx 0)]
        [b (list-ref fx 1)]
        [c (list-ref fx 2)]
        [d (list-ref fx 3)]
        [e (list-ref fx 4)]
        [f (list-ref fx 5)]
        [g (list-ref fx 6)]
        [h (list-ref fx 7)]
        [i (list-ref fx 8)]
        [j (list-ref fx 9)]
        ;Derivative parameters
        [da (list-ref dx 0)]
        [db (list-ref dx 1)]
        [dc (list-ref dx 2)]
        [dd (list-ref dx 3)]
        [de (list-ref dx 4)]
        [df (list-ref dx 5)]
        [dg (list-ref dx 6)]
        [dh (list-ref dx 7)]
        [di (list-ref dx 8)]
        [dj (list-ref dx 9)]
        ;Derivative slope
        [dm (calculateFunctionVal xi da db dc dd de df dg dh di dj)]
        ;Calculate the new x
        [xn (gradientIteration xi alpha dm)]
        )
    ;Append the new points to the points list
    
    
    ;Check how the algorithm is doing
    (cond
      ;If the difference between the old x and new x is small (algorithm has converged), the minimum has been found
      ;If the minimum has been found, plot the function with the min found
      [(< (abs (- xn xi)) .001)
       (display "Convergence at (")
       (display xn)
       (display ", ")
       (display (calculateFunctionVal xn a b c d e f g h i j))
       (display ") after ")
       (display iterations)
       (display " iterations")
       (plotGradientDescent a b c d e f g h i j xpoints ypoints)]
      ;If it's gone over the iteration limit
      [(> iterations iterationsLimit) (display "Function did not converge, try more iterations or change the learning rate")]
      ;Otherwise, go to the next iteration with the new points added to the lists
      [else (gradientDescent xn fx dx alpha (append xpoints (list xn)) (append ypoints (list (calculateFunctionVal xn a b c d e f g h i j))) (+ 1 iterations) iterationsLimit)]
      )
    )
  )

(define (plotGradientDescent a b c d e f g h i j xs ys)
  ;Plot the points that the gradient descent took along with the graph
  ;X range is x - 10 to x + 10
  (plot (list (function-interval (λ (x) 0) (λ (x) (+ (* (expt x 0) a) (* (expt x 1) b) (* (expt x 2) c) (* (expt x 3) d) (* (expt x 4) e) (*(expt x 5) f) (* (expt x 6) g) (* (expt x 7) h) (*(expt x 8) i) (* (expt x 9) j))) (- (last xs) 10) (+ (last xs) 10))
              (points (map vector xs ys) #:color 'red)))
  )

;Calculate the new x value based on some stuff
(define (gradientIteration xi alpha dm)
  (- xi (* alpha dm))
  )

;Calculate a function given x and its parameters
(define (calculateFunctionVal x a b c d e f g h i j)
  (+ (* (expt x 0) a) (* (expt x 1) b) (* (expt x 2) c) (* (expt x 3) d) (* (expt x 4) e) (*(expt x 5) f) (* (expt x 6) g) (* (expt x 7) h) (*(expt x 8) i) (* (expt x 9) j))
  )


;--------------------------Multivariable functions---------------------------------------------------------

;Functions reprsented as a + bx + cx^2 + dy + ey^2 + exy + fxy^2 + gx^2y + hx^2y^2

;Function with friendlier input syntax since it initialises some variables
(define (runGradientDescent3D xi yi fxy alpha iterations)
  ;Get the derivative from the function
  (let ([dx (getDerivX3D fxy)]
        [dy (getDerivY3D fxy)]
        [a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)])
    ;Call gradient descent with the inital values for x and y passed as lists
    (gradientDescent3D xi fxy dx dy alpha (list xi) (list (calculateFunctionVal3D xi a b c d e f g h)) 0 iterations)
    )
  )

;Returns the parameters for the derivative of f(x, y) with respect to x
;Functions reprsented as a + bx + cx^2 + dy + ey^2 + exy + fxy^2 + gx^2y + hx^2y^2
(define (getDerivX3D fxy)
  (let ([a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)])
    (list
     (* 1  b)
     (* 2  c)
     (* 0  d)
     (* 0  e)
     (* 5  f)
     (* 6  g)
     (* 7  h)
     (* 8  i)
     (* 9  j)
     (* 10 0)
     )
    )
  )

;Returns the parameters for the derivative of f(x, y) with respect to y
;Functions reprsented as a + bx + cx^2 + dy + ey^2 + exy + fxy^2 + gx^2y + hx^2y^2
(define (getDerivY3D fxy)
  (let ([a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)])
    (list
     (* 1  b)
     (* 2  c)
     (* 3  d)
     (* 4  e)
     (* 5  f)
     (* 6  g)
     (* 7  h)
     (* 8  i)
     (* 9  j)
     (* 10 0)
     )
    )
  )

;Main function: initial x, function parameters, derivative parameters, learning rate
(define (gradientDescent3D xi fx dx alpha xpoints ypoints iterations iterationsLimit)
  ;Declare some useful 'variables'
  (let* (
        ;Function parameters
        [a (list-ref fx 0)]
        [b (list-ref fx 1)]
        [c (list-ref fx 2)]
        [d (list-ref fx 3)]
        [e (list-ref fx 4)]
        [f (list-ref fx 5)]
        [g (list-ref fx 6)]
        [h (list-ref fx 7)]
        [i (list-ref fx 8)]
        [j (list-ref fx 9)]
        ;Derivative parameters
        [da (list-ref dx 0)]
        [db (list-ref dx 1)]
        [dc (list-ref dx 2)]
        [dd (list-ref dx 3)]
        [de (list-ref dx 4)]
        [df (list-ref dx 5)]
        [dg (list-ref dx 6)]
        [dh (list-ref dx 7)]
        [di (list-ref dx 8)]
        [dj (list-ref dx 9)]
        ;Derivative slope
        [dm (calculateFunctionVal3D xi da db dc dd de df dg dh di dj)]
        ;Calculate the new x
        [xn (gradientIteration3D xi alpha dm)]
        )
    ;Append the new points to the points list
    
    
    ;Check how the algorithm is doing
    (cond
      ;If the difference between the old x and new x is small (algorithm has converged), the minimum has been found
      ;If the minimum has been found, plot the function with the min found
      [(< (abs (- xn xi)) .001)
       (display "Convergence at (")
       (display xn)
       (display ", ")
       (display (calculateFunctionVal xn a b c d e f g h i j))
       (display ") after ")
       (display iterations)
       (display " iterations")
       (plotGradientDescent a b c d e f g h i j xpoints ypoints)]
      ;If it's gone over the iteration limit
      [(> iterations iterationsLimit) (display "Function did not converge, try more iterations or change the learning rate")]
      ;Otherwise, go to the next iteration with the new points added to the lists
      [else (gradientDescent3D xn fx dx alpha (append xpoints (list xn)) (append ypoints (list (calculateFunctionVal3D xn a b c d e f g h i j))) (+ 1 iterations) iterationsLimit)]
      )
    )
  )

(define (plotGradientDescent3D a b c d e f g h i j xs ys)
  ;Plot the points that the gradient descent took along with the graph
  ;X range is x - 10 to x + 10
  (plot (list (function-interval (λ (x) 0) (λ (x) (+ (* (expt x 0) a) (* (expt x 1) b) (* (expt x 2) c) (* (expt x 3) d) (* (expt x 4) e) (*(expt x 5) f) (* (expt x 6) g) (* (expt x 7) h) (*(expt x 8) i) (* (expt x 9) j))) (- (last xs) 10) (+ (last xs) 10))
              (points (map vector xs ys) #:color 'red)))
  )

;Calculate the new x value based on some stuff
(define (gradientIteration3D xi alpha dm)
  (- xi (* alpha dm))
  )

;Calculate a function given x and its parameters
(define (calculateFunctionVal3D x a b c d e f g h i j)
  (+ (* (expt x 0) a) (* (expt x 1) b) (* (expt x 2) c) (* (expt x 3) d) (* (expt x 4) e) (*(expt x 5) f) (* (expt x 6) g) (* (expt x 7) h) (*(expt x 8) i) (* (expt x 9) j))
  )


;References
;https://www.charlesbordet.com/en/gradient-descent/#but-the-maths