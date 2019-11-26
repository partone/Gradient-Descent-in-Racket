;Eric Parton


;Interesting examples
;(runGradientDescent 5 (list 5 0 1 3 2 0 0 0 0 0) .001 100000)
;(runGradientDescent3D 10 10 (list 3 2 1 2 1 0 0 0 0) .01 1000)

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
    
    ;Check how the algorithm is doing
    (cond
      ;If the difference between the old x and new x is small (algorithm has converged), the minimum has been found
      ;If the minimum has been found, plot the function with the min found
      [(< (expt (- xn xi) 2) .00000001)
       (display "Convergence at (")
       (display xn)
       (display ", ")
       (display (calculateFunctionVal xn a b c d e f g h i j))
       (display ") after ")
       (display iterations)
       (display " iterations\n")
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

;Functions reprsented as a + bx + cx^2 + dy + ey^2 + fxy + gx^2y + hxy^2 + ix^2y^2

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
        [h (list-ref fxy 7)]
        [i (list-ref fxy 8)])
    ;Call gradient descent with the inital values for x and y passed as lists
    (gradientDescent3D xi yi fxy dx dy alpha (list xi) (list yi) (list (calculateFunctionVal3D xi yi a b c d e f g h i)) 0 iterations)
    )
  )

;Returns the parameters for the derivative of f(x, y) with respect to x
;Functions reprsented as a + bx + cx^2 + dy + ey^2 + fxy + gx^2y + hxy^2 + ix^2y^2
(define (getDerivX3D fxy)
  (let ([a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)]
        [i (list-ref fxy 8)])
    (list
     b
     (* 2  c)
     0
     f
     h
     (* 2  g)
     0
     (* 2  i)
     0
     )
    )
  )

;Returns the parameters for the derivative of f(x, y) with respect to y
;Functions reprsented as a + bx + cx^2 + dy + ey^2 + fxy + gx^2y + hxy^2 + ix^2y^2
(define (getDerivY3D fxy)
  (let ([a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)]
        [i (list-ref fxy 8)])
    (list
     d
     f
     g
     (* 2 e)
     0
     (* 2  h)
     (* 2  i)
     0
     0
     )
    )
  )

;Main function: initial x, function parameters, derivative parameters, learning rate
(define (gradientDescent3D xi yi fxy dx dy alpha xpoints ypoints zpoints iterations iterationsLimit)
  ;Declare some useful 'variables'
  (let* (
        ;Function parameters
        [a (list-ref fxy 0)]
        [b (list-ref fxy 1)]
        [c (list-ref fxy 2)]
        [d (list-ref fxy 3)]
        [e (list-ref fxy 4)]
        [f (list-ref fxy 5)]
        [g (list-ref fxy 6)]
        [h (list-ref fxy 7)]
        [i (list-ref fxy 8)]
        ;Derivative parameters
        [dxa (list-ref dx 0)]
        [dxb (list-ref dx 1)]
        [dxc (list-ref dx 2)]
        [dxd (list-ref dx 3)]
        [dxe (list-ref dx 4)]
        [dxf (list-ref dx 5)]
        [dxg (list-ref dx 6)]
        [dxh (list-ref dx 7)]
        [dxi (list-ref dx 8)]
        [dya (list-ref dy 0)]
        [dyb (list-ref dy 1)]
        [dyc (list-ref dy 2)]
        [dyd (list-ref dy 3)]
        [dye (list-ref dy 4)]
        [dyf (list-ref dy 5)]
        [dyg (list-ref dy 6)]
        [dyh (list-ref dy 7)]
        [dyi (list-ref dy 8)]
        ;Get the value of the derivative at this point for dx and dy
        [dxm (calculateFunctionVal3D xi yi dxa dxb dxc dxd dxe dxf dxg dxh dxi)]
        [dym (calculateFunctionVal3D xi yi dya dyb dyc dyd dye dyf dyg dyh dyi)]
        ;Calculate the new x and y
        [xn (gradientIteration3D xi alpha dxm)]
        [yn (gradientIteration3D yi alpha dym)]
        )

    ;Check how the algorithm is doing
    (cond
      ;If the difference between the old x and new x is small (algorithm has converged), the minimum has been found
      ;If the minimum has been found, plot the function with the min found
      ;The minimum is found when deltay^2 + deltax^2 < .001
      [(< (+ (expt (- yn yi) 2) (expt (- xn xi) 2)) .00000001)
       (display "Convergence at (")
       (display xn)
       (display ", ")
       (display yn)
       (display ", ")
       (display (calculateFunctionVal3D xn yn a b c d e f g h i))
       (display ") after ")
       (display iterations)
       (display " iterations\n")
       (plotGradientDescent3D a b c d e f g h i xpoints ypoints zpoints)]
      ;If it's gone over the iteration limit
      [(> iterations iterationsLimit) (display "Function did not converge, try more iterations or change the learning rate")]
      ;Otherwise, go to the next iteration with the new points added to the lists
      [else (gradientDescent3D xn yn fxy dx dy alpha (append xpoints (list xn)) (append ypoints (list yn)) (append zpoints (list (calculateFunctionVal3D xn yn a b c d e f g h i))) (+ 1 iterations) iterationsLimit)]
      )
    )
  )

(define (plotGradientDescent3D a b c d e f g h i xs ys zs)
  ;Plot the points that the gradient descent took along with the graph
  (plot3d (list (surface3d (λ (x y) (+ a (* b x) (* c (expt x 2)) (* d y) (* e (expt y 2)) (* f x y) (* g (expt x 2) y) (* h x (expt y 2)) (* i (expt x 2) (expt y 2))))
                     (- (last xs) 20) (+ (last xs) 20) (- (last ys) 20) (+ (last ys) 20))
                (points3d (map vector xs ys zs) #:color 'red)
                ))
  )

;Calculate the new x value based on some stuff
(define (gradientIteration3D v alpha dm)
  (- v (* alpha dm))
  )

;Calculate a function given x, y and its parameters
(define (calculateFunctionVal3D x y a b c d e f g h i)
  (+ a (* b x) (* c (expt x 2)) (* d y) (* e (expt y 2)) (* f x y) (* g (expt x 2) y) (* h x (expt y 2)) (* i (expt x 2) (expt y 2)))
  )


;References
;https://www.charlesbordet.com/en/gradient-descent/#but-the-maths
;https://towardsdatascience.com/machine-learning-bit-by-bit-multivariate-gradient-descent-e198fdd0df85