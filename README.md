# Gradient Descent in Racket  

This is an implementation of the algorithm [gradient descent](https://en.wikipedia.org/wiki/Gradient_descent) in Racket for functions in one variable as well as multi variable.

## Function form

Functions must be able to be written in the following forms:  

### Single variable
*a + bx + cx<sup>2</sup> + dx<sup>3</sup> + ex<sup>4</sup> + fx<sup>5</sup> + gx<sup>6</sup> + hx<sup>7</sup> + ix<sup>8</sup> + jx<sup>9</sup>*

### Multivariable
*a + bx + cx<sup>2</sup> + dy + ey<sup>2</sup> + fxy + gx<sup>2</sup>y + hxy<sup>2</sup> + ix<sup>2</sup>y<sup>2</sup>*

Where a, b, c, d, e, f, g, h, and i are constants
<br/><br/>
## Usage

The two main functions are runGradientDescent and runGradientDescent3D.  

### Single variable

The function *runGradientDescent* receives four parameters:  

| Parameter  | Explanation                                                                                                             |
|------------|-------------------------------------------------------------------------------------------------------------------------|
| xi         | An initial value from which to begin the gradient descent.                                                              |
| fx         | A list containing the parameters (a, b, c...) of the function.  Check the "Function Form" section for the correct form. |
| alpha      | A learning rate.                                                                                                        |
| iterations | The maximum number of iterations that should be executed before giving up (if there's no convergence).                  |  

**Example usage:**  

| Parameter  | Human friendly value                                                                                                    |
|------------|-------------------------------------------------------------------------------------------------------------------------|
| xi         | 5                                                                                                                       |
| fx         |  5 + x<sup>2</sup> + 3x<sup>3</sup> + 2x<sup>4</sup>                                                                    |
| alpha      | .001                                                                                                                    |
| iterations | 100000                                                                                                                  |  

Would be called as:  

`(runGradientDescent 5 (list 5 0 1 3 2 0 0 0 0 0) .001 100000)`

With the following output:  

![Univariable example](https://raw.githubusercontent.com/partone/Gradient-Descent-in-Racket/master/exampleImages/univariable.PNG)
<br/><br/>
### Multivariable

The function *runGradientDescent3D* receives five parameters:  

| Parameter  | Explanation                                                                                                             |
|------------|-------------------------------------------------------------------------------------------------------------------------|
| xi         | An initial x value from which to begin the gradient descent.*                                                           |
| yi         | An initial y value from which to begin the gradient descent.*                                                           |
| fxy        | A list containing the parameters (a, b, c...) of the function.  Check the "Function Form" section for the correct form. |
| alpha      | A learning rate.                                                                                                        |
| iterations | The maximum number of iterations that should be executed before giving up (if there's no convergence).                  |  

**Example usage:**  

| Parameter  | Human friendly value                                                                                                    |
|------------|-------------------------------------------------------------------------------------------------------------------------|
| xi         | 100                                                                                                                     |
| yi         | 100                                                                                                                     |
| fx         | 3 + 2x + x<sup>2</sup> + 2y + y<sup>2</sup>                                                                             |
| alpha      | .01                                                                                                                     |
| iterations | 1000                                                                                                                    |  

Would be called as:  

`(runGradientDescent3D 10 10 (list 3 2 1 2 1 0 0 0 0) .01 1000)`

With the following output:  

![Multivariable example](https://raw.githubusercontent.com/partone/Gradient-Descent-in-Racket/master/exampleImages/multivariable.PNG)
<br/><br/>  
## Additional Comments

- The convergence can be changed in the code.  Currently it's hard-coded to .00000001 which may be too small for some alphas/functions.  
- The input format for the function is as it is so that the derivative can easily be calculated by the program.
