# Hello

Just a collection of notes as I work through any coding problems. 

## Karatsuba Multiplication

Implementing this algorithm seemed straight forward, and the aside on it being an improvement over the third grade algorithm was fun, but there was snag. It's all simple until the numbers split into something where one of them isn't a power of 2, e.g. multiplyiny `46 * 134`. That's where it becomes a bit less straightfoward how to calculate `n` and how to split the numbers. The reality is they must be powers of 2, and actually already are, as long as the numbers are padded, e.g. 0046 and 0134. That's what the `halves` and `compute-n` code aims to do and after that, implementing the calculations is straight forward. It really helped to calculate out a product by hand to understand how to handle the "edge cases".