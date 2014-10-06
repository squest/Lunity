Lunity
======

Stupid attempt to make Clojure/Haskell functions available in Common Lisp

### Usage

simply (load "clojure.lisp") and/or (load "haskell.lisp")

### Rationale

Clojure and Haskell are beautiful languages, and CL is my personal "heritage" language.  
I want to code in CL but added with some modern features, hence this repo.  
Not so much an implementation of both languages on CL, simply made the superficial attempt  
so that I can call things like range, iterate, comp, juxt, etc.

Also, I added some common mathematical functions.

### Here are the list of functions that already implemented:

1. range
2. iterate (not lazy, but with predicate function to stop)
3. sum
4. product
5. permutations
6. combinations
7. prime?
8. primes-under
9. take
10. drop
11. take-while
12. drop-while
13. juxt
14. comp
15. partial
16. clojure map -> cmap
17. spit
18. some?
19. every?
20. palin?
21. pascal
22. numcol
23. colnum
24. sum-primes
25. prime-list
26. fibo-list
27. fibo
28. next-prime
29. lcm-list
30. factors
31. sum-factors
32. count-factors
33. sorted-factors
34. psqr?
35. filter
36. next-prime
37. prev-prime
