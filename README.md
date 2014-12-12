Lunity
======

Stupid attempt to make Clojure/Haskell functions available in Common Lisp & Racket

### Usage

simply (load "clojure.lisp") or (require clojure) for racket.

### Rationale

Clojure and Haskell are beautiful languages, and CL is my personal "heritage" language.  
I want to code in CL but added with some modern features, hence this repo.  
Not so much an implementation of both languages on CL, simply made the superficial attempt  
so that I can call things like range, iterate, comp, juxt, etc.

Important features added including lazy seq class & basic memoization.

Also created the racket version

### Here are the list of functions that already implemented in CL:

1. -> & ->> macro
2. strict range
3. lazy range => lrange no upper limit
4. take => works for both lazy and strict
5. drop => works for strict list
6. lazy cons => lcons
7. geo-seq like lrange but for creating a geometric sequence
8. take-while
9. drop-while
10. iterate
11. dec and inc
12. div and rem
13. sort-by (non destructive using quicksort)
14. loop recur (CL already have TCO, but I like this loop recur style)
15. clojure's let syntax and behaviour
16. comp
17. juxt
18. partial
19. lreduce (lazy reduce with an extra predicate to check when to stop)
20. function literal macro using fn instead of #(* % %) you can write (fn (* % %)) or (fn2 (* %1 %2))
21. lmap (lazy map)
22. filter (lazy version of CL's remove-if-not)
23. distinct
24. flatten
25. lmapcat
26. Clojure's [] literal for vector
27. clojure's last behaviour

Short-term todo list:

1. lnth => lazy nth
2. concat
3. any? (strict and lazy)
4. some? (strict and lazy)
5. every?
6. repeat (lazy & strict)
7. cycle (lazy)
8. for

medium-term todo list:

1. literal for hash-map
2. all hash-map functions
3. Clojure's set wont be implemented as CL can treat list as set
4. pmap
5. clojure's state management using atom, refs, and agents.
6. So far, no plan (before I acquire the required skills) to create a generic sequence abstractions nor clojure's vector (tree style)
