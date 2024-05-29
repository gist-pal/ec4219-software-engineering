# EC4219: Software Engineering (소프트웨어공학)
- Instructor: [Sunbeom So](https://gist-pal.github.io)
- Location: EECS B203
- Time: Mon/Wed 13:00-14:30

## About The Course
Software Engineering is an engineering discipline that is concerned with all aspects of SW production process.
In particular, SW validation is highly important in the SW process due to the prevalence of SW errors, comprising more than half of the SW development cost.
Our focus in this course is to study program analysis methods, which are fundamental and modern technologies that can effectively help the SW validation process.

## Setting Up the Programming Environment
**Note1**: This instruction assumes that you are using the Linux command line interface. For Windows users, I recommend using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install). <br>
**Note2**: I checked that the following commands successfully work for the clean docker image ``python:3.9.19-slim``.

### Installing [OCaml](https://ocaml.org/install)
```
$ chmod +x setup/ocaml.sh; ./setup/ocaml.sh
```

### Installing [Z3](https://github.com/Z3Prover/z3)
```
$ eval $(opam env); chmod +x setup/z3.sh; ./setup/z3.sh
```

### Testing
If your installations were successful, you must get appropriate results by executing the following commands.
```
$ cd setup
$ eval $(opam env); chmod +x test_ocaml.sh; ./test_ocaml.sh
$ export PYTHONPATH=$home/mylib:$PYTHONPATH; python3 ex.py
```

## Schedule
|#|Date|Topics|Recommended Reading|
|-|-|------|------|
|0|2/26|[Course Overview](slides/lec0.pdf)||
|1|2/28,3/4|[Introduction to Program Analysis](slides/lec1.pdf)||
|2|3/6|[Fuzzing](slides/lec2.pdf)||
|3|3/11|[Propositional Logic (1)](slides/lec3.pdf)|The Calculus of Computation Ch. 1|
|4|3/13, 3/18|[Propositional Logic (2)](slides/lec4.pdf) |The Calculus of Computation Ch. 1|
|5|3/20, 3/25|[First-Order Logic](slides/lec5.pdf)|The Calculus of Computation Ch. 2|
|6|3/27|[First-Order Theories](slides/lec6.pdf)|The Calculus of Computation Ch. 3|
|7|4/1|[Problem Solving using SMT Solvers](slides/lec7.pdf)| |
|8|4/3|[Functional Programming in OCaml](slides/lec8.pdf)||
|9|4/8|[Problem Solving using SMT Solvers (2)](slides/lec9.pdf)||
|-|4/15|[Mid-term Exam]||
|-|4/22|[Reviewing Mid-term Exam]||
|10|4/24|[Program Verification (1)](slides/lec10.pdf)|The Calculus of Computation Ch. 5|
|11|4/29|[Program Verification (2)](slides/lec11.pdf)|The Calculus of Computation Ch. 5|
|12|5/8|[Program Verification (3)](slides/lec12.pdf)||
|13|5/13|[Program Verification (4)](slides/lec13.pdf)|[Houdini Paper](https://users.soe.ucsc.edu/~cormac/papers/fme01.pdf)|
|14|5/14|[Symbolic Execution](slides/lec14.pdf)|[A Survey of Symbolic Execution Techniques](https://dl.acm.org/doi/10.1145/3182657)|
|15|5/16|Review + Detailed Guidance about hw1||
|16|5/27|[Symbolic Execution (2)](slides/lec15.pdf)||
|16|5/29|[Abstract Interpretation (1)](slides/lec16.pdf)||

## References
I am truly grateful to the authors of the materials from the following courses, which greatly helped to improve the quality of this course.
* [CIS547:Software Analysis](https://software-analysis-class.org) course taught by Prof. Mayur Naik at UPenn
* [AAA615:Formal Methods](https://prl.korea.ac.kr/courses/aaa615/2017) course taught by Prof. Hakjoo Oh at Korea University
* [CS398L:Automated Logical Reasoning](https://www.cs.utexas.edu/~isil/cs389L/) course taught by Prof. Isil Dillig at UT Austin
