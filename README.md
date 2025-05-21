# EC4219: Software Engineering (소프트웨어공학)
- Instructor: [Sunbeom So](https://gist-pal.github.io)
- Location: EECS B201
- Time: Mon/Wed 16:00-17:30

## About The Course
Software Engineering is an engineering discipline that is concerned with all aspects of SW production process.
In particular, SW validation is highly important in the SW process due to the prevalence of SW errors, comprising more than half of the SW development cost.
Our focus in this course is to study program analysis methods, which are fundamental and modern technologies that can effectively help the SW validation process.

## Configuring the Programming Environment
We will use [OCaml](https://ocaml.org/install) programming language for our programming exercises. This instruction assumes that you are using the Linux command line interface.

### macOS
To install OCaml, simply copy and run the following commands in the terminal.
```
$ chmod +x setup/install_ocaml_mac.sh; ./setup/install_ocaml_mac.sh; eval $(opam env)
```

### Ubuntu
To install OCaml, simply copy and run the following commands in the terminal. I checked that the following commands successfully work for the clean docker image ``python:3.9.19-slim``.
```
$ chmod +x setup/install_ocaml_ubuntu.sh; ./setup/install_ocaml_ubuntu.sh; eval $(opam env)
```

### Windows
For Windows users, I recommend using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install).
  * [WSL setup](setup/wsl-install-guideline.pdf): We provide a PDF guideline that explains how to install WSL and how to share files between the host OS and the guest OS.
  * [script](setup/wsl_install.ps1): For running this installation script, please refer to p.02 of the above PDF guideline.
    
After setting up WSL, simply copy and run the following commands in the terminal to install OCaml.
```
$ chmod +x setup/install_ocaml_ubuntu.sh; ./setup/install_ocaml_ubuntu.sh; eval $(opam env)
```

### Installing Z3
To install Z3, simply copy and run the following commands in the terminal.
```
$ chmod +x setup/install_z3.sh; ./setup/install_z3.sh
```

### Installation Verification
```
$ ocaml --version
The OCaml toplevel, version 5.1.1
$ z3 --version
Z3 version 4.13.0 - 64 bit
```


## Textbook
* [The Calculus of Computation](https://link.springer.com/book/10.1007/978-3-540-74113-8) (COC)
* [Semantics with Applications: An Appetizer](https://link.springer.com/book/10.1007/978-1-84628-692-6) (SAA)

## Schedule
|#|Date|Topics|Recommended Reading|
|-|-|------|------|
|0|3/5|[Course Overview](slides/lec0.pdf)||
|1|3/10|[Propositional Logic (1)](slides/lec1.pdf)| COC Ch.1.1-1.5|
|2|3/17, 3/19|[Propositional Logic (1)](slides/lec2.pdf)| COC Ch.1.6-1.7|
|3|3/24, 3/27, 3/31|[First-Order Logic](slides/lec3.pdf)| COC Ch.2|
|4|4/2, 4/7|[First-Order Theories](slides/lec4.pdf)| COC Ch.3|
|-|4/7|[Undecidability, Halting Problem](slides/lec-halting.pdf)| |
|5|4/9|[Problem Solving using SMT Solvers](slides/lec5.pdf)| |
|-|4/14|[Reviewing Past Exam Questions]| |
|-|4/21|[Mid-term Exam]| |
|-|4/28|[Functional Programming in OCaml](slides/lec-ocaml.pdf)||
|6|4/30, 5/7|[Program Verification (1): Specification](slides/lec6.pdf)|COC Ch.5.1|
|7|5/7, 5/12, 5/14|[Program Verification (2): Inductive Assertion Method](slides/lec7.pdf)|COC Ch.5.2|
|8|5/19|[Program Verification (3): Inductive Assertion Method](slides/lec8.pdf)||
|9|5/21|[Program Verification (4): Invariant Inference](slides/lec9.pdf)|[Houdini Paper](https://users.soe.ucsc.edu/~cormac/papers/fme01.pdf)|


## Academic Integrity
By registering for this course, I will assume you agree with the policy below.
* All assignments (i.e., writing code) must be your own work. No discussions are allowed.
  * You should not share/show your code.
  * You should not post your code on public websites.
  * You should not modify other students’ code.
* I will have a one-on-one meeting with each student under suspicion.
If you fail to prove your integrity in the meeting (e.g., failing to answer my questions about the details of your submissions), I will consider that you committed academic misconduct, even if you do not admit to your cheating.
* Cheating on assignments will result in an F.


## References & Acknowledgements
My lecture slides are based on those from the following courses.
I sincerely appreciate their authors for providing materials that have greatly enhanced the quality of this course.

* [AAA615:Formal Methods](https://prl.korea.ac.kr/courses/aaa615/2017) course taught by Prof. Hakjoo Oh at Korea University
* [CS398L:Automated Logical Reasoning](https://www.cs.utexas.edu/~isil/cs389L/) course taught by Prof. Isil Dillig at UT Austin
* [CIS547:Software Analysis](https://software-analysis-class.org) course taught by Prof. Mayur Naik at UPenn
