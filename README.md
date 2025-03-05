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

### Installation Verification
For any OS, if the installation was successful, you should see the message ``The OCaml toplevel, version 5.1.1``
by running the command ``ocaml --version`` in the terminal.
```
$ ocaml --version
The OCaml toplevel, version 5.1.1
```

## Schedule
|#|Date|Topics|Recommended Reading|
|-|-|------|------|
|0|3/5|[Course Overview](slides/lec0.pdf)||

## References & Acknowledgements
My lecture slides are based on those from the following courses.
I sincerely appreciate their authors for providing materials that have greatly enhanced the quality of this course.

* [AAA615:Formal Methods](https://prl.korea.ac.kr/courses/aaa615/2017) course taught by Prof. Hakjoo Oh at Korea University
* [CS398L:Automated Logical Reasoning](https://www.cs.utexas.edu/~isil/cs389L/) course taught by Prof. Isil Dillig at UT Austin
* [CIS547:Software Analysis](https://software-analysis-class.org) course taught by Prof. Mayur Naik at UPenn
