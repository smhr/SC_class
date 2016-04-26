# HPC Course

Session 4

S.Mohammad Hoseini-Rad

[smhr.github.io](http://smhr.github.io/)

## MAKING YOUR PROJECT

### Simple example

```fortran
program demo
    print *, "In main program"
    call sub1()
    call sub2()
end program demo

subroutine sub1()
    print *, "In sub1"
end subroutine sub1

subroutine sub2()
    print *, "In sub2"
end subroutine sub2
```

Let's break it up.


---


```fortran
program demo
    print *, "In main program"
    call sub1()
    call sub2()
end program demo
```
```fortran
subroutine sub1()
    print *, "In sub1"
end subroutine sub1
```
```fortran
subroutine sub2()
    print *, "In sub2"
end subroutine sub2
```

To compile:
```bash
gfortran -c main.f90 sub1.f90 sub2.f90
gfortran main.o sub1.o sub2.o -o main.exe
```

### Basic makefile

Define a rule

```bash
target: dependencies
[tab] system command (action)
```

--


If each "dependencies" are **newer** than "target", make will run the "action".

Each of the dependencies are searched through all the targets available and executed if found.


--


A target can have more than one

To use specific makefile:
```bash
make -f your_makefile
```

The default target is ``all``


### Example1

```make
output.txt: main.exe
	./main.exe > output.txt

main.exe: main.o sub1.o sub2.o
	gfortran main.o sub1.o sub2.o -o main.exe

main.o: main.f90
	gfortran -c main.f90
sub1.o: sub1.f90
	gfortran -c sub1.f90
sub2.o: sub2.f90
	gfortran -c sub2.f90
```

### Macro

In makefile, variables are called **macro**

``Variable=Its_value``


--



To dereference it use:

``$(Variable)`` or ``${Variable}``

If the macro name is a single letter, the parentheses or braces are optional.


--


Special macros:

``$@`` name of the file to be made.

``$?`` names of the changed dependents.

``$<`` name of the prerequisite of the rule (a .f90 file)


### Macro modifiers

We often want to start with the value of a macro and modify it in some manner.

Example: get the list of object files from the SRC macro:

```make
\#using Suffix Replacement within a macro:
\# $(name:string1=string2)

SRCS = main.f90 sub1.f90 sub2.f90
OBJS = $(SRCS:.c=.o)
```

### Inference rules

Compiling .f90 files to .o files is a common task.

We use ``%`` for making an Inference rule.

```make
%.o : %.f90
	$(FC) $(FFLAGS) –c $(SRCS)
```

This rule states that a ``.o`` file can be built from a corresponding ``.f90`` file with the action line ``$(FC) $(FFLAGS) –c $(SRCS)``

### Example 2

In the second version there is a general rule for creating .o files from .f90 files:

```make
output.txt: main.exe
	./main.exe > output.txt

main.exe: main.o sub1.o sub2.o
	gfortran main.o sub1.o sub2.o -o main.exe

%.o : %.f90
	gfortran -c $<
```

### Example 3

In the third version we define a macro ``OBJECTS`` so we only have to write out this list once.

```make
OBJECTS = main.o sub1.o sub2.o

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	gfortran $(OBJECTS) -o main.exe

%.o : %.f90
	gfortran -c $< 
```

### Example 4

In the fourth version, we add a Fortran compile flag (for level 3 optimization) and an linker flag (blank in this example):

```make
FC = gfortran    
FFLAGS = -O3
LFLAGS =
OBJECTS = main.o sub1.o sub2.o

.PHONY: clean

output.txt: main.exe
	./main.exe > output.txt

main.exe: $(OBJECTS)
	$(FC) $(LFLAGS) $(OBJECTS) -o main.exe

%.o : %.f90
	$(FC) $(FFLAGS) -c $< 
clean:
	rm -f $(OBJECTS) main.exe
```

### Conditional directives

```make
ifeq (arg1, arg2)
ifeq 'arg1' 'arg2'
ifeq "arg1" "arg2"
ifeq "arg1" 'arg2'
ifeq 'arg1' "arg2" 
```

```make
ifneq (arg1, arg2)
ifneq 'arg1' 'arg2'
ifneq "arg1" "arg2"
ifneq "arg1" 'arg2'
ifneq 'arg1' "arg2"
```

### Example

```make
libs_for_gcc = -lgnu
normal_libs =

foo: $(objects)
ifeq ($(CC),gcc)
        $(CC) -o foo $(objects) $(libs_for_gcc)
else
        $(CC) -o foo $(objects) $(normal_libs)
endif
```