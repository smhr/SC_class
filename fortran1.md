# HPC Course

Session 3

S.Mohammad Hoseini-Rad

[smhr.github.io](http://smhr.github.io/)

## MODERN FORTRAN

### References

* [annefou/Fortran](http://annefou.github.io/Fortran/)
* [Introduction to Modern Fortran](http://people.ds.cam.ac.uk/nmm1/Fortran/)
* [Modern Fortran Programming for Chemists and Physicists](http://www.chem.helsinki.fi/~manninen/fortran2014/)
* [Fortran Wiki](http://fortranwiki.org/fortran/show/HomePage)
* [High Performance Scientific Computing](https://class.coursera.org/scicomp-002)

### Fortran history

* Prior to Fortran, programs were often written in machine code or assembly language.

--
* Fortran I: 1954–57, followed by Fortran II, III, IV, Fortran 66.

--
* Major changes in **Fortran 77**, which is still widely used.

--
* Major changes again from Fortran 77 to Fortran 90 (array, generic and modular programming).

--
* Fortran 95: minor changes (high performance computing).

--
* Fortran 2003 (object oriented), 2008 (concurrent).Not fully implemented by most compilers.


### Compiled vs. interpreted language

* interpreter: e.g. Python, Matlab, Mathematica, Octave, ...

    * Takes commands one at a time, converts into machine code, and executes.
    * Allows interactive programming at a shell prompt.
    * Can’t take advantage of optimizing over a entire program — does not know what instructions are coming next.
    * Must translate each command while running the code, possibly many times over in a loop.
    
--
    
* Compiled: e.g. Fortran, c, c++
    
    * The program must be written in 1 or more files (source code).
    * These files are input data for the compiler, which is a computer program that analyzes the source code and converts it into object code.
    

* **Why two steps?**

### Compiling and linking
* GNU fortran, intel fortran, Absoft, NAG, ...

![compiling_and_linking](../images/Compilation.jpg)

---
* To compile:

```bash
gfortran myprog.f90 -o myprog.exe 
```
* Not all modern fortran features are implemented in compilers. Check complying standard with:

* gfortran

```bash
gfortran myprog.f90 -o myprog -std=f95
gfortran myprog.f90 -o myprog -std=f2003
gfortran myprog.f90 -o myprog -std=f2008 
```
* ifort

```bash
ifort myprog.f90 -o myprog -std95
ifort myprog.f90 -o myprog -std03
ifort myprog.f90 -o myprog -std08 
```

See below for complete list:

* [Fortran 2003](http://fortranwiki.org/fortran/show/Fortran+2003+status)
* [Fortran 2008](http://fortranwiki.org/fortran/show/Fortran+2008+status)

### Fortran syntax

* Big differences between Fortran 77 and Fortran 90/95.
* Fortran 77 still widely used:
    * Legacy codes (written long ago, millions of lines...)
    * Faster for some things.
    
--

* One big difference: Fortran 77 (and prior versions) required fixed format of lines:
* Executable statements must start in column 7 or greater, only the first 72 columns are used, the rest ignored!

![fortran](https://upload.wikimedia.org/wikipedia/commons/thumb/5/58/FortranCardPROJ039.agr.jpg/320px-FortranCardPROJ039.agr.jpg)

--

* Fortran > 77, free format
* Indentation is optional **but** highly recommended.
* Fixed format: .f
* Free format: .f90

## Fortran arrays

Arrays in Fortran can be defined in various ways, but **remember** they start from **index 1**.

```fortran
INTEGER, PARAMETER :: M = 50, N = 2000
INTEGER            :: idx(M)
REAL               :: vector(0:N-1)
REAL               :: matrix(M,N)
CHARACTER(LEN=40)  :: screen(200)
```
or

```fortran
INTEGER, PARAMETER                :: M = 50, N = 2000
INTEGER, DIMENSION(M)             :: idx
REAL, DIMENSION(0:N-1)            :: vector
REAL, DIMENSION(M,N)              :: matrix
CHARACTER(LEN=40), DIMENSION(200) :: screen
```

## Fortran arrays

Fortran arrays are very powerful and allows to define:

* matrices
* vectors
* other arrays with up to 7 dimensions.

### Arrays syntax

In older Fortran codes, arrays are usually accessed element by element while in modern Fortran, what is called the Fortran 90 array syntax is used. 

```fortran
vector(:) = vector(:) + 1.0
 
 do j=1,n
    y(:) = y(:) + m(:,J) * x(j)
 enddo
```
--

Do not forget to initialize your arrays!
```fortran
do j=1,10
  vector(j) = 0.0
  idx(j) = j
enddo
```
--

or using an array syntax initialization:
```Fortran
vector = 0
vector(:) = 0
idx(1:10)= 0
idx(0:) = (/ (j, j=0,10)/)
```

### Dynamic memory allocation

If an array size depends on the input of your program, its memory should be *allocated at runtime*.

Two ways for dynamical allocation of memory:

--

* Allocatable array: memory is allocated through the ALLOCATE statement, and freed through DEALLOCATE

```Fortran
INTEGER, DIMENSION(:,:), ALLOCATABLE :: A
```

Then in some part of the code

```Fortran
M = 100; N = 200
ALLOCATE(A(N,M), STAT = err)
IF (err /= 0) THEN 
   PRINT*,'Memory is not accessible.'
   STOP
ENDIF
DEALLOCATE(A)
```
### Dynamic memory allocation

If an array size depends on the input of your program, its memory should be *allocated at runtime*.

Two ways for dynamical allocation of memory:

* Automatic arrays: Local arrays with run-time bounds

Bounds may be taken from an argument Or a constant or variable in a module

```Fortran
SUBROUTINE me (size)
USE sizemod ! This defines worksize
INTEGER                           :: size
REAL , DIMENSION(1:worksize)      :: array1
REAL , DIMENSION(1:size (size+1)) :: array2
```

### Multi-dimensional array storage

* Memory can be thought of linear, indexed by a single address.

--

* A one-dimensional array of length N will generally occupy N consecutive memory locations: 8N bytes for floats.

--

* A two-dimensional array (e.g. matrix) of size m × n will require mn memory locations.

--

* Might be stored by rows, e.g. first row, followed by second row, etc. (C and Python)

--

* Or, could be stored by **columns**, as done in **Fortran**!

### Fortran program units

![procedure](../images/procedure_85p.png)

* Internal: within program structure
* External: independently declared, may be other language
* Module procedure: defined in module

### Procedure types
**Subroutine:**
```fortran
SUBROUTINE mysub (args)
...
END SUBROUTINE mysub
```
To use it: 
```fortran
CALL mysub (args)
```

--

**Example**
```fotran
subroutine simple_function(s,res)
  implicit none
  integer, intent(in) :: s   ! s is an input parameter
                             ! it cannot be modified
  integer,intent(out) :: res ! res is an output parameter

  res = 10*s
end subroutine simple_function
```

---

**Function:**
```fortran
[Type] FUNCTION myfunc (args) [result (args)]
...
END FUNCTION myfunc
```
To use it: 
```fortran
res = myfunc (args)
```

---

**Example**
```fortran
integer function simple_function(s)
  implicit none
  integer, intent(in) :: s  ! s is an input parameter
                            ! it cannot be modified

  simple_function = 10*s
end function simple_function
```
--

The same function with "RESULT": 
```fortran
function simple_function(s) result(res)
  implicit none
  integer, intent(in) :: s  ! s is an input parameter
                            ! it cannot be modified
  integer             :: res ! we define a local variable to
                             ! store our result

  res = 10*s
end function simple_function
```
---

**Module:**
```fortran
MODULE mymmod
...
END MODULE mymod
```
To use it: after program unit name and before any other things
```fortran
USE mymod
```

**Internal and module procedures provide a defined interface, compiler uses this to check arguments**


### Internal procedure

* Each program unit may contain internal procedures
* Declared at the end of program unit after the CONTAINS statement
* Inherits variables and objects from the program unit 


*  **Nested CONTAINS statements are not allowed**

---

```fortran
program main
  implicit none
  integer            :: N, err
  real, allocatable  :: x(:)

  print*, 'Enter an integer N'
  read*, N
  allocate(x(N), STAT=err)
  if (err /= 0 ) STOP
  call random_number(x)

  print*, 'Processing x...', process()
  deallocate(x)
  contains
  logical function process()
! in this function N and X can be accessed directly
! Please not that this method is not recommended:
! it would be better to pass X as an argument of process
    implicit none

    if (sum(x) > 5.) then
       process = .FALSE.
    else
       process = .TRUE.
    endif
  end function process
```

### External procedure

* Declared in separate program units, included with the EXTERNAL keyword
* Modules are much easier
* They are compiled and linked separately
* Library routines are external procedures

--

* ** DO NOT USE THEM**. Modules are much easier and more robust! They are only needed when procedures are written with different programming language or when using external libraries(such as BLAS) 

---
```fotran
program main
  implicit none
  integer            :: N, err
  real, allocatable  :: x(:)
  external           :: subOutside

  print*, 'Enter an integer N'
  read*, N
  allocate(x(N), STAT=err)
  if (err /= 0 ) STOP
  call random_number(x)

  print*, 'Processing x...', process()

  call printX(x)

  call subOutside(x,N)
  deallocate(x)
  contains
```

---

```fortran
  logical function process()
! in this function N and X can be accessed directly
! Please not that this method is not recommended:
! it would be better to pass X as an argument of process
    implicit none

    if (sum(x) > 5.) then
       process = .FALSE.
    else
       process = .TRUE.
    endif 
  end function process
! 
  subroutine printX(x)
    implicit none
    real, intent(in)  :: x(:) ! x is defined as part of main
                              ! so its size and shape is known
    
    integer           :: j 

    print*, 'size of x ', size(x)
    do  j=1,size(x)
      print*, 'x(',j,') = ', x(j)
    enddo
  end subroutine printX
end program main
```
---

And we have another file for instance called external_subroutine.f90:
```fortran
! subOutside is an external procedure
subroutine subOutside(y,M)
  implicit none

  real, intent(in)    :: y(M)
  integer, intent(in) :: M


  integer             :: j

  print*, 'size of y ', size(y)
  do  j=1,size(y)
    print*, 'y(',j,') = ', y(j)
  enddo

! it will not work here:  print*,x
! because x is not visible from subOutside (no interface)
end subroutine subOutside
```

---

** To compile **

```bash
gfortran -c external_subroutine.f90
gfortran visibility_internal_procedure.f90 external_subroutine.o
```
### Interface blocks

A program must ensure that the actual arguments in a reference to a procedure are consistent with the dummy arguments expected by that procedure ==> checked by compiler

* explicit: internal and module procedures
* implicit: external procedures

Where ever possible interfaces should be made explicit. This can be done through the
interface block:

```fortran
INTERFACE
   interface statements
END INTERFACE
```

---
Example:

```fortran
PROGRAM count
INTERFACE
   SUBROUTINE ties(score, nties)
      REAL :: score(50)
      INTEGER :: nties
   END SUBROUTINE ties
END INTERFACE

REAL, DIMENSION(50):: data
...
CALL ties(data, n)
...
END PROGRAM count
```

---

Good practice: Write yourself an INTERFACE block for F77 code.

For instance, you can define an interface for g05faf subroutine (generates a set of random number) of the NAG library: 

```fortran
SUBROUTINE nag_rand(table)
  INTERFACE 
    SUBROUTINE g05faf(a,b,n,x)
	  REAL, INTENT(IN)    :: a, b
	  INTEGER, INTENT(IN) :: n
	  REAL, INTENT(OUT)   :: x(n)
	END SUBROUTINE g05faf
  END INTERFACE
  REAL, DIMENSION(:), INTENT(OUT) :: table
  
  call g05faf(-1.0,-1.0, SIZE(table), table)
END SUBROUTINE nag_rand
```

### Passing procedures as argument

```fortran
program degtest
  implicit none
  intrinsic asin, acos, atan

  write(*,*) 'arcsin(0.5) : ', deg(ASIN, 0.5)
  write(*,*) 'arccos(0.5) : ', deg(ACOS, 0.5)
  write(*,*) 'arctan(1.0) : ', deg(ATAN, 1.0)

CONTAINS
  REAL function deg(f,x)
    implicit none
    
    intrinsic atan
    REAL, EXTERNAL :: f
    REAL, INTENT(IN) :: x

    deg = 45*f(x) / ATAN(1.0)
  end function deg 
end program degtest
```

---

```fortran
PROGRAM test
    INTERFACE
      REAL FUNCTION func( x )
         REAL, INTENT(IN) ::x
      END FUNCTION func
    END INTERFACE
...
    CALL sub1( a, b, func(2) )
...
END PROGRAM test
```
-------- In other file:

```fortran
REAL FUNCTION func( x )!external
    REAL, INTENT(IN) :: x
    func = 1/x
END FUNCTION func
```
or equivalently
```fortran
FUNCTION func( x )
    REAL :: func
    REAL, INTENT(IN) :: x
    func = 1/x
END FUNCTION func
```

###  Passing array arguments
There are two ways to pass arrays to procedures: 
* Explicit shape array (dimensions passed explicitly)

```fortran
subroutine test(N,M,matrix)
  implicit none
  real, dimension(N,M) :: matrix
  ...
end subroutine test
```
* Assumed (implicit) shape array (only possible if interface is visible):

```fortran
subroutine test(matrix)
  implicit none
  real, dimension(:,:) :: matrix
end subroutine test
```

### Possible mistake

```fortran
program main
      real, dimension(5) :: x

      x = 0.
! THIS IS WRONG
      call incb(x)
      print *, x

      end program main

      subroutine incb(a)
! this is a fortran90 style subroutine
      real, dimension(:) :: a
      a = a + 1.
      end subroutine incb
```
The subroutine incb uses a Fortran 90 style assumed shape array (containing dimension(:)). Such routines **must either be in a module**, or **have an explicit interface** wherever they are used.

---

One correct way: use an explicit interface

```fortran
program main
      real, dimension(5) :: x

! THIS IS THE RIGHT WAY
      interface
         subroutine incb(a)
           real, dimension(:) :: a
         end subroutine incb
      end interface

      x = 0.
      call incb(x)
      print *, x

      end program main

      subroutine incb(a)
! this is a fortran90 style subroutine
      real, dimension(:) :: a
      a = a + 1.
      end subroutine incb
```

--

Better way: use module

## Module

Applications:

* global data (instead of common blocks in F77)

* shared declaration (i.e "headers")

* procedure interfaces

* precision definition

* derived type definition

--

One file could have many modules.

Modules could be spread out in different files.

### Global data

```fortran
MODULE global
   REAL, DIMENSION(100) :: a, b, c
   INTEGER :: list(100)
   LOGICAL :: test
END MODULE global
```
--

All variables in the module global can be accessed by a program unit through the statement:
```fortran
USE global
```
or
```fortran
USE global, ONLY: a, c
```
--

To prevent name clashes,
```fortran
USE global, new_name => name_in_module
```

### Module procedures

* Procedures contained within a module become global, i.e. can be referenced from any program unit with the appropriate `USE` statement.

* Unlike external procedures, no need to provide an interface in the referencing program unit

* Provided that module is `use`d, its procedures could be `call`ed.

---

This is that Better way:

```fortran
! THIS IS ANOTHER RIGHT WAY
      module inc
      contains
      subroutine incb(a)
! this is a fortran90 style subroutine
      real, dimension(:) :: a
      a = a + 1.
      end subroutine incb
      end module inc

      program main
      use inc
      real, dimension(5) :: x

      x = 0.
      call incb(x)
      print *, x

      end program main
```

---

Other example:

```fortran
MODULE cartesian
    TYPE point
         REAL :: x, y
    END TYPE point
 CONTAINS
    SUBROUTINE swap( p1, p2 )
         TYPE(point), INTENT(INOUT):: p1
         TYPE(point), INTENT(INOUT):: p2
         TYPE(point) :: tmp
         tmp = p1
         p1 = p2
         p2 = tmp
    END SUBROUTINE swap
END MODULE cartesian
```
```fortran
PROGRAM graph
 USE cartesian
    TYPE(point) :: first, last
    ...
    CALL swap( first, last)
    ...
END PROGRAM graph
```

### PUBLIC and PRIVATE

* By default all entities in a module are accessible to program units with the correct `USE` statement.

```fortran
MODULE one
    PRIVATE            ! set default to private 
    REAL, PUBLIC :: a
    REAL :: b
    PUBLIC :: init_a
 CONTAINS
    SUBROUTINE init_a()
    SUBROUTINE init_b()
    ...
END MODULE one
```

### Overloading operators

* It is possible to extend the meaning of an intrinsic operator to apply to additional data types. This requires an interface block with the form: 

```fortran
INTERFACE OPERATOR (intrinsic_operator)
[interface_body]
END INTERFACE
```
Example:

```fortran
MODULE operator_overloading
 IMPLICIT NONE
 INTERFACE OPERATOR (+)
    MODULE PROCEDURE concat
 END INTERFACE 
 CONTAINS
    FUNCTION concat(cha,chb)
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: cha, chb 
        CHARACTER (LEN=(LEN_TRIM(cha) + LEN_TRIM(chb))) :: concat
        concat = TRIM(cha)//TRIM(chb)
    END FUNCTION concat
END MODULE operator_overloading
```

### Defining your own operators

* Operator must have a . at the beginning and end of the operator name.
* E.g. in the preceding example .plus. could have been defined instead of using `+'.
* The following example shows the definition of an operator .DIST. which calculates the straight line distance between two derived type `points'.

```fortran
MODULE new_operators
 IMPLICIT NONE
 INTERFACE OPERATOR (.PLUS.)
    MODULE PROCEDURE concat
 END INTERFACE
 
 CONTAINS
    FUNCTION concat(cha,chb)
       IMPLICIT NONE
       CHARACTER (LEN=*), INTENT(IN) :: cha, chb 
       CHARACTER (LEN=(LEN_TRIM(cha) + LEN_TRIM(chb))) :: concat
       concat = TRIM(cha)//TRIM(chb)
    END FUNCTION concat
END MODULE new_operators
```
The calling program would contain, (next slide)

---



```fortran
USE new_operators
   character(len=10) :: ch1, ch2
   print*, ch1 .PLUS. ch2
```