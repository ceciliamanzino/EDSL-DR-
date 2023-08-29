# A Haskell-embedded DSL for Secure Information-flow
## 

This domain specific language embedded in Haskell is used for enforcing the information flow policy Delimited
Release to programs.

## Instalation

The EDSL implementation is located in the DR folder, to use it import the modules: DR.AbstractSyntax, 
DR.Constructors, DR.Environment and DR.Interpreter.


## Modules Content

* **DR.AbstractSyntax**: contain the abstract syntax of the security language. 


* **DR.Constructors**: contain the smart constructors of the EDSL

* **DR.Environment**: contain the data type definition for security environments of variables and an example of how to build and environment using this data type.   


* **DR.Interpreter**: it define an interpreter for the EDSL. 

## Examples

Some examples given in the literature were implemented using this EDSL and can be found here: Average salary, Electronic wallet and Password-checking. 




