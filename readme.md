# BIGINT Signature
type bigint

bigint is of type (int list * bool), if bool is true then it represents a positive integer else negative. 

Here, int list is a list of digits of the integer in big endian form.

Representation of zero : if one gives ([0,0,0..],) false then autoconvert to ([0],true) so that there is unique "0" with sign "true"

# RATIONAL Signature
type rational

rational is of type bigint*bigint, to implement all functions related to rational, I the numerator can be positive or negative but the denominator is always positive

Representation of zero : (([0],true),([1],true))

# GRAMMAR of Rational numbers

regular expression of decimal is given by [S]I.N(R)

this is the same as used in calc.lex file. that is -
    [~]?{digit}*"."{digit}*"("{digit}+")" 

B = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, .,(,), +, ~} is the alphabet.

I is a possibly empty integer part defined by “d*”

N is defined by "d*" is non-recurring fractional part

R is defined by "d+" is the recurring fractional part enclosed in left and right parenthesis



The grammar for rational numbers is as follow - 

    P -> I.N | A/D | A
    I -> S|SI0|SI1|SI2|SI3|SI4|SI5|SI6|SI7|SI8|SI9
    S -> eps|+|~
    N -> N(R)|0N(R)|1N(R)|2N(R)|3N(R)|4N(R)|5N(R)|6N(R)|7N(R)|8N(R)|9N(R)|(R)
    R -> 0R|1R|2R|3R|4R|5R|6R|7R|8R|9R|0|1|2|3|4|5|6|7|8|9

    A -> ~M|M
    M -> M0|M1|M2|M3|M4|M5|M6|M7|M8|M9|0|1|2|3|4|5|6|7|8|9
    D -> D0|D1|D2|D3|D4|D5|D6|D7|D8|D9|1|2|3|4|5|6|7|8|9

P is start symbol

I symbol "represents" integer part

A "represents" positive or negative integers

D "represents" positive integers (denominator)

N represents non-recurring part

R represents recurring part

# GRAMMAR of Rational number expressions

This is defined in the calc.grm file also (program incorporates the rules of precedence on its own)

    E -> P  
    E -> E+E
    E -> E*E
    E -> E/E
    E -> E-E
    E -> (E)

Here is the grammar which incorporates precedence explicitly (left associative)

    E → E+T | E-T | T 
    T → T*F | T/F | F
    F → P | (E)

where P symbol represents rational number (grammar rules defined previously) and E represents the start state/expression

C = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, .,(,), +, ~, *, /, -} is the alphabet.

# ACKNOWLEDGEMENTS

SMLNJ Official documentation and the example files given in the program files in linux (/ml-yacc/examples) and SAK slides

The code for calc.lex, calc.grm, calc.sml (gluefile) and sources.cm is tweeked with little changes for making the parser for this assignment