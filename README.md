SARD script language
====================

It is a script language not programming language.

The idea behind it, it is not use any of reserved word only sign, only objects there is no "void" or "function" or "procedure"

When i finish it it will be clear than i can explain it.

Examples
========

    /*
      This examples are worked, and this comment will ignored, not compiled or parsed as we say.
    */
    x := 10 +5 - (5 * 5); //Single Line comment

    x := x + 10; //Using same variable, until now local variable implemented
    x := {    //Block it any where
          y := 0;
          := y + 5; //this is a result return of the block
      }; //do not forget to add ; here
    {* This a block comment, compiled, useful for documentation, or regenrate the code *};
    := x; //Return result to the main object

    s:='Foo';
    s:=s+' Bar';
    :=s; //It will retrun 'Foo Bar';

    i := 10;
    i := i + 5.5;
    //variable i now have 15 not 15.5

    i := 10.0;
    i := i + 5.5;
    //variable i now have 15.5

    {* First init of the variable define the type *}

Still working on it.

Compile
=======

FreePascal 2.6.4

I need contributors to port it to Java or C, it is easy to port becuase i still not use any special features of FPC.
