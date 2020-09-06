Sard Script
===========

It is an object script language not a programming language, but you can use it as programming language.

But "A new language should bring something new to the table.", Until now it is just training to build language or teaching my self D language.

The idea behind it, it is not use any of reserved words it is kind of challenge, only signs (symbols), only objects there is no "void", "var", "let" "function" or "procedure" or even "if", "else" or "while".
But we can implement "if" or "while" internally by define it as internal object (not yet).

This project ported to D from my old project writen in Pascal language, and I will continue it with D.

https://github.com/parmaja/fpc-sard/

Specification
-------------

  * It is case insensitive.
  * Declaration is after the id name.
  * Assigning ":=" , compare "=", object child pattren "."
  * There is no assign as operator
  * Dot as Identifier separator "."
  * Not equal: "<>" or "!="
  * Return value not end the execute of block
  * Blocks: { }, or predefined it (not yet)
  * Comments: //single line and /* multiline */  * 
  * Multiline strings "" or ''
  * Identifiers can take unicode/utf8 characters, so it will support any forign language
  * Blocks have return values
  * Functions is objects, or Object can take arguments and return a value.
  * When execute object we can pass arguments (), array [] and blocks {}{}{} //not yet
  * There is no "For". "While", "Repeat" or even "If" "Else", those are an objects //not yet
  * No global, but object functions is global for child objects
  
##### Done: #####

```D
/*
  This examples worked, and this comment will ignored, not compiled or parsed as we say.
*/

x := 10 + 5 - (5 * 5); //Single Line comment

x := x + 10; //Using same variable, until now local variable implemented
x := {    //Block it any where
      y := 0;
      := y + 5; //this is a result return of the block
  }

{* This a block comment, compiled, useful for documentation, or regenrate the code *}

:= x; //Return result to the main object
```
First init of the variable define the type

```D
s:='Foo';
s:=s+' Bar';
:=s; //It will retrun 'Foo Bar';

i := 10;
i := i + 5.5;
//variable i now have 15 not 15.5

i := 10.0;
i := i + 5.5;
//variable i now have 15.5
```

Next f is a function or let us say it is an object we can run it.

```D
f:{
    x := 10;
    z: {
      x:=5;
      := x + 5;
    }
    := x + z;
  }

:=f + 10;
```

Declare function/object with parameters

```D
foo:(p1, p2) {
  := p1 * p2;
}

x := 10;

:= x + foo(5, 5);
```

##### TODO: #####

```D
x := #0; // Boolean values, true and false words are just global variables.
x := #1;
x := #fc0f1c; //Color const and operator mix the colors not just add it
x := 0xffec;  //hex integer number like but the style of print it as hex we need to override ToString
x := "foo"\n\r"bar"; //escape char outside the string
x := "I said:"\""As he said";
```

/*
    Preprocessor, it will run in external addon/command.... and return string into it
    similar to <?foo ?> in xml
*/
```D
{?foo
?}
```

Pattren of object f.b;
```D
object.{     <-not sure
}
```

#### Rules ####


-	Do not use $ or % sign any where, I reserved it for special financial operating, btw i am an accountant.
- No escapes inside the string/text see todo, escape is outside | x := "foo"\13"bar"\n; 
- There is no special functions objects for compiler/parser.
- No special name/char/case for classes.
- Keep it visual, i mean, you can convert it to uml diagram and vise versa, in fact my target to convert it to Scratch similar visual programming, so any complex parsing will break the rules.

### Thinking loud ###

Arrays:

```D
    a := [];

    a := ["x", "y", "z"];
    
    a :[10];
```
    mayebe manage property as array inside the object like

```D
    a:{
      num=10;
      str="test";
    }

    s := a['num']; <- not sure if is good
```

Arrays, If initial parse [] as an index and passit to executer or assigner, not initial,
it is be a list of statments then run it in runtime and save the results in the list in TsoArray

Optional open source code with <?sard ?> like php

Preprocessor: When {?somthing it will passed to addon in engine to return the result to rescan it or replace it with this preprocessor

What about private, public or protected, the default must be protected

    x:(p1, p2){ block } //protected
    x:-(){} //private
    x:+(){} //public

We need to add multi blocks to the identifier like this

    x(10,10){ ... } { ... }

or with : as seperator

    x(10,10){ ... }:{ ... }

it is good to make the "if" object with "else" as the second block.

New object

You not need to create object if u declared it based on another object like that

    AnyObject:{
      num = 0;
    }

    AnotherObject:AnyObject; <-this is new object from the first one <-naah not goood

    New sign is ~

You can create object based on any other object, but it will not copy the values(not sure).

    obj := ~AnyObject; //it is mean obj=new AnyObject

    obj := ~~AnyObject; // new and copy the values

    you can use "with" with it

    AnyObject: {
      num = 0;
    }

    (~AnyObject).{
      num = 10; <- this a member of the object you can use it
    }


License
=======

The SARD script is open-sourced software licensed under the [MIT license](http://opensource.org/licenses/MIT)

### Articles ###

This articles i want to read

http://blogs.msdn.com/b/abhinaba/archive/2009/01/25/back-to-basic-series-on-dynamic-memory-management.aspx
