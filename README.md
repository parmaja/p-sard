[Abandonded] SARD script language
====================

###abandonded###
This project now abandonded because i ported to D language, here https://github.com/parmaja/sard
I will keep this repo for history issues.

It is an object script language not a programming language, but you can use it as programming language.

The idea behind it, it is not use any of reserved words, only signs, only objects there is no "void", "var", "let" "function" or "procedure" or even "if", "else" or "while".

When finished it it will be clear than we can explain it.

####Compare####

__Like pascal__

  * It is case insensitive
  
  * Declareing after the name
  
  * Assigning ":=" or "=", compare "=", object child "."
  
  * There is no assign as operator

  * Dot as Identifier separator "."

  * Not equal: "<>" or "!="

  * Return value not end the execute of block

```javascript
foo:{
  bar: integer;
  i: integer = 5; //Declare and Assign
  method1:{
    :=i * bar //return value
  }
}
foo.bar := 10;
```

__Like C__

  * Blocks: { }, no more "begin" and "end"

  * Comments: //single line and /* multiline */

  * Not: "!"  or "|"

__Like PHP__

  * Multiline strings "" or ''

__Like its self__

  * No escaping chars inside the string you need to escape it outside it
  
  ```javascript
    s := "text"\n;  (not implemented)
  ```

  * Identifiers can take unicode/utf8 characters, so it will support forign languages

__Not have__

  * There is no "For" "While" "Repeat" or even "If" "Else"
  
  Ops, what a programming language this, it is not programming language, it object script
  
  But:
  
  We can declare internal objects (in FPC source) any kind of object can control the blocks
  so we can do it as internal objects

####Reference####

  https://www.d.umn.edu/~rmaclin/cs5641/Notes/L16_Interpreters.pdf

###Examples###

#####Done:#####
```javascript
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
```
First init of the variable define the type

```javascript
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
```javascript
f:{
    x := 10;
    z: {
      x:=5;
      := x + 5;
    };
    := x + z;

  };

:=f + 10;
```
Declare function/object with parameters
```javascript
foo:(p1, p2) {
  := p1 * p2;
};

x := 10;

:= x + foo(5, 5);
```

Declare a variable type, type now not working but parsed
```javascript
x:integer;

x:integer=10;
```
You can use = as assignment
```javascript
x=10;
```

#####TODO:#####
```javascript
x:=#0; // Boolean values, true and false words are just global variables.
x:=#1;
x:=#fc0f1c; //Color const and operator mix the colors not just add it
x:=0xffec;  //hex integer number like but the style of print it as hex we need to override ToString
x:="foo"\13"bar"; //escape char outside the string
x:="I said:"\""As he said";

//include external file to compile it

`lib/foo.sard`; //need a good trick, but it must take a relative path to the current file

/*
    Preprocessor, it will run in external addon/command.... and return string into it
    similar to <?foo ?> in xml
*/
{?foo
?}

//Run child object
f.b;
~~~

There is no 'if' so we need this, still have no idea how i can do it by scanner
```javascript
(x=10)?{ true bock }:{ false block }
i am thinking about
?(x=10){ true bock }:{ false block }

maybe statment not a blocks

condition statment ? true statment : false statment;

Scope 
./

// -With-
object.{     <-not sure
};
```

####Rules####

There is no special functions objects for compiler/parser.
No special name/char/case for classes.

###Thinking loud###

Array:s

    a := [];

    a := ["x", "y", "z"];
    
    a :[10];

    mayebe manage property as array inside the object like

    a:{
      num=10;
      str="test";
    }

    s := a['num']; <- not sure if is good

New object

    You not need to create object if u declared it based on another object like that

    AnyObject:{
      num = 0;
    }

    AnotherObject:AnyObject; <-this is new object from the first one <-naah not goood

    New sign is ~
    You can create object based on any other object, but it will not copy the values(not sure).

    obj = ~AnyObject; //it is mean obj=new AnyObject

    obj = ~~AnyObject; // new and copy the values

    you can use "with" with it

    AnyObject: {
      num = 0;
    }

    (~AnyObject).{
      num = 10; <- theis a member of the object you can use it
    }


###Compile###

FreePascal 2.6.4

###License###

The SARD script is open-sourced software licensed under the [MIT license](http://opensource.org/licenses/MIT)
