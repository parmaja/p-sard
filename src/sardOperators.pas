unit sardOperators;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  sardClasses, sardLexers;

type

  { TOpNone }

  TOpNone = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpAnd }

  TOpAnd = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpOr }

  TOpOr = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpPlus }

  TOpPlus = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpSub }

  TOpSub = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpMultiply }

  TOpMultiply = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpDivide }

  TOpDivide = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpPower }

  TOpPower = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpGreater }

  TOpGreater = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpLesser }

  TOpLesser = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpEqual }

  TOpEqual = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpNotEqual }

  TOpNotEqual = class(TSardOperator)
  public
    constructor Create;
  end;

  { TOpNot }

  TOpNot = class(TSardOperator)
  public
    constructor Create;
  end;


implementation

{ TOpEqual }

constructor TOpEqual.Create;
begin
  Name := '=';
  Title := 'Equal';
  Associative := asLeft;
  Description := '';
end;

{ TOpPower }

constructor TOpPower.Create;
begin
  Name := '^';
  Title := 'Power';
  Associative := asLeft;
  Description := '';
end;

{ TOpGreater }

constructor TOpGreater.Create;
begin
  Name := '>';
  Title := 'Greater';
  Associative := asLeft;
  Description := '';
end;

{ TOpLesser }

constructor TOpLesser.Create;
begin
  Name := '<';
  Title := 'Lesser';
  Associative := asLeft;
  Description := '';
end;

{ TOpNotEqual }

constructor TOpNotEqual.Create;
begin
  Name := '<>';
  Title := 'NotEqual';
  Associative := asLeft;
  Description := 'Check Equal';
end;

{ TOpNot }

constructor TOpNot.Create;
begin
  Name := '!';
  Title := 'not';
  Associative := asLeft;
  Description := 'Not';
end;

{ TOpDivide }

constructor TOpDivide.Create;
begin
  Name := '/';
  Title := 'Divide';
  Associative := asLeft;
  Description := 'Divide object on another object';
end;

{ TOpMultiply }

constructor TOpMultiply.Create;
begin
  Name := '*';
  Title := 'Multiply';
  Associative := asLeft;
  Description := 'Multiply object with another object';
end;

{ TOpSub }

constructor TOpSub.Create;
begin
  Name := '-';
  Title := 'Minus';
  Associative := asLeft;
  Description := 'Sub object from another object';
end;

{ TOpPlus }

constructor TOpPlus.Create;
begin
  Name := '+';
  Title := 'Plus';
  Associative := asLeft;
  Description := 'Add object to another object';
end;

{ TOpOr }

constructor TOpOr.Create;
begin
  Name := '|';
  Title := 'Or';
  Associative := asLeft;
  Description := '';
end;

{ TOpAnd }

constructor TOpAnd.Create;
begin
  Name := '&';
  Title := 'And';
  Associative := asLeft;
  Description := '';
end;


{ TOpNone }

constructor TOpNone.Create;
begin
  Name := '';
  Title := 'None';
  Associative := asLeft;
  Description := 'Nothing';
end;

end.
