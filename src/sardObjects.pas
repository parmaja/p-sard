unit sardObjects;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}
{$INTERFACES CORBA}

(*

  o1: {
    o2: {
    }
    fn1;
    fn2;
  }

 o1;
 fn1;
 fn2;
 o1.o2;

 o3 := 10+5;
*)

interface

uses
  Classes, SysUtils, sardClasses;

type

  TsrdoObjectType = (otUnkown, otInteger, otFloat, otBoolean, otString, otObject, otClass);
  TsrdoCompare = (cmpLess, cmpEqual, cmpGreater);

  TsrdoDebug = class(TsardObject)
  public
    Line: Integer;
    Column: Integer;
    FileName: string;
    BreakPoint: Boolean; //not sure do not laugh
  end;

  TsrdoObject = class;
  TsrdoObjectClass = class of TsrdoObject;
  TsrdoOperator = class;

  { TsrdoResult }

  TsrdoResult = class(TsardObject)
    AnObject: TsrdoObject;
  end;

  { TsrdoStatementItem }

  TsrdoStatementItem = class(TsardObject)
  public
    AnOperator: TsrdoOperator;
    AnObject: TsrdoObject;
    function Execute(var vResult: TsrdoResult): Boolean;
  end;

  { TsrdoStatement }

  TsrdoStatement = class(TsardObjectList)
  private
    FDebug: TsrdoDebug;
    function GetItem(Index: Integer): TsrdoStatementItem;
    procedure SetDebug(AValue: TsrdoDebug);
  public
    function Add(AObject: TsrdoStatementItem): Integer;
    function Add(AOperator:TsrdoOperator; AObject:TsrdoObject): TsrdoStatementItem;
    function Execute(var vResult: TsrdoResult): Boolean;
    property Items[Index: Integer]: TsrdoStatementItem read GetItem; default;
    property Debug: TsrdoDebug read FDebug write SetDebug; //<-- Nil until we compile it with Debug Info
  end;

  { TsrdoStatements }

  TsrdoStatements = class(TsardObjectList)
  private
    function GetStatement: TsrdoStatement;
    function GetItem(Index: Integer): TsrdoStatement;
  public
    function Add(AStatement: TsrdoStatement): Integer;
    function New: TsrdoStatement;
    procedure Check; //Check if empty then create first statement
    property Items[Index: Integer]: TsrdoStatement read GetItem; default;
    property Statement: TsrdoStatement read GetStatement;
    function Execute(var vResult: TsrdoResult): Boolean;
  end;

  { TStatementsStack }

  TStatementsStack = class(TsardStack)
  private
    function GetCurrent: TsrdoStatements;
  public
    property Current: TsrdoStatements read GetCurrent;
  end;

  IsrdoObject = interface['{9FD9AEE0-507E-4EEA-88A8-AE686E6A1D98}']
  end;

  IsrdoOperate = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Add(var vResult: TsrdoResult): Boolean;
    function Sub(var vResult: TsrdoResult): Boolean;
    function Mulpiply(var vResult: TsrdoResult): Boolean;
    function Divide(var vResult: TsrdoResult): Boolean;
  end;

  IsrdoCompare = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Compare(var vResult: TsrdoResult): TsrdoCompare;
  end;

  IsrdoBlock = interface['{CB4C0FA1-E233-431E-8CC2-3755F62D93F2}']
    function Execute(var vResult: TsrdoResult): Boolean;
  end;
  { TsrdoObject }

  TsrdoObject = class(TsardObject, IsrdoObject)
  private
    FName: string; //if name is '' then the object cant change the value
    procedure SetName(AValue: string);
  protected
    FObjectType: TsrdoObjectType;
    procedure Created; virtual;
  public
    constructor Create; virtual;
    procedure AfterConstruction; override;
    function This: TsrdoObject; //return the same object, stupid but save some code :P
    function Execute(var vResult: TsrdoResult): Boolean; virtual;
    function Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean; virtual;
    property Name: string read FName write SetName;
    procedure Assign(FromObject: TsrdoObject); virtual;
    function Clone: TsrdoObject; virtual;
    property ObjectType: TsrdoObjectType read FObjectType;
    function AsString: String;
    function AsFloat: Float;
    function AsInteger: int;
    function AsBoolean: Boolean;
    function ToBoolean(out outValue: Boolean): Boolean; virtual;
    function ToString(out outValue: string): Boolean; virtual; reintroduce;
    function ToFloat(out outValue: Float): Boolean; virtual;
    function ToInteger(out outValue: int): Boolean; virtual;
  end;

  { TsrdoObjects }

  TsrdoObjects = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdoObject;
  public
    property Items[Index: Integer]: TsrdoObject read GetItem; default;
  end;

  { TsrdoClass }

  TsrdoClass = class(TsrdoObject)
  public
    procedure Created; override;
  end;

  { TsrdoInstance }

  TsrdoInstance = class(TsrdoObject)
  public
    Value: TsrdoObject;
    procedure Created; override;
  end;

  TsrdoBlock = class(TsrdoObject, IsrdoBlock)
  protected
    FStatements: TsrdoStatements;
    procedure Created; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute(var vResult: TsrdoResult): Boolean; override;
    property Statements: TsrdoStatements read FStatements;
  end;

(**** Common Objects *****)

  { TsrdoNone }

  TsrdoNone = class(TsrdoObject) //None it is not Null, it is an initial value we sart it
  public
    //Do operator
    //Convert to 0 or ''
  end;

  { TsrdoNumber }

  TsrdoNumber = class(TsrdoObject) //abstract
  public
    //Assign
    //Do operator
  end;

  { TsrdoInteger }

  TsrdoInteger = class(TsrdoNumber)
  public
    Value: int;
    procedure Created; override;
    procedure Assign(FromObject: TsrdoObject); override;
    function Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdoFloat }

  TsrdoFloat = class(TsrdoNumber)
  public
    Value: Float;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdoBoolean }

  TsrdoBoolean = class(TsrdoNumber)
  public
    Value: Boolean;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdoString }

  TsrdoString = class(TsrdoObject)
  public
    Value: string;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

{* Run Time Engine *}

  { TsrdoRun }

  TsrdoRun = class(TsardObject)
  public
    Stack: TsardStack;
    constructor Create;
    destructor Destroy; override;
    function Execute(vStatements: TsrdoStatements):Boolean;
  end;


  { TsrdoOperator }

  TsrdoOperator = class(TsardObject)
  protected
    function DoOperate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean; virtual;
  public
    Code: string;
    Name: string;
    Level: Integer;
    Description: string;
    function Operate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean;
    constructor Create; virtual;
  end;

  TsrdoOperatorClass = class of TsrdoOperator;
  { TsrdoOperators }

  TsrdoOperators = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdoOperator;
  protected
    function CheckBeforeRegister(AOperator: TsrdoOperator): Boolean; virtual;
  public
    function Find(const Code: string): TsrdoOperator;
    function FindByName(const vName: string): TsrdoOperator;
    function RegisterOperator(AOperator: TsrdoOperator): Boolean;
    function RegisterOperator(AOperatorClass: TsrdoOperatorClass): Boolean;
    property Items[Index: Integer]: TsrdoOperator read GetItem; default;
  end;

  { TopPlus }

  TopPlus = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopMinus }

  TopMinus = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopMultiply }

  TopMultiply = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopDivide }

  TopDivide = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopPower }

  TopPower = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopLesser }

  TopLesser = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopGreater }

  TopGreater = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopEqual }

  TopEqual = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopNotEqual }

  TopNot = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  TopNotEqual = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopAnd }

  TopAnd = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TopOr }

  TopOr = class(TsrdoOperator)
  public
    constructor Create; override;
  end;

  { TsrdoEngine }

  TsrdoEngine = class(TsardCustomEngine)
  protected
    FOperators: TsrdoOperators;
    procedure Created; override;
    function CreateOperators: TsrdoOperators;
  public
    constructor Create;
    function IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsControl(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsOperator(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsNumber(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsIdentifier(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;

    property Operators: TsrdoOperators read FOperators;
  end;

function sardEngine: TsrdoEngine;

implementation

uses
  sardScripts;

var
  FsardEngine: TsrdoEngine = nil;

function sardEngine: TsrdoEngine;
begin
  if FsardEngine = nil then
    FsardEngine := TsrdoEngine.Create;
  Result := FsardEngine;
end;

{ TStatementsStack }

function TStatementsStack.GetCurrent: TsrdoStatements;
begin
  Result := (inherited GetCurrent) as TsrdoStatements;
end;

{ TopNot }

constructor TopNot.Create;
begin
  inherited Create;
  Code := '!'; //or '~'
  Name := 'Not';
  Level := 100;
end;

{ TopOr }

constructor TopOr.Create;
begin
  inherited Create;
  Code := '|';
  Name := 'Or';
  Level := 51;
end;

{ TopAnd }

constructor TopAnd.Create;
begin
  inherited Create;
  Code := '&';
  Name := 'And';
  Level := 51;
end;

{ TopNotEqual }

constructor TopNotEqual.Create;
begin
  inherited Create;
  Code := '<>';
  Name := 'NotEqual';
  Level := 51;
end;

{ TopEqual }

constructor TopEqual.Create;
begin
  inherited Create;
  Code := '=';
  Name := 'Equal';
  Level := 51;
end;

{ TopGreater }

constructor TopGreater.Create;
begin
  inherited Create;
  Code := '>';
  Name := 'Greater';
  Level := 51;
end;

{ TopLesser }

constructor TopLesser.Create;
begin
  inherited Create;
  Code := '<';
  Name := 'Lesser';
  Level := 51;
end;

{ TopPower }

constructor TopPower.Create;
begin
  inherited Create;
  Code := '^';
  Name := 'Power';
  Level := 52;
end;

{ TsrdoBlock }

procedure TsrdoBlock.Created;
begin
  inherited Created;
end;

constructor TsrdoBlock.Create;
begin
  inherited Create;
  FStatements := TsrdoStatements.Create;
end;

destructor TsrdoBlock.Destroy;
begin
  FreeAndNil(FStatements);
  inherited Destroy;
end;

function TsrdoBlock.Execute(var vResult: TsrdoResult): Boolean;
begin
  Result := Statements.Execute(vResult);
end;

{ TopDivide }

constructor TopDivide.Create;
begin
  inherited Create;
  Code := '/';
  Name := 'Divition';
  Level := 51;
end;

{ TopMultiply }

constructor TopMultiply.Create;
begin
  inherited Create;
  Code := '*';
  Name := 'Multiply';
  Level := 51;
end;

{ TopMinus }

constructor TopMinus.Create;
begin
  inherited Create;
  Code := '-';
  Name := 'Minus';
  Level := 50;
  Description := 'Sub object to another object';
end;

{ TsrdoOperator }

function TsrdoOperator.DoOperate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean;
begin
  Result := False;
end;

function TsrdoOperator.Operate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean;
begin
  if vResult.AnObject = nil then
  begin
    vResult.AnObject := vObject.Clone;
    Result := True;
  end
  else
  begin
    if Self = nil then
      raise EsardException.Create('Operator not defined!');
    Result := vObject.Operate(vResult, Self);
    if not Result then
    begin
      DoOperate(vResult, vObject);
      //Ok let me do it. : the TopPlus said
    end;
  end;
end;

constructor TsrdoOperator.Create;
begin
  inherited Create;
end;

function TsrdoOperators.Find(const Code: string): TsrdoOperator;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Code = Items[i].Code then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsrdoOperators.FindByName(const vName: string): TsrdoOperator;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if vName = Items[i].Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsrdoOperators.RegisterOperator(AOperator: TsrdoOperator): Boolean;
begin
  Result := CheckBeforeRegister(AOperator);
  if Result then
    Add(AOperator);
end;

function TsrdoOperators.RegisterOperator(AOperatorClass: TsrdoOperatorClass): Boolean;
begin
  Result := RegisterOperator(AOperatorClass.Create);
end;

{ TopPlus }

constructor TopPlus.Create;
begin
  inherited Create;
  Code := '+';
  Name := 'Plus';
  Level := 50;
  Description := 'Add object to another object';
end;

{ TsrdoOperators }

function TsrdoOperators.GetItem(Index: Integer): TsrdoOperator;
begin
  Result := inherited Items[Index] as TsrdoOperator;
end;

function TsrdoOperators.CheckBeforeRegister(AOperator: TsrdoOperator): Boolean;
begin
  Result := True;
end;

{ TsrdoEngine }

procedure TsrdoEngine.Created;
begin
  inherited;
  with Operators do
  begin
    Operators.RegisterOperator(TopPlus);
    Operators.RegisterOperator(TopMinus);
    Operators.RegisterOperator(TopMultiply);
    Operators.RegisterOperator(TopDivide);

    Operators.RegisterOperator(TopEqual);
    Operators.RegisterOperator(TopNotEqual);
    Operators.RegisterOperator(TopAnd);
    Operators.RegisterOperator(TopOr);
    Operators.RegisterOperator(TopNot);

    Operators.RegisterOperator(TopGreater);
    Operators.RegisterOperator(TopLesser);

    Operators.RegisterOperator(TopPower);
  end;
end;

function TsrdoEngine.CreateOperators: TsrdoOperators;
begin
  Result := TsrdoOperators.Create;
end;

constructor TsrdoEngine.Create;
begin
  inherited Create;
  FOperators := CreateOperators;
end;

function TsrdoEngine.IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := vChar in sWhitespace;
end;

function TsrdoEngine.IsControl(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in aControlsOpenChars
  else
    Result := vChar in sControlsChars;
end;

function TsrdoEngine.IsOperator(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  //if vOpen then
    Result := vChar in sOperatorOpenChars
//  else
//    Result := not IsWhiteSpace(vChar, False) and not IsNumber(vChar, False) and not IsControl(vChar, False); //Can be any thing except those
end;

function TsrdoEngine.IsNumber(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in sNumberOpenChars
  else
    Result := vChar in sNumberChars;
end;

function TsrdoEngine.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := inherited IsIdentifier(vChar, vOpen);
end;


{ TsrdoStatementItem }

function TsrdoStatementItem.Execute(var vResult: TsrdoResult): Boolean;
begin
  if AnObject = nil then
    raise EsardException.Create('Object not set!');
  {if AnOperator = nil then
    raise EsardException.Create('Object not set!');}
  AnObject.Execute(vResult);
  Result := AnOperator.Operate(vResult, AnObject);//Even AnOperator is nil it will work
end;

{ TsrdoClass }

procedure TsrdoClass.Created;
begin
  inherited Created;
  FObjectType := otClass;
end;

{ TsrdoInstance }

procedure TsrdoInstance.Created;
begin
  inherited Created;
  FObjectType := otObject;
end;

{ TsrdoBoolean }

procedure TsrdoBoolean.Created;
begin
  inherited Created;
  FObjectType := otBoolean;
end;

function TsrdoBoolean.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsrdoBoolean.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsrdoBoolean.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsrdoBoolean.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value;
  Result := True;
end;

{ TsrdoString }

procedure TsrdoString.Created;
begin
  inherited Created;
  FObjectType := otString;
end;

function TsrdoString.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsrdoString.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsrdoString.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsrdoString.ToBoolean(out outValue: Boolean): Boolean;
begin
  if not TryStrToBool(Value, outValue) then
    outValue := AsInteger <> 0;
  Result := True;
end;

{ TsrdoFloat }

procedure TsrdoFloat.Created;
begin
  inherited Created;
  FObjectType := otFloat;
end;

function TsrdoFloat.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsrdoFloat.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsrdoFloat.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsrdoFloat.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsrdoInteger }

procedure TsrdoInteger.Created;
begin
  inherited Created;
  FObjectType := otInteger;
end;

procedure TsrdoInteger.Assign(FromObject: TsrdoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsrdoInteger then
      Value := (FromObject as TsrdoInteger).Value
    else
      Value := FromObject.AsInteger;
  end;
end;

function TsrdoInteger.Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean;
begin
  Result := False;
  if vResult.AnObject = nil then
  begin
    vResult.AnObject := Clone;
    Result := True;
  end
  else if vResult.AnObject is TsrdoInteger then
  begin
    Result := True;
    case AnOperator.Code of
      '+': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value + Value;
      '-': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value - Value;
      '*': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value * Value;
      '/': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value div Value;
      else
        Result := False;
    end;
  end;
end;

function TsrdoInteger.ToString(out outValue: string): Boolean;
begin
  Result := True;
  outValue := IntToStr(Value);
end;

function TsrdoInteger.ToFloat(out outValue: Float): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsrdoInteger.ToInteger(out outValue: int): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsrdoInteger.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsrdoStatements }

function TsrdoStatements.GetStatement: TsrdoStatement;
begin
  Check;//TODO: not sure
  Result := Last as TsrdoStatement;
end;

function TsrdoStatements.GetItem(Index: Integer): TsrdoStatement;
begin
  Result := inherited Items[Index] as TsrdoStatement;
end;

function TsrdoStatements.Add(AStatement: TsrdoStatement): Integer;
begin
  Result := inherited Add(AStatement);
end;

function TsrdoStatements.New: TsrdoStatement;
begin
  Result := TsrdoStatement.Create;
  Add(Result);
end;

procedure TsrdoStatements.Check;
begin
  if Count = 0  then
    New;
end;

function TsrdoStatements.Execute(var vResult: TsrdoResult): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
  begin
    Result := Result and Items[i].Execute(vResult);
  end;
end;

{ TsrdoStatement }

function TsrdoStatement.GetItem(Index: Integer): TsrdoStatementItem;
begin
  Result := inherited Items[Index] as TsrdoStatementItem;
end;

procedure TsrdoStatement.SetDebug(AValue: TsrdoDebug);
begin
  if FDebug =AValue then Exit;
  FDebug :=AValue;
end;

function TsrdoStatement.Add(AObject: TsrdoStatementItem): Integer;
begin
  Result := inherited Add(AObject);
end;

function TsrdoStatement.Add(AOperator: TsrdoOperator; AObject: TsrdoObject): TsrdoStatementItem;
begin
  Result := TsrdoStatementItem.Create;
  Result.AnOperator := AOperator;
  Result.AnObject := AObject;
  Add(Result);
end;

function TsrdoStatement.Execute(var vResult: TsrdoResult): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
  begin
    Result := Result and Items[i].Execute(vResult);
  end;
end;

{ TsrdoRun }

constructor TsrdoRun.Create;
begin
  inherited Create;
  Stack := TsardStack.Create;
end;

destructor TsrdoRun.Destroy;
begin
  FreeAndNil(Stack);
  inherited Destroy;
end;

function TsrdoRun.Execute(vStatements: TsrdoStatements): Boolean;
var
  R: TsrdoResult;
begin
  R := TsrdoResult.Create;
  try
    Result := vStatements.Execute(R);
    Writeln(R.AnObject.AsString)
  finally
    R.Free;
  end;
end;

{ TsrdoObjects }

function TsrdoObjects.GetItem(Index: Integer): TsrdoObject;
begin
  Result := inherited Items[Index] as TsrdoObject;
end;

{ TsrdoObject }

procedure TsrdoObject.SetName(AValue: string);
begin
  if FName <> AValue then
  begin
    //CheckUniqe; TODO
    FName := AValue;
  end;
end;

procedure TsrdoObject.Created;
begin
end;

constructor TsrdoObject.Create;
begin
  inherited Create;
end;

procedure TsrdoObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

function TsrdoObject.This: TsrdoObject;
begin
  Result := Self;
end;

function TsrdoObject.Execute(var vResult: TsrdoResult): Boolean;
begin
  Result := True;//yes true, nothing to do, it is fine
end;

function TsrdoObject.Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean;
begin
  Result := False;
end;

procedure TsrdoObject.Assign(FromObject: TsrdoObject);
begin
  //Nothing to do
end;

function TsrdoObject.Clone: TsrdoObject;
begin
  Result := TsrdoObjectClass(ClassType).Create;
  Result.Assign(Self);
end;

function TsrdoObject.AsString: String;
var
  o: string;
begin
  if ToString(o) then
    Result := o
  else
    Result := '';
end;

function TsrdoObject.AsFloat: Float;
var
  o: Float;
begin
  if ToFloat(o) then
    Result := o
  else
    Result := 0;
end;

function TsrdoObject.AsInteger: int;
var
  o: int;
begin
  if ToInteger(o) then
    Result := o
  else
    Result := 0;
end;

function TsrdoObject.AsBoolean: Boolean;
var
  o: Boolean;
begin
  if ToBoolean(o) then
    Result := o
  else
    Result := False;
end;

function TsrdoObject.ToBoolean(out outValue: Boolean): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ToString(out outValue: string): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ToFloat(out outValue: Float): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ToInteger(out outValue: int): Boolean;
begin
  Result := False;
end;

end.
