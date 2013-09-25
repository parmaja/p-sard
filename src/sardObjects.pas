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

  TsrdObjectType = (otUnkown, otInteger, otFloat, otBoolean, otString, otObject, otClass, otVariable);
  TsrdCompare = (cmpLess, cmpEqual, cmpGreater);

  TsrdDebug = class(TsardObject)
  public
    Line: Integer;
    Column: Integer;
    FileName: string;
    BreakPoint: Boolean; //not sure do not laugh
  end;

  TsoObject = class;
  TsoObjectClass = class of TsoObject;

  TopOperator = class;

  { TsrdResult }

  TsrdResult = class(TsardObject)
    AnObject: TsoObject;
  end;

  { TsrdStatementItem }

  TsrdStatementItem = class(TsardObject)
  public
    AnOperator: TopOperator;
    AnObject: TsoObject;
    function Execute(var vResult: TsrdResult): Boolean;
  end;

  { TsrdStatement }

  TsrdStatement = class(TsardObjectList)
  private
    FDebug: TsrdDebug;
    function GetItem(Index: Integer): TsrdStatementItem;
    procedure SetDebug(AValue: TsrdDebug);
  public
    function Add(AObject: TsrdStatementItem): Integer;
    function Add(AOperator:TopOperator; AObject: TsoObject): TsrdStatementItem;
    function Execute(var vResult: TsrdResult): Boolean;
    property Items[Index: Integer]: TsrdStatementItem read GetItem; default;
    property Debug: TsrdDebug read FDebug write SetDebug; //<-- Nil until we compile it with Debug Info
  end;

  { TsrdBlock }

  TsrdBlock = class(TsardObjectList)
  private
    function GetStatement: TsrdStatement;
    function GetItem(Index: Integer): TsrdStatement;
  public
    function Add(AStatement: TsrdStatement): Integer;
    function New: TsrdStatement;
    procedure Check; //Check if empty then create first statement
    property Items[Index: Integer]: TsrdStatement read GetItem; default;
    property Statement: TsrdStatement read GetStatement;
    function Execute(var vResult: TsrdResult): Boolean;
  end;

  { TsrdVariable }

  TsrdVariable = class(TsardObject)
  private
    FName: string;
    procedure SetName(AValue: string);
  public
    property Name: string read FName write SetName;
  end;

  { TsardVariables }

  TsrdVariables = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdVariable;
  public
    property Items[Index: Integer]: TsrdVariable read GetItem; default;
    function Find(vName: string): TsrdVariable;
  end;

  { TsrdScope }

  TsrdScope = class(TsrdBlock)
  private
    FParent: TsrdScope;
    FVariables: TsrdVariables;
  public
    constructor Create(AParent: TsrdScope);
    destructor Destroy; override;
    //It is find it in the parents also
    function FindVariable(const vName: string): TsrdVariable; virtual;
    property Variables: TsrdVariables read FVariables;
    property Parent: TsrdScope read FParent;
  end;

  { TsrdBlockStack }

  TsrdBlockStack = class(TsardStack)
  private
    function GetCurrent: TsrdBlock;
  public
    procedure Push(vItem: TsrdBlock);
    property Current: TsrdBlock read GetCurrent;
  end;

  IsrdObject = interface['{9FD9AEE0-507E-4EEA-88A8-AE686E6A1D98}']
  end;

  IsrdOperate = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Add(var vResult: TsrdResult): Boolean;
    function Sub(var vResult: TsrdResult): Boolean;
    function Mulpiply(var vResult: TsrdResult): Boolean;
    function Divide(var vResult: TsrdResult): Boolean;
  end;

  IsrdCompare = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Compare(var vResult: TsrdResult): TsrdCompare;
  end;

  IsrdBlock = interface['{CB4C0FA1-E233-431E-8CC2-3755F62D93F2}']
    function Execute(var vResult: TsrdResult): Boolean;
  end;
  { TsoObject }

  TsoObject = class(TsardObject, IsrdObject)
  private
  protected
    FObjectType: TsrdObjectType;
    procedure Created; virtual;
    function GetCanExecute: Boolean; virtual;//TODO not sure
  public
    constructor Create; virtual;
    procedure AfterConstruction; override;
    function This: TsoObject; //return the same object, stupid but save some code :P
    function Execute(var vResult: TsrdResult): Boolean; virtual;
    function Operate(var vResult: TsrdResult; AnOperator: TopOperator): Boolean; virtual;
    procedure Assign(FromObject: TsoObject); virtual;
    function Clone: TsoObject; virtual;
    property ObjectType: TsrdObjectType read FObjectType;
    property CanExecute: Boolean read GetCanExecute;
    function AsString: String;
    function AsFloat: Float;
    function AsInteger: int;
    function AsBoolean: Boolean;
    function ToBoolean(out outValue: Boolean): Boolean; virtual;
    function ToString(out outValue: string): Boolean; virtual; reintroduce;
    function ToFloat(out outValue: Float): Boolean; virtual;
    function ToInteger(out outValue: int): Boolean; virtual;
  end;

  { TsoObjects }

  TsoObjects = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsoObject;
  public
    property Items[Index: Integer]: TsoObject read GetItem; default;
  end;

  { TsrdClass }

  TsoClass = class(TsoObject)
  public
    procedure Created; override;
  end;

  { TsrdInstance }

  TsoInstance = class(TsoObject)
  public
    Value: TsoObject;
    procedure Created; override;
  end;

  { TsoVariable }

  TsoVariable = class(TsoObject)
  private
    FName: string;
    procedure SetName(AValue: string);
  public
    Value: TsrdResult;
    procedure Created; override;
    property Name: string read FName write SetName;
  end;

  { TsoBranch }

  TsoScope = class(TsoObject, IsrdBlock)
  protected
    FBlock: TsrdScope;
    procedure Created; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute(var vResult: TsrdResult): Boolean; override;
    property Block: TsrdScope read FBlock;
  end;

(**** Common Objects *****)

  { TsrdNone }

  TsoNone = class(TsoObject) //None it is not Null, it is an initial value we sart it
  public
    //Do operator
    //Convert to 0 or ''
  end;

  { TsrdNumber }

  TsoNumber = class(TsoObject) //abstract
  public
    //Assign
    //Do operator
  end;

  { TsrdInteger }

  TsoInteger = class(TsoNumber)
  public
    Value: int;
    procedure Created; override;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(var vResult: TsrdResult; AnOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdFloat }

  TsoFloat = class(TsoNumber)
  public
    Value: Float;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdBoolean }

  TsoBoolean = class(TsoNumber)
  public
    Value: Boolean;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdString }

  TsoString = class(TsoObject)
  public
    Value: string;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

{* Run Time Engine *}

  { TsrdRun }

  TsrdRun = class(TsardObject)
  public
    Stack: TsardStack;
    constructor Create;
    destructor Destroy; override;
    function Execute(vBlock: TsrdBlock):Boolean;
  end;

  { TopOperator }

  TopOperator = class(TsardObject)
  protected
    function DoOperate(vResult: TsrdResult; vObject: TsoObject): Boolean; virtual;
  public
    Code: string;
    Name: string;
    Level: Integer;
    Description: string;
    function Operate(vResult: TsrdResult; vObject: TsoObject): Boolean;
    constructor Create; virtual;
  end;

  TopOperatorClass = class of TopOperator;
  { TopOperators }

  TopOperators = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TopOperator;
  protected
    function CheckBeforeRegister(AOperator: TopOperator): Boolean; virtual;
  public
    function Find(const Code: string): TopOperator;
    function FindByName(const vName: string): TopOperator;
    function RegisterOperator(AOperator: TopOperator): Boolean;
    function RegisterOperator(AOperatorClass: TopOperatorClass): Boolean;
    property Items[Index: Integer]: TopOperator read GetItem; default;
  end;

  { TopAssign }

  TopAssign = class(TopOperator)//Naaaaaaaaaah
  public
    constructor Create; override;
  end;

  { TopPlus }

  TopPlus = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopMinus }

  TopMinus = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopMultiply }

  TopMultiply = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopDivide }

  TopDivide = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopPower }

  TopPower = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopLesser }

  TopLesser = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopGreater }

  TopGreater = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopEqual }

  TopEqual = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopNotEqual }

  TopNot = class(TopOperator)
  public
    constructor Create; override;
  end;

  TopNotEqual = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopAnd }

  TopAnd = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopOr }

  TopOr = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TsrdEngine }

  TsrdEngine = class(TsardCustomEngine)
  protected
    FOperators: TopOperators;
    procedure Created; override;
    function CreateOperators: TopOperators;
  public
    constructor Create;
    function IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsControl(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsOperator(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsNumber(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsIdentifier(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;

    property Operators: TopOperators read FOperators;
  end;

function sardEngine: TsrdEngine;

implementation

uses
  sardScripts;

var
  FsardEngine: TsrdEngine = nil;

function sardEngine: TsrdEngine;
begin
  if FsardEngine = nil then
    FsardEngine := TsrdEngine.Create;
  Result := FsardEngine;
end;

{ TsardVariables }

function TsrdVariables.GetItem(Index: Integer): TsrdVariable;
begin
  Result := inherited Items[Index] as TsrdVariable;
end;

function TsrdVariables.Find(vName: string): TsrdVariable;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

{ TsrdScope }

constructor TsrdScope.Create(AParent: TsrdScope);
begin
  inherited Create;
  FVariables := TsrdVariables.Create;
end;

destructor TsrdScope.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

function TsrdScope.FindVariable(const vName: string): TsrdVariable;
begin
  Result := Variables.Find(vName);
  while (Parent <> nil) and (Result = nil) do
  begin
    Result := Parent.FindVariable(vName);
  end;
end;

{ TsrdVariable }

procedure TsrdVariable.SetName(AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

{ TopAssign }

constructor TopAssign.Create;
begin
  inherited Create;
  Code := ':=';
  Name := 'Assign';
  Level := 10;
  Description := 'Clone to another object';
end;

{ TsrdBlockStack }

function TsrdBlockStack.GetCurrent: TsrdBlock;
begin
  Result := (inherited GetCurrent) as TsrdBlock;
end;

procedure TsrdBlockStack.Push(vItem: TsrdBlock);
begin
  inherited Push(vItem);
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

procedure TsoVariable.SetName(AValue: string);
begin
  FName := AValue;
end;

procedure TsoVariable.Created;
begin
  inherited Created;
  FObjectType := otVariable;
end;

{ TsrdBranch }

procedure TsoScope.Created;
begin
  inherited Created;
end;

constructor TsoScope.Create;
begin
  inherited Create;
  FBlock := TsrdScope.Create(nil);
end;

destructor TsoScope.Destroy;
begin
  FreeAndNil(FBlock);
  inherited Destroy;
end;

function TsoScope.Execute(var vResult: TsrdResult): Boolean;
begin
  //Just Forward
  Result := Block.Execute(vResult);
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

{ TopOperator }

function TopOperator.DoOperate(vResult: TsrdResult; vObject: TsoObject): Boolean;
begin
  Result := False;
end;

function TopOperator.Operate(vResult: TsrdResult; vObject: TsoObject): Boolean;
var
  aResult: TsrdResult;
  procedure OperateNow(O: TsoObject);
  begin
    if Self = nil then //TODO: If assign
      vResult.AnObject := O.Clone
    else
    begin
      Result := DoOperate(vResult, O);
      if not Result then
      begin
        Result := O.Operate(vResult, Self);
        //Ok let me do it. : the TopPlus said
      end;
    end;
  end;
begin
  aResult := TsrdResult.Create;
  try
    if vObject.Execute(aResult) then //it is a block
    begin
      if aResult.AnObject <> nil then
        OperateNow(aResult.AnObject)
    end
    else//<-- maybe not !!!
      OperateNow(vObject);
  finally
    aResult.Free;
  end;
end;

constructor TopOperator.Create;
begin
  inherited Create;
end;

function TopOperators.Find(const Code: string): TopOperator;
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

function TopOperators.FindByName(const vName: string): TopOperator;
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

function TopOperators.RegisterOperator(AOperator: TopOperator): Boolean;
begin
  Result := CheckBeforeRegister(AOperator);
  if Result then
    Add(AOperator);
end;

function TopOperators.RegisterOperator(AOperatorClass: TopOperatorClass): Boolean;
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

{ TopOperators }

function TopOperators.GetItem(Index: Integer): TopOperator;
begin
  Result := inherited Items[Index] as TopOperator;
end;

function TopOperators.CheckBeforeRegister(AOperator: TopOperator): Boolean;
begin
  Result := True;
end;

{ TsrdEngine }

procedure TsrdEngine.Created;
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

function TsrdEngine.CreateOperators: TopOperators;
begin
  Result := TopOperators.Create;
end;

constructor TsrdEngine.Create;
begin
  inherited Create;
  FOperators := CreateOperators;
end;

function TsrdEngine.IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := vChar in sWhitespace;
end;

function TsrdEngine.IsControl(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in aControlsOpenChars
  else
    Result := vChar in sControlsChars;
end;

function TsrdEngine.IsOperator(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  //if vOpen then
    Result := vChar in sOperatorOpenChars
//  else
//    Result := not IsWhiteSpace(vChar, False) and not IsNumber(vChar, False) and not IsControl(vChar, False); //Can be any thing except those
end;

function TsrdEngine.IsNumber(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in sNumberOpenChars
  else
    Result := vChar in sNumberChars;
end;

function TsrdEngine.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := inherited IsIdentifier(vChar, vOpen);
end;


{ TsrdStatementItem }

function TsrdStatementItem.Execute(var vResult: TsrdResult): Boolean;
begin
  if AnObject = nil then
    raise EsardException.Create('Object not set!');
  {if AnOperator = nil then
    raise EsardException.Create('Object not set!');}
  Result := AnOperator.Operate(vResult, AnObject);//Even AnOperator is nil it will work
end;

{ TsrdClass }

procedure TsoClass.Created;
begin
  inherited Created;
  FObjectType := otClass;
end;

{ TsrdInstance }

procedure TsoInstance.Created;
begin
  inherited Created;
  FObjectType := otObject;
end;

{ TsrdBoolean }

procedure TsoBoolean.Created;
begin
  inherited Created;
  FObjectType := otBoolean;
end;

function TsoBoolean.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsoBoolean.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsoBoolean.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsoBoolean.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value;
  Result := True;
end;

{ TsrdString }

procedure TsoString.Created;
begin
  inherited Created;
  FObjectType := otString;
end;

function TsoString.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsoString.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsoString.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsoString.ToBoolean(out outValue: Boolean): Boolean;
begin
  if not TryStrToBool(Value, outValue) then
    outValue := AsInteger <> 0;
  Result := True;
end;

{ TsrdFloat }

procedure TsoFloat.Created;
begin
  inherited Created;
  FObjectType := otFloat;
end;

function TsoFloat.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsoFloat.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsoFloat.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsoFloat.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsrdInteger }

procedure TsoInteger.Created;
begin
  inherited Created;
  FObjectType := otInteger;
end;

procedure TsoInteger.Assign(FromObject: TsoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsoInteger then
      Value := (FromObject as TsoInteger).Value
    else
      Value := FromObject.AsInteger;
  end;
end;

function TsoInteger.Operate(var vResult: TsrdResult; AnOperator: TopOperator): Boolean;
begin
  Result := False;
  if vResult.AnObject = nil then
  begin
    vResult.AnObject := Clone;
    Result := True;
  end
  else if vResult.AnObject is TsoInteger then
  begin
    Result := True;
    WriteLn(IntToStr(TsoInteger(vResult.AnObject).Value) + ' '+ AnOperator.Code+' ' +IntToStr(Value));
    case AnOperator.Code of
      '+': TsoInteger(vResult.AnObject).Value := TsoInteger(vResult.AnObject).Value + Value;
      '-': TsoInteger(vResult.AnObject).Value := TsoInteger(vResult.AnObject).Value - Value;
      '*': TsoInteger(vResult.AnObject).Value := TsoInteger(vResult.AnObject).Value * Value;
      '/': TsoInteger(vResult.AnObject).Value := TsoInteger(vResult.AnObject).Value div Value;
      else
        Result := False;
    end;
  end;
end;

function TsoInteger.ToString(out outValue: string): Boolean;
begin
  Result := True;
  outValue := IntToStr(Value);
end;

function TsoInteger.ToFloat(out outValue: Float): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsoInteger.ToInteger(out outValue: int): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsoInteger.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsoBlock }

function TsrdBlock.GetStatement: TsrdStatement;
begin
  Check;//TODO: not sure
  Result := Last as TsrdStatement;
end;

function TsrdBlock.GetItem(Index: Integer): TsrdStatement;
begin
  Result := inherited Items[Index] as TsrdStatement;
end;

function TsrdBlock.Add(AStatement: TsrdStatement): Integer;
begin
  Result := inherited Add(AStatement);
end;

function TsrdBlock.New: TsrdStatement;
begin
  Result := TsrdStatement.Create;
  Add(Result);
end;

procedure TsrdBlock.Check;
begin
  if Count = 0  then
    New;
end;

function TsrdBlock.Execute(var vResult: TsrdResult): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
  begin
    Result := Result and Items[i].Execute(vResult);
  end;
end;

{ TsrdStatement }

function TsrdStatement.GetItem(Index: Integer): TsrdStatementItem;
begin
  Result := inherited Items[Index] as TsrdStatementItem;
end;

procedure TsrdStatement.SetDebug(AValue: TsrdDebug);
begin
  if FDebug =AValue then Exit;
  FDebug :=AValue;
end;

function TsrdStatement.Add(AObject: TsrdStatementItem): Integer;
begin
  Result := inherited Add(AObject);
end;

function TsrdStatement.Add(AOperator: TopOperator; AObject: TsoObject): TsrdStatementItem;
begin
  Result := TsrdStatementItem.Create;
  Result.AnOperator := AOperator;
  Result.AnObject := AObject;
  Add(Result);
end;

function TsrdStatement.Execute(var vResult: TsrdResult): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
  begin
    Result := Result and Items[i].Execute(vResult);
  end;
end;

{ TsoRun }

constructor TsrdRun.Create;
begin
  inherited Create;
  Stack := TsardStack.Create;
end;

destructor TsrdRun.Destroy;
begin
  FreeAndNil(Stack);
  inherited Destroy;
end;

function TsrdRun.Execute(vBlock: TsrdBlock): Boolean;
var
  R: TsrdResult;
begin
  R := TsrdResult.Create;
  try
    Result := vBlock.Execute(R);
    Writeln(R.AnObject.AsString)
  finally
    R.Free;
  end;
end;

{ TsoObjects }

function TsoObjects.GetItem(Index: Integer): TsoObject;
begin
  Result := inherited Items[Index] as TsoObject;
end;

{ TsoObject }

function TsoObject.GetCanExecute: Boolean;
begin
  Result := False;
end;

procedure TsoObject.Created;
begin
end;

constructor TsoObject.Create;
begin
  inherited Create;
end;

procedure TsoObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

function TsoObject.This: TsoObject;
begin
  Result := Self;
end;

function TsoObject.Execute(var vResult: TsrdResult): Boolean;
begin
  Result := False;
end;

function TsoObject.Operate(var vResult: TsrdResult; AnOperator: TopOperator): Boolean;
begin
  Result := False;
end;

procedure TsoObject.Assign(FromObject: TsoObject);
begin
  //Nothing to do
end;

function TsoObject.Clone: TsoObject;
begin
  Result := TsoObjectClass(ClassType).Create;
  Result.Assign(Self);
end;

function TsoObject.AsString: String;
var
  o: string;
begin
  if ToString(o) then
    Result := o
  else
    Result := '';
end;

function TsoObject.AsFloat: Float;
var
  o: Float;
begin
  if ToFloat(o) then
    Result := o
  else
    Result := 0;
end;

function TsoObject.AsInteger: int;
var
  o: int;
begin
  if ToInteger(o) then
    Result := o
  else
    Result := 0;
end;

function TsoObject.AsBoolean: Boolean;
var
  o: Boolean;
begin
  if ToBoolean(o) then
    Result := o
  else
    Result := False;
end;

function TsoObject.ToBoolean(out outValue: Boolean): Boolean;
begin
  Result := False;
end;

function TsoObject.ToString(out outValue: string): Boolean;
begin
  Result := False;
end;

function TsoObject.ToFloat(out outValue: Float): Boolean;
begin
  Result := False;
end;

function TsoObject.ToInteger(out outValue: int): Boolean;
begin
  Result := False;
end;

end.
