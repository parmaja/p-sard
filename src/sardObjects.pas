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

  TsrdoDebug = class(TsardObject)
  public
    Line: Int64;
    Column: Integer;
    FileName: string;
    BreakPoint: Boolean; //not sure do not laugh
  end;

  TsrdoObject = class;
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
    property Items[Index: Integer]: TsrdoStatementItem read GetItem; default;
    property Debug: TsrdoDebug read FDebug write SetDebug; //<-- Nil until we compile it with Debug Info
  end;

  { TsrdoBlock }

  TsrdoBlock = class(TsardObjectList)
  private
    function GetStatement: TsrdoStatement;
    function GetItem(Index: Integer): TsrdoStatement;
  public
    function Add(AStatement: TsrdoStatement): Integer;
    function New: TsrdoStatement;
    property Items[Index: Integer]: TsrdoStatement read GetItem; default;
    property Statement: TsrdoStatement read GetStatement;
  end;

  { TsrdoObjects }

  TsrdoObjects = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdoObject;
  public
    property Items[Index: Integer]: TsrdoObject read GetItem; default;
  end;

  { TsrdoObject }

  TsrdoObject = class(TsardObject)
  private
    FName: string; //if name is '' then the object cant change the value
    FStatement: TsrdoStatement;
    procedure SetName(AValue: string);
    procedure SetStatement(AValue: TsrdoStatement);
  protected
    FObjectType: TsrdoObjectType;
    procedure Created; virtual;
  public
    constructor Create(AOperator:TsrdoOperator; AStatment:TsrdoStatement); overload; deprecated;
    procedure AfterConstruction; override;
    function This: TsrdoObject; //return the same object, stupid but save some code :P
    function Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean; virtual;
    property Name: string read FName write SetName;
    procedure Assign(FromObject: TsrdoObject); virtual;
    function Clone: TsrdoObject; virtual;
    property Statement: TsrdoStatement read FStatement write SetStatement;
    property ObjectType: TsrdoObjectType read FObjectType;
    function ToString: String; reintroduce;
    function ToFloat: Float;
    function ToInteger: Int64;
    function ConvertToBoolean(out outValue: Boolean): Boolean; virtual;
    function ConvertToString(out outValue: string): Boolean; virtual;
    function ConvertToFloat(out outValue: Float): Boolean; virtual;
    function ConvertToInteger(out outValue: Int64): Boolean; virtual;
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
    Value: Int64;
    procedure Created; override;
    function Clone: TsrdoObject; override;
    procedure Assign(FromObject: TsrdoObject); override;
    function Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean; override;
    function ConvertToString(out outValue: string): Boolean; override;
    function ConvertToFloat(out outValue: Float): Boolean; override;
    function ConvertToInteger(out outValue: Int64): Boolean; override;
  end;

  { TsrdoFloat }

  TsrdoFloat = class(TsrdoNumber)
  public
    Value: Float;
    procedure Created; override;
    function ConvertToString(out outValue: string): Boolean; override;
    function ConvertToFloat(out outValue: Float): Boolean; override;
    function ConvertToInteger(out outValue: Int64): Boolean; override;
  end;

  { TsrdoBoolean }

  TsrdoBoolean = class(TsrdoNumber)
  public
    Value: Byte;
    procedure Created; override;
    function ConvertToString(out outValue: string): Boolean; override;
    function ConvertToFloat(out outValue: Float): Boolean; override;
    function ConvertToInteger(out outValue: Int64): Boolean; override;
  end;

  { TsrdoString }

  TsrdoString = class(TsrdoObject)
  public
    Value: string;
    procedure Created; override;
    function ConvertToString(out outValue: string): Boolean; override;
    function ConvertToFloat(out outValue: Float): Boolean; override;
    function ConvertToInteger(out outValue: Int64): Boolean; override;
  end;

{* Run Time Engine *}

  { TsrdoRun }

  TsrdoRun = class(TsardObject)
  public
    Stack: TsardStack;
    constructor Create;
    destructor Destroy; override;
    function Execute(var vResult: TsrdoResult; AStatement: TsrdoStatement):Boolean; overload;
    function Execute(var vResult: TsrdoResult; ABlock: TsrdoBlock):Boolean; overload;
    function Execute(ABlock: TsrdoBlock):Boolean; overload;
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
    function Operate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean; virtual;
    constructor Create; virtual;
  end;

  { TsrdoOperators }

  TsrdoOperators = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdoOperator;
  protected
    function CheckBeforeRegister(AOperator: TsrdoOperator): Boolean; virtual;
  public
    function Find(const Code: string): TsrdoOperator;
    function FindByName(const vName: string): TsrdoOperator;
    function RegisterOperator(AOperator: TsrdoOperator): Boolean; virtual;
    property Items[Index: Integer]: TsrdoOperator read GetItem; default;
  end;

  { TopPlus }

  //opPlus, opMinus, opMultiply, opDivision, opNot, opSeprator

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

  { TopDivision }

  TopDivision = class(TsrdoOperator)
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

{ TopDivision }

constructor TopDivision.Create;
begin
  inherited Create;
  Code := '/';
  Name := 'Divition';
  Level := 1;
  Description := '';
end;

{ TopMultiply }

constructor TopMultiply.Create;
begin
  inherited Create;
  Code := '*';
  Name := 'Multiply';
  Level := 1;
  Description := '';
end;

{ TopMinus }

constructor TopMinus.Create;
begin
  inherited Create;
  Code := '-';
  Name := 'Minus';
  Level := 0;
  Description := 'Sub object to another object';
end;

{ TsrdoOperator }

function TsrdoOperator.DoOperate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean;
begin
  Result := False;
end;

function TsrdoOperator.Operate(vResult: TsrdoResult; vObject: TsrdoObject): Boolean;
begin
  Result := vObject.Operate(vResult, Self);
  if not Result then
  begin
    DoOperate(vResult, vObject);
    //Ok let me do it. : the TopPlus said
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


{ TopPlus }

constructor TopPlus.Create;
begin
  inherited Create;
  Code := '+';
  Name := 'Plus';
  Level := 0;
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
  Operators.RegisterOperator(TopPlus.Create);
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
  if vOpen then
    Result := vChar in sOperatorOpenChars
  else
    Result := not IsWhiteSpace(vChar, False) and not IsNumber(vChar, False) and not IsControl(vChar, False); //Can be any thing except those
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
  Result := AnObject.Operate(vResult, AnOperator);
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

function TsrdoBoolean.ConvertToString(out outValue: string): Boolean;
begin
  Result :=inherited ConvertToString(outValue);
end;

function TsrdoBoolean.ConvertToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ConvertToFloat(outValue);
end;

function TsrdoBoolean.ConvertToInteger(out outValue: Int64): Boolean;
begin
  Result :=inherited ConvertToInteger(outValue);
end;

{ TsrdoString }

procedure TsrdoString.Created;
begin
  inherited Created;
  FObjectType := otString;
end;

function TsrdoString.ConvertToString(out outValue: string): Boolean;
begin
  Result :=inherited ConvertToString(outValue);
end;

function TsrdoString.ConvertToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ConvertToFloat(outValue);
end;

function TsrdoString.ConvertToInteger(out outValue: Int64): Boolean;
begin
  Result :=inherited ConvertToInteger(outValue);
end;

{ TsrdoFloat }

procedure TsrdoFloat.Created;
begin
  inherited Created;
  FObjectType := otFloat;
end;

function TsrdoFloat.ConvertToString(out outValue: string): Boolean;
begin
  Result :=inherited ConvertToString(outValue);
end;

function TsrdoFloat.ConvertToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ConvertToFloat(outValue);
end;

function TsrdoFloat.ConvertToInteger(out outValue: Int64): Boolean;
begin
  Result :=inherited ConvertToInteger(outValue);
end;

{ TsrdoInteger }

procedure TsrdoInteger.Created;
begin
  inherited Created;
  FObjectType := otInteger;
end;

function TsrdoInteger.Clone: TsrdoObject;
begin
  Result := TsrdoInteger.Create;
  Result.Assign(Self);
end;

procedure TsrdoInteger.Assign(FromObject: TsrdoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsrdoInteger then
      Value := (FromObject as TsrdoInteger).Value
    else
      Value := FromObject.ToInteger;
  end;
end;

function TsrdoInteger.Operate(var vResult: TsrdoResult; AnOperator: TsrdoOperator): Boolean;
begin
  Result := True;
  if vResult.AnObject = nil then
  begin
    vResult.AnObject := Clone;
  end
  else if vResult.AnObject is TsrdoInteger then
  begin
    case AnOperator.Code of
      '+': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value + Value;
      '-': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value - Value;
      '*': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value * Value;
      '/': TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value div Value;
    end;
  end;
end;

function TsrdoInteger.ConvertToString(out outValue: string): Boolean;
begin
  Result := True;
  outValue := IntToStr(Value);
end;

function TsrdoInteger.ConvertToFloat(out outValue: Float): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsrdoInteger.ConvertToInteger(out outValue: Int64): Boolean;
begin
  Result := True;
  outValue := Value;
end;

{ TsrdoBlock }

function TsrdoBlock.GetStatement: TsrdoStatement;
begin
  Result := Last as TsrdoStatement;
end;

function TsrdoBlock.GetItem(Index: Integer): TsrdoStatement;
begin
  Result := inherited Items[Index] as TsrdoStatement;
end;

function TsrdoBlock.Add(AStatement: TsrdoStatement): Integer;
begin
  Result := inherited Add(AStatement);
end;

function TsrdoBlock.New: TsrdoStatement;
begin
  Result := TsrdoStatement.Create;
  Add(Result);
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

function TsrdoRun.Execute(var vResult: TsrdoResult; AStatement: TsrdoStatement): Boolean;
var
  i: Integer;
begin
  Result := AStatement.Count > 0;
  for i := 0 to AStatement.Count -1 do
  begin
    Result := Result and AStatement[i].Execute(vResult);
  end;
end;

function TsrdoRun.Execute(var vResult: TsrdoResult; ABlock: TsrdoBlock): Boolean;
var
  i: Integer;
begin
  Result := ABlock.Count > 0;
  for i := 0 to ABlock.Count -1 do
  begin
    Result := Result and Execute(vResult, ABlock[i]);
  end;
end;


function TsrdoRun.Execute(ABlock: TsrdoBlock): Boolean;
var
  R: TsrdoResult;
begin
  R := TsrdoResult.Create;
  try
    Result := Execute(R, ABlock);
    Writeln(R.AnObject.ToString)
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

procedure TsrdoObject.SetStatement(AValue: TsrdoStatement);
begin
  if FStatement =AValue then Exit;
  FStatement :=AValue;
end;

procedure TsrdoObject.Created;
begin
end;

constructor TsrdoObject.Create(AOperator: TsrdoOperator; AStatment: TsrdoStatement);
begin
  inherited Create;
  AStatment.Add(AOperator, Self);
  FStatement := AStatment;
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
  Result := TsrdoObject.Create;
end;

function TsrdoObject.ToString: String;
var
  o: string;
begin
  if ConvertToString(o) then
    Result := o
  else
    Result := '';
end;

function TsrdoObject.ToFloat: Float;
var
  o: Float;
begin
  if ConvertToFloat(o) then
    Result := o
  else
    Result := 0;
end;

function TsrdoObject.ToInteger: Int64;
var
  o: Int64;
begin
  if ConvertToInteger(o) then
    Result := o
  else
    Result := 0;
end;

function TsrdoObject.ConvertToBoolean(out outValue: Boolean): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ConvertToString(out outValue: string): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ConvertToFloat(out outValue: Float): Boolean;
begin
  Result := False;
end;

function TsrdoObject.ConvertToInteger(out outValue: Int64): Boolean;
begin
  Result := False;
end;

end.
