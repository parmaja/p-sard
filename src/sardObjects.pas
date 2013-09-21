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

  { TsrdoResult }

  TsrdoResult = class(TsardObject)
    AnObject: TsrdoObject;
  end;

  { TsrdoStatementItem }

  TsrdoStatementItem = class(TsardObject)
  public
    AnOperator: TsardOperator;
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
    function Add(AOperator:TsardOperator; AObject:TsrdoObject): TsrdoStatementItem;
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
    constructor Create(AOperator:TsardOperator; AStatment:TsrdoStatement); overload; deprecated;
    function This: TsrdoObject; //return the same object, stupid but save some code :P
    function Execute(var vResult: TsrdoResult; AnOperator: TsardOperator): Boolean; virtual;
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
    function Execute(var vResult: TsrdoResult; AnOperator: TsardOperator): Boolean; override;
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

  TsrdoOperator = class(TsardOperator)
  protected
    {L: Left, R: Right objects}
    function DoOperate(var vResult: TsrdoObject; vObject: TsrdoObject): Boolean; virtual;
  public
    function Operate(var vResult: TsardObject; vObject: TsardObject): Boolean; override; final;
  end;

  { TsrdoOperators }

  TsrdoOperators = class(TsardOperators)
  protected
    function CheckBeforeRegister(AOperator:TsardOperator): Boolean; override;
  public
  end;

  { TopPlus }

  TopPlus = class(TsrdoOperator)
  public
    function DoOperate(var vResult: TsrdoObject; vObject: TsrdoObject): Boolean; override;
  end;

  { TsrdoEngine }

  TsrdoEngine = class(TsardCustomEngine)
  protected
    procedure Created; override;
    function CreateOperators: TsardOperators; override;
  public
  end;

var
  sardEngine: TsrdoEngine = nil;

implementation

{ TopPlus }

function TopPlus.DoOperate(var vResult: TsrdoObject; vObject: TsrdoObject): Boolean;
begin

end;

{ TsrdoOperators }

function TsrdoOperators.CheckBeforeRegister(AOperator: TsardOperator): Boolean;
begin
  Result := True;
end;

{ TsrdoOperator }

function TsrdoOperator.DoOperate(var vResult: TsrdoObject; vObject: TsrdoObject): Boolean;
begin
  Result := False;
end;

function TsrdoOperator.Operate(var vResult: TsardObject; vObject: TsardObject): Boolean;
begin
  Result := DoOperate(TsrdoObject(vResult), vObject as TsrdoObject);
end;

{ TsrdoEngine }

procedure TsrdoEngine.Created;
begin
  inherited;
  Operators.RegisterOperator(TopPlus.Create);
end;

function TsrdoEngine.CreateOperators: TsardOperators;
begin
  Result := TsrdoOperators.Create;
  //TsardOperator = (opPlus, opMinus, opMultiply, opDivision, opNot, opSeprator);//no level until now //Move to sardObjects
end;

{ TsrdoStatementItem }

function TsrdoStatementItem.Execute(var vResult: TsrdoResult): Boolean;
begin
  Result := AnObject.Execute(vResult, AnOperator);
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

function TsrdoInteger.Execute(var vResult: TsrdoResult; AnOperator: TsardOperator): Boolean;
begin
  Result := True;
  if vResult.AnObject = nil then
  begin
    vResult.AnObject := Clone;
  end
  else if vResult.AnObject is TsrdoInteger then
  begin
{    case AnOperator of
      opPlus: TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value + Value;
      opMinus: TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value - Value;
      opMultiply: TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value * Value;
      opDivision: TsrdoInteger(vResult.AnObject).Value := TsrdoInteger(vResult.AnObject).Value div Value;
    end;}
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

function TsrdoStatement.Add(AOperator: TsardOperator; AObject: TsrdoObject): TsrdoStatementItem;
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

constructor TsrdoObject.Create(AOperator: TsardOperator; AStatment: TsrdoStatement);
begin
  inherited Create;
  AStatment.Add(AOperator, Self);
  FStatement := AStatment;
end;

function TsrdoObject.This: TsrdoObject;
begin
  Result := Self;
end;

function TsrdoObject.Execute(var vResult: TsrdoResult; AnOperator: TsardOperator): Boolean;
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
