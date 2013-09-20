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

  TsrdoStatementItem = class(TsardObject)
  public
    AnOperator: TsardOperator;
    AnObject: TsrdoObject;
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
    function GetCurrent: TsrdoStatement;
    function GetItem(Index: Integer): TsrdoStatement;
  public
    function Add(AStatement: TsrdoStatement): Integer;
    function New: TsrdoStatement;
    property Items[Index: Integer]: TsrdoStatement read GetItem; default;
    property Current: TsrdoStatement read GetCurrent;
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
    FParent: TsrdoObject;
    FStatement: TsrdoStatement;
    procedure SetName(AValue: string);
    procedure SetStatement(AValue: TsrdoStatement);
  protected
    FObjectType: TsrdoObjectType;
    procedure Created; virtual;
  public
    constructor Create(AParent: TsrdoObject); overload;
    constructor Create(AOperator:TsardOperator; AStatment:TsrdoStatement); overload;
    function DoOperator(WithObject: TsrdoObject; AnOperator: TsardOperator): Boolean;
    property Parent: TsrdoObject read FParent;
    property Name: string read FName write SetName;
    property Statement: TsrdoStatement read FStatement write SetStatement;
    property ObjectType: TsrdoObjectType read FObjectType;
    //function ConvertToString(out oValue):Boolean;
    //ConvertToFloat
    //ConvertToInteger
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
  end;

  { TsrdoFloat }

  TsrdoFloat = class(TsrdoNumber)
  public
    Value: extended;
    procedure Created; override;
  end;

  { TsrdoBoolean }

  TsrdoBoolean = class(TsrdoNumber)
  public
    Value: Byte;
    procedure Created; override;
  end;

  { TsrdoString }

  TsrdoString = class(TsrdoObject)
  public
    Value: string;
    procedure Created; override;
  end;

{* Run Time Engine *}

  { TsrdoRun }

  TsrdoRun = class(TsardObject)
  public
    Stack: TsardStack;
    constructor Create;
    destructor Destroy; override;
    function Run(AnObject: TsrdoObject):Boolean; virtual;
  end;

implementation

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

{ TsrdoString }

procedure TsrdoString.Created;
begin
  inherited Created;
  FObjectType := otString;
end;

{ TsrdoFloat }

procedure TsrdoFloat.Created;
begin
  inherited Created;
  FObjectType := otFloat;
end;

{ TsrdoInteger }

procedure TsrdoInteger.Created;
begin
  inherited Created;
  FObjectType := otInteger;
end;

{ TsrdoBlock }

function TsrdoBlock.GetCurrent: TsrdoStatement;
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

function TsrdoRun.Run(AnObject: TsrdoObject): Boolean;
begin

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

constructor TsrdoObject.Create(AParent: TsrdoObject);
begin
  inherited Create;
  FParent := AParent;
end;

constructor TsrdoObject.Create(AOperator: TsardOperator; AStatment: TsrdoStatement);
begin
  inherited Create;
  AStatment.Add(AOperator, Self);
  FStatement := AStatment;
end;

function TsrdoObject.DoOperator(WithObject: TsrdoObject; AnOperator: TsardOperator): Boolean;
begin
  Result := False;
end;

end.

