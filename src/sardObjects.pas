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
  Classes, SysUtils, Contnrs, sardClasses;

type
  TsardObject = class;

  TsardStatementItem = class(TObject)
  public
    AnOperator: TsardOperator;
    AnObject: TsardObject;
  end;

  { TsardStatement }

  TsardStatement = class(TObjectList)
  private
    function GetItem(Index: Integer): TsardStatementItem;
  public
    function Add(AObject: TsardStatementItem): Integer;
    function Add(AOperator:TsardOperator; AObject:TsardObject): TsardStatementItem;
    property Items[Index: Integer]: TsardStatementItem read GetItem; default;
  end;

  { TsardBlock }

  TsardBlock = class(TObjectList)
  private
    function GetItem(Index: Integer): TsardStatement;
  public
    property Items[Index: Integer]: TsardStatement read GetItem; default;
  end;

  { TsardObjects }

  TsardObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TsardObject;
  public
    property Items[Index: Integer]: TsardObject read GetItem; default;
  end;

  { TsardObject }

  TsardObject = class(TObject)
  private
    FName: string; //if name is '' then the object cant change the value
    FParent: TsardObject;
    FStatement: TsardStatement;
    procedure SetName(AValue: string);
    procedure SetStatement(AValue: TsardStatement);
  public
    constructor Create(AParent: TsardObject); virtual;
    function DoOperator(WithObject: TsardObject; AnOperator: TsardOperator): Boolean;
    property Parent: TsardObject read FParent;
    property Name: string read FName write SetName;
    property Statement: TsardStatement read FStatement write SetStatement;
    //function ConvertToString(out oValue):Boolean;
    //ConvertToFloat
    //ConvertToInteger
  end;

  TsardClass = class(TsardObject)
  end;

  TsardInstance = class(TsardObject)
  end;


(**** Common Objects *****)

  TsardNoneObject = class(TsardObject) //None it is not Null, it is an initial value we sart it
  public
    //Do operator
    //Convert to 0 or ''
  end;

  TsardNumberObject = class(TsardObject) //abstract
  public
    //Assign
    //Do operator
  end;

  TsardIntegerObject = class(TsardNumberObject)
  public
    Value: Int64;
  end;

  TsardFloatObject = class(TsardNumberObject)
  public
    Value: extended;
  end;

  TsardStringObject = class(TsardObject)
  public
    Value: string;
  end;

(******************************
*****                     *****
*******************************)

  { TsardRun }

  TsardRun = class(TObject)
  public
    Stack: TsardStack;
    constructor Create;
    destructor Destroy; override;
    function Run(AnObject: TsardObject):Boolean; virtual;
  end;

implementation

{ TsardBlock }

function TsardBlock.GetItem(Index: Integer): TsardStatement;
begin
  Result := inherited Items[Index] as TsardStatement;
end;

{ TsardStatement }

function TsardStatement.GetItem(Index: Integer): TsardStatementItem;
begin
  Result := inherited Items[Index] as TsardStatementItem;
end;

function TsardStatement.Add(AObject: TsardStatementItem): Integer;
begin
  Result := inherited Add(AObject);
end;

function TsardStatement.Add(AOperator: TsardOperator; AObject: TsardObject): TsardStatementItem;
begin
  Result := TsardStatementItem.Create;
  Result.AnOperator := AOperator;
  Result.AnObject := AObject;
  Add(Result);
end;

{ TsardRun }

constructor TsardRun.Create;
begin
  inherited Create;
  Stack := TsardStack.Create;
end;

destructor TsardRun.Destroy;
begin
  FreeAndNil(Stack);
  inherited Destroy;
end;

function TsardRun.Run(AnObject: TsardObject): Boolean;
begin

end;

{ TsardObjects }

function TsardObjects.GetItem(Index: Integer): TsardObject;
begin
  Result := inherited Items[Index] as TsardObject;
end;

{ TsardObject }

procedure TsardObject.SetName(AValue: string);
begin
  if FName <> AValue then
  begin
    //CheckUniqe; TODO
    FName := AValue;
  end;
end;

procedure TsardObject.SetStatement(AValue: TsardStatement);
begin
  if FStatement =AValue then Exit;
  FStatement :=AValue;
end;

constructor TsardObject.Create(AParent: TsardObject);
begin
  inherited Create;
  FParent := AParent;
end;

function TsardObject.DoOperator(WithObject: TsardObject; AnOperator: TsardOperator): Boolean;
begin
  Result := False;
end;

end.

