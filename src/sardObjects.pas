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
  { TsardObject }

  TsardObject = class(TObject)
  private
    FName: string; //if name is '' then the object cant change the value
    FParent: TsardObject;
    procedure SetName(AValue: string);
  public
    constructor Create(AParent: TsardObject); virtual;
    property Parent: TsardObject read FParent;
    property Name: string read FName write SetName;
    function DoOperator(WithObject: TsardObject; AnOperator: TsardOperator): Boolean;
    //function ConvertToString(out oValue):Boolean;
    //ConvertToFloat
    //ConvertToInteger
  end;

  { TsardObjects }

  TsardObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TsardObject;
  public
    property Items[Index: Integer]: TsardObject read GetItem; default;
  end;

  TNumberObject = class(TsardObject) //abstract
  public
    //Assign
    //Do operator
  end;

  TIntegerObject = class(TNumberObject)
  public
    Value: Integer
  end;

  TFloatObject = class(TNumberObject)
  public
    Value: extended;
  end;

  TStringObject = class(TsardObject)
  public
    Value: string;
  end;

implementation

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

