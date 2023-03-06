unit sardJSONRTTIs;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey 
*}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  mnUtils, mnClasses, mnRTTIUtils, mnDON,
  sardClasses, sardParsers, sardStandards, sardJSONs;

type

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure AcquirePair(AParentObject: TObject; const AName: string; out AObject: TObject); override;
    function AcquireValue(AObject: TObject; const AName: string; const AValue: string; AType: TDONType): TObject; override;
  end;

implementation

uses
  StrUtils;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.AcquirePair(AParentObject: TObject; const AName: string; out AObject: TObject);
begin
  AObject := AParentObject;
end;

function TRTTIJSONParser.AcquireValue(AObject: TObject; const AName: string; const AValue: string; AType: TDONType): TObject;
begin
  if AName <> '' then
    SetPropertyValue(AObject, AName, AValue);
  Result := AObject;
end;

end.
