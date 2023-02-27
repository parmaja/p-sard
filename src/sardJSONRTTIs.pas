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
  mnUtils, mnClasses, mnRTTIUtils,
  sardClasses, sardParsers, sardStandards, sardJSONs;

type

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure RequirePair(AParentObject: TObject; const AName: string; out AObject: TObject); override;
    function SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject; override;
  end;

implementation

uses
  StrUtils;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.RequirePair(AParentObject: TObject; const AName: string; out AObject: TObject);
begin
  AObject := AParentObject;
end;

function TRTTIJSONParser.SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject;
begin
  if AName <> '' then
    SetPropertyValue(AObject, AName, AValue);
  Result := AObject;
end;

end.
