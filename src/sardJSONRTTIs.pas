unit sardJSONRTTIs;
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
  Classes, SysUtils, TypInfo, Variants,
  mnUtils, mnClasses, mnRTTIUtils,
  sardClasses, sardParsers, sardStandards, sardJSONs;

type

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure NeedElement(AParentObject: TObject; AName: string; out AObject: TObject); override;
    function SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType): TObject; override;
  end;

implementation

uses
  StrUtils;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.NeedElement(AParentObject: TObject; AName: string; out AObject: TObject);
begin
  AObject := AParentObject;
end;

function TRTTIJSONParser.SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType): TObject;
begin
  if AName <> '' then
    SetPropertyValue(AObject, AName, AValue);
  Result := AObject;
end;

end.
