unit sard;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0 (modified of http://www.gnu.org/licenses/lgpl.html)
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Contnrs,
  sardClasses, sardScripts;

type

  { TmyScanner }

  TmyParser = class(TsardScriptParser)
  protected
    procedure Push(Token: String; TokenID: Integer); override;
  end;

  { TmyScript }

  TmyScript = class(TsardScript)
  protected
    function CreateParser: TsardParser; override;
  end;

function Execute(Lines: TStrings): Boolean;

implementation

uses
  StrUtils;

{ TmyScript }

function TmyScript.CreateParser: TsardParser;
begin
  Result := TmyParser.Create;
end;

{ TmyScanner }

procedure TmyParser.Push(Token: String; TokenID: Integer);
begin
  inherited;
  WriteLn(Token);
end;

function Execute(Lines: TStrings): Boolean;
var
  Scanner: TmyScript;
  Element: TsardElement;
begin
  Scanner := TmyScript.Create;
  Element:= TsardElement.Create(nil);
  try
    Scanner.IntoElement := Element;
    Scanner.Scan(Lines);
  finally
    Element.Free;
    Scanner.Free;
  end;
  Result := True;
end;

end.

