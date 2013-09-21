unit sard;
{**
*  This file is part of the "SARD"
*
* @license   Apache License Version 2.0
*            included in this distribution
* @author    Zaher Dirkey <zaher at parmaja dot com>
*}


{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  sardObjects, sardScripts;

type

  { TmyScanner }

  TmyParser = class(TsardScriptParser)
  protected
  public
    procedure TriggerToken(Token: String; TokenID: Integer); override;
  end;

  { TmyScript }

  TmyScript = class(TsardScript)
  protected
  end;

function Execute(Lines: TStrings): Boolean;
procedure Test;

implementation

{ TmyScript }

procedure TmyParser.TriggerToken(Token: String; TokenID: Integer);
begin
  inherited;
  WriteLn(Token);
end;

function Execute(Lines: TStrings): Boolean;
var
  Scanner: TmyScript;
  Parser: TsardScriptParser;
  Block: TsrdoBlock;
  Run: TsrdoRun;
begin
  Scanner := TmyScript.Create;
  try
    Block := TsrdoBlock.Create;
    Block.New;
    Parser := TmyParser.Create(Block);
    Scanner.Scanners.Parser := Parser;
    Scanner.Scan(Lines);
    Scanner.Scanners.Parser := nil;

    Run := TsrdoRun.Create;
    Run.Execute(Block);

    FreeAndNil(Parser);
    FreeAndNil(Block);
  finally
    FreeAndNil(Scanner);
  end;
  Result := True;
end;

procedure Test;
begin
end;

end.
