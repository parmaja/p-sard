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

  TmyParser = class(TsrdParser)
  protected
  public
    procedure TriggerToken(Token: String; TokenID: Integer); override;
  end;

  { TmyScript }

  TmyScript = class(TsrdScript)
  protected
  end;

function Execute(Lines: TStrings): Boolean;

implementation

{ TmyScript }

procedure TmyParser.TriggerToken(Token: String; TokenID: Integer);
begin
  inherited;
  //WriteLn(Token);
end;

function Execute(Lines: TStrings): Boolean;
var
  Scanner: TmyScript;
  Parser: TsrdParser;
  Block: TsrdScope;
  Run: TsrdRun;
begin
  //sardEngine.Run(Lines);
  WriteLn('-------------------------------');
  Scanner := TmyScript.Create;
  try
    Block := TsrdScope.Create(nil);
    Block.New;
    Parser := TmyParser.Create(Block);
    Scanner.Scanners.Parser := Parser;
    Scanner.Scan(Lines);
    Scanner.Scanners.Parser := nil;

    Run := TsrdRun.Create;
    Run.Execute(Block);

    FreeAndNil(Parser);
    FreeAndNil(Block);
  finally
    FreeAndNil(Scanner);
  end;
  Result := True;
end;

end.
