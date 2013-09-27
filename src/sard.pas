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

function Execute(Lines: TStrings): Boolean;

implementation

{ TmyFeeder }

procedure TmyParser.TriggerToken(Token: String; TokenID: Integer);
begin
  inherited;
  //WriteLn(Token);
end;

function Execute(Lines: TStrings): Boolean;
var
  Scanners: TsrdScanners;
  Feeder: TsrdFeeder;
  Parser: TsrdParser;
  Stack: TrunStack;
  Main: TsoMain;
begin
  WriteLn('-------------------------------');

  Main := TsoMain.Create;

  Parser := TmyParser.Create(Main.Items);
  Scanners := TsrdScanners.Create(Parser);
  Feeder := TsrdFeeder.Create(Scanners);

  Feeder.Scan(Lines);

  Stack := TrunStack.Create;
  Stack.New;
  Main.Execute(Stack);

  FreeAndNil(Stack);
  FreeAndNil(Main);
  FreeAndNil(Parser);
  FreeAndNil(Scanners);
  FreeAndNil(Feeder);

  Result := True;
end;

end.
