unit sard;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey <zaher at parmaja dot com>
*}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  sardObjects, sardScanners;

function Build(Lines: TStrings): Boolean;
function Execute(Lines: TStrings; out vResult: string): Boolean;

implementation

{ TmyFeeder }

function Build(Lines: TStrings): Boolean;
var
  Lexical: TsrdLexical;
  Feeder: TsrdFeeder;
  Parser: TsrdParser;
  Main: TsoMain;
  Data: TrunData;
begin
  WriteLn('-------------------------------');

  Main := TsoMain.Create;
  Data:=TrunData.Create;
  { Compile }
  Parser := TsrdParser.Create(Data, Main.Block);
  Lexical := TsrdLexical.Create(Parser);
  Feeder := TsrdFeeder.Create(Lexical);

  Feeder.Scan(Lines);

  { End }
  FreeAndNil(Parser);
  FreeAndNil(Lexical);
  FreeAndNil(Feeder);
  FreeAndNil(Main);
  FreeAndNil(Data);

  Result := True;
end;


function Execute(Lines: TStrings; out vResult: string): Boolean;
var
  Lexical: TsrdLexical;
  Feeder: TsrdFeeder;
  Parser: TsrdParser;
  Stack: TrunStack;
  Data: TrunData;
  Main: TsoMain;
begin
  WriteLn('-------------------------------');

  Main := TsoMain.Create;
  Data := TrunData.Create;
  { Compile }
  Parser := TsrdParser.Create(Data, Main.Block);
  Lexical := TsrdLexical.Create(Parser);
  Feeder := TsrdFeeder.Create(Lexical);

  Feeder.Scan(Lines);
  WriteLn('');
  WriteLn('-------------------------------');
  { Run }
  Stack := TrunStack.Create;
  Stack.Push;
//  Stack.Local.Push;
  //Stack.Local.Current.Variables.SetValue('__ver__', TsoInteger.Create(101));
  //Main.AddClass('__ver__', nil);
  Main.Execute(Stack, nil);
  if Stack.Current.Result.AnObject <> nil then
  begin
    vResult := Stack.Current.Result.AnObject.AsString;
    WriteLn('=== Result:  ' + Stack.Current.Result.AnObject.AsString + '  ===');
  end;
  Stack.Pop;
//  Stack.Local.Pop;

  { End }
  FreeAndNil(Stack);
  FreeAndNil(Main);
  FreeAndNil(Data);
  FreeAndNil(Parser);
  FreeAndNil(Lexical);
  FreeAndNil(Feeder);

  Result := True;
end;

end.
