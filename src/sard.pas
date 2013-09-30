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
  sardObjects, sardScanners;

function Build(Lines: TStrings): Boolean;
function Execute(Lines: TStrings): Boolean;

implementation

{ TmyFeeder }

function Build(Lines: TStrings): Boolean;
var
  Scanners: TsrdScanners;
  Feeder: TsrdFeeder;
  Parser: TsrdParser;
  Main: TsoMain;
  Data: TrunData;
begin
  WriteLn('-------------------------------');

  Main := TsoMain.Create;
  Data:=TrunData.Create;
  { Compile }
  Parser := TsrdParser.Create(Data, Main.Items);
  Scanners := TsrdScanners.Create(Parser);
  Feeder := TsrdFeeder.Create(Scanners);

  Feeder.Scan(Lines);

  { End }
  FreeAndNil(Parser);
  FreeAndNil(Scanners);
  FreeAndNil(Feeder);
  FreeAndNil(Main);
  FreeAndNil(Data);

  Result := True;
end;


function Execute(Lines: TStrings): Boolean;
var
  Scanners: TsrdScanners;
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
  Parser := TsrdParser.Create(Data, Main.Items);
  Scanners := TsrdScanners.Create(Parser);
  Feeder := TsrdFeeder.Create(Scanners);

  Feeder.Scan(Lines);
  WriteLn('');
  WriteLn('-------------------------------');
  { Run }
  Stack := TrunStack.Create;
  Stack.Push;
  //Stack.Current.Scope.Variables.Register('__ver__'); //just for test
  //Stack.Current.Scope.Variables.SetValue('__ver__', TsoInteger.Create(100));
  Main.Execute(Stack, nil);
  if Stack.Current.Result.AnObject <> nil then
    WriteLn('=== Result:  ' + Stack.Current.Result.AnObject.AsString + '  ===');
  Stack.Pop;

  { End }
  FreeAndNil(Stack);
  FreeAndNil(Main);
  FreeAndNil(Data);
  FreeAndNil(Parser);
  FreeAndNil(Scanners);
  FreeAndNil(Feeder);

  Result := True;
end;

end.
