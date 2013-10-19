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
  sardClasses, sardObjects, sardScanners;

type

  { TsardRun }

  TsardRun = class(TsardObject)
  protected
  public
    Main: TsoMain;
    Result: string;//Temp
    constructor Create;
    destructor Destroy; override;
    procedure Compile(Lines: TStrings);
    procedure Run;
  end;

implementation

{ TsardRun }

constructor TsardRun.Create;
begin

end;

destructor TsardRun.Destroy;
begin
  FreeAndNil(Main);
  inherited;
end;

procedure TsardRun.Compile(Lines: TStrings);
var
  Lexical: TsrdLexical;
  Feeder: TsrdFeeder;
  Parser: TsrdParser;
begin
  WriteLn('-------------------------------');
  FreeAndNil(Main);
  Main := TsoMain.Create;

  { Compile }
  Parser := TsrdParser.Create(Main.Block);
  Lexical := TsrdLexical.Create(Parser);
  Feeder := TsrdFeeder.Create(Lexical);

  Feeder.Scan(Lines);

  { End }
  FreeAndNil(Parser);
  FreeAndNil(Lexical);
  FreeAndNil(Feeder);

end;

procedure TsardRun.Run;
var
  Stack: TrunStack;
begin
  WriteLn('');
  WriteLn('-------------------------------');
  Stack := TrunStack.Create;
  Stack.Return.Push;
//  Stack.Local.Push;
  //Stack.Local.Current.Variables.SetValue('__ver__', TsoInteger.Create(101));
  //Main.AddClass('__ver__', nil);
  Main.Execute(Stack, nil);
  if Stack.Return.Current.Result.AnObject <> nil then
  begin
    Result := Stack.Return.Current.Result.AnObject.AsString;
    WriteLn('=== Result:  ' + Stack.Return.Current.Result.AnObject.AsString + '  ===');
  end;
  Stack.Return.Pop;
//  Stack.Local.Pop;
  FreeAndNil(Stack);
end;

end.
