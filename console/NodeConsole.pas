unit NodeConsole;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey
 *}

{$ifdef fpc}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$else }
{$define delphi}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  mnUtils,
  sardClasses, sardObjects, sardParsers, sardScripts, sardJSONs;

type

  { TSardConsole }

  TSardConsole = class(TObject)
  protected
    FParams: TStringList;
    Verbose: Boolean;
  public
    {$ifdef delphi}
    function Location: string;
    {$endif}
    procedure Run;
    constructor Create;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function WaitKey: Boolean;
  end;

implementation

{ TSardApplication }

constructor TSardConsole.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
  ParseCommandArguments(FParams);
end;

destructor TSardConsole.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

{$ifdef delphi}
function TSardConsole.Location: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;
{$endif}

function TSardConsole.WaitKey: Boolean;
begin
  Result := GetArgumentSwitch(FParams, '-w', '-wait');
end;

procedure TSardConsole.WriteHelp;
begin
  writeln('Usage: sard -h');
end;


procedure TSardConsole.Run;
var
  Script: TCodeScript;
begin
  Script := TCodeScript.Create;
  try
    try
      Script.RegisterInternals := False;
      Script.Init;
      {
        x:=10+10;
        y:=11+11;
        print(x+y);
      }

      with Script.Main do
      begin
        with Statements.Add do
        begin
          Add(TAssign_Node.Create('x'));
          Add(TInteger_Node.Create(10));
          Add(TOpPlus_Node.Create);
          Add(TInteger_Node.Create(15));
        end;

        with Statements.Add do
        begin
          Add(TAssign_Node.Create(''));
          Add(TInstance_Node.Create('x'));
        end;
      end;

      Script.ExportToConsole;
      Script.Run;
      WriteLn('------------------');
      WriteLn(Script.Main.Value.AsText);
    finally
      FreeAndNil(Script);
    end;
  except
    on E:Exception do
    begin
      WriteLn(E.Message);
//      raise;
    end;
  end;
end;

end.
